#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <functional>
#include "Logics.h"
// Asegúrate de que Logics.h está en src/ y accesible.
// Este archivo contiene get_tnorm, get_implication y los typedefs.

using namespace Rcpp;

// --- Estructuras ---

struct Factor {
  NumericVector extent;
  NumericVector intent;
};

// --- Auxiliares para Conexiones de Galois ---

// Definimos un tipo para la función de flecha (arrow)
// Input: Vector fuente (Source), Matriz I, Dimensiones, Operadores
// Output: Vector resultado (Target)
typedef std::function<NumericVector(const NumericVector&, const NumericMatrix&, LogicOperator, LogicOperator)> ArrowFunction;

// 1. Standard (Conceptos Formales Clásicos)
// Up: Min(Extent -> I)
NumericVector arrow_up_standard(const NumericVector& extent, const NumericMatrix& I, LogicOperator tnorm, LogicOperator implication) {
  int n = I.nrow();
  int m = I.ncol();
  NumericVector intent(m);
  for(int j=0; j<m; ++j) {
    double min_val = 1.0;
    for(int i=0; i<n; ++i) {
      double val = implication(extent[i], I(i, j));
      if (val < min_val) min_val = val;
    }
    intent[j] = min_val;
  }
  return intent;
}

// Down: Min(Intent -> I)
NumericVector arrow_down_standard(const NumericVector& intent, const NumericMatrix& I, LogicOperator tnorm, LogicOperator implication) {
  int n = I.nrow();
  int m = I.ncol();
  NumericVector extent(n);
  for(int i=0; i<n; ++i) {
    double min_val = 1.0;
    for(int j=0; j<m; ++j) {
      double val = implication(intent[j], I(i, j));
      if (val < min_val) min_val = val;
    }
    extent[i] = min_val;
  }
  return extent;
}

// 2. Benevolent 1 (A menudo usado en Rough Sets / Posibilidad)
// Up (UpRight): Max(Tnorm(Extent, I))
NumericVector arrow_up_benevolent1(const NumericVector& extent, const NumericMatrix& I, LogicOperator tnorm, LogicOperator implication) {
  int n = I.nrow();
  int m = I.ncol();
  NumericVector intent(m);
  for(int j=0; j<m; ++j) {
    double max_val = 0.0;
    for(int i=0; i<n; ++i) {
      double val = tnorm(extent[i], I(i, j));
      if (val > max_val) max_val = val;
    }
    intent[j] = max_val;
  }
  return intent;
}

// Down (DownLeft): Min(Imp(I, Intent))  <-- Ojo: I implica Intent
NumericVector arrow_down_benevolent1(const NumericVector& intent, const NumericMatrix& I, LogicOperator tnorm, LogicOperator implication) {
  int n = I.nrow();
  int m = I.ncol();
  NumericVector extent(n);
  for(int i=0; i<n; ++i) {
    double min_val = 1.0;
    for(int j=0; j<m; ++j) {
      double val = implication(I(i, j), intent[j]);
      if (val < min_val) min_val = val;
    }
    extent[i] = min_val;
  }
  return extent;
}

// --- Funciones Principales ---

// Reconstrucción usando Sup-Tnorm product (Estándar para factorización)
NumericMatrix reconstruct_matrix(const std::vector<Factor>& factors, int n_rows, int n_cols, LogicOperator tnorm) {
  NumericMatrix R(n_rows, n_cols);
  std::fill(R.begin(), R.end(), 0.0);

  if (factors.empty()) return R;

  for (int i = 0; i < n_rows; ++i) {
    for (int j = 0; j < n_cols; ++j) {
      double val = 0.0;
      for (const auto& f : factors) {
        double prod = tnorm(f.extent[i], f.intent[j]);
        if (prod > val) val = prod;
      }
      R(i, j) = val;
    }
  }
  return R;
}

// Calcular Ganancia (Logic-aware)
double calculate_gain(const NumericMatrix& I, const NumericMatrix& BaseRec,
                      const NumericVector& C, const NumericVector& D_curr,
                      int j_cand, double a_cand, double w,
                      LogicOperator tnorm) {

  double gain = 0.0;
  int n = I.nrow();

  for(int i=0; i<n; ++i) {
    double I_ij = I(i, j_cand);
    double base_val = BaseRec(i, j_cand);

    double factor_val_curr = tnorm(C[i], D_curr[j_cand]);
    double cur_ij = std::max(base_val, factor_val_curr);

    double factor_val_new = tnorm(C[i], a_cand);
    double new_ij = std::max(base_val, factor_val_new);

    if (new_ij <= I_ij) {
      gain += (new_ij - cur_ij);
    } else if (cur_ij < I_ij && I_ij < new_ij) {
      gain += (I_ij - cur_ij) - w * (new_ij - I_ij);
    } else if (I_ij <= cur_ij) {
      gain += -w * (new_ij - cur_ij);
    }
  }
  return gain;
}

// Expansión (Logic-aware)
Factor expansion(Factor nucleus, const NumericMatrix& I, const std::vector<Factor>& F_set, double w, LogicOperator tnorm) {
  Factor candidate = nucleus;
  int n_attrs = I.ncol();
  NumericMatrix BaseRec = reconstruct_matrix(F_set, I.nrow(), I.ncol(), tnorm);

  std::vector<double> L = {0.0, 1.0};
  for(auto val : I) L.push_back(val);
  std::sort(L.begin(), L.end());
  L.erase(std::unique(L.begin(), L.end()), L.end());

  bool change = true;
  while(change) {
    change = false;
    double max_gain = 0.0;
    int best_j = -1;
    double best_a = -1.0;

    for(int j=0; j<n_attrs; ++j) {
      double current_val = candidate.intent[j];
      for(double a : L) {
        if (a <= current_val) continue;
        double g = calculate_gain(I, BaseRec, candidate.extent, candidate.intent, j, a, w, tnorm);
        if (g > max_gain) {
          max_gain = g;
          best_j = j;
          best_a = a;
        }
      }
    }

    if (max_gain > 1e-9) {
      candidate.intent[best_j] = best_a;
      change = true;
    }
  }
  return candidate;
}

// [[Rcpp::export]]
List grecond_plus(NumericMatrix I, double w = 1.0, double stop_threshold_ratio = 0.0,
                  String connection = "standard", String logic_name = "Lukasiewicz") {

  // 1. Configurar Lógica
  LogicOperator tnorm = get_tnorm(logic_name);
  LogicOperator implication = get_implication(logic_name);

  // Lambda para Bi-residuum: min(a->b, b->a)
  auto biresiduum = [&](double a, double b) {
    return std::min(implication(a, b), implication(b, a));
  };

  // 2. Configurar Conexión (Flechas)
  ArrowFunction arrow_up;
  ArrowFunction arrow_down;

  if (connection == "standard") {
    arrow_up = arrow_up_standard;
    arrow_down = arrow_down_standard;
  } else if (connection == "benevolent1") {
    arrow_up = arrow_up_benevolent1;
    arrow_down = arrow_down_benevolent1;
  } else {
    // Fallback a standard o lanzar error
    arrow_up = arrow_up_standard;
    arrow_down = arrow_down_standard;
    Rcpp::warning("Connection not explicitly supported in GreConD+, defaulting to 'standard'.");
  }

  int n_rows = I.nrow();
  int n_cols = I.ncol();
  std::vector<Factor> F;

  std::vector<double> L = {0.0, 1.0};
  for(auto val : I) L.push_back(val);
  std::sort(L.begin(), L.end());
  L.erase(std::unique(L.begin(), L.end()), L.end());

  bool u_not_empty = true;
  NumericMatrix CurrentRec(n_rows, n_cols);
  std::fill(CurrentRec.begin(), CurrentRec.end(), 0.0);

  while (u_not_empty) {

    // --- Paso 1: Encontrar Núcleo (Usando Connection seleccionada) ---
    NumericVector D(n_cols, 0.0);
    NumericVector C(n_rows);

    double V = -1.0;
    bool improved = true;

    while(improved) {
      improved = false;
      int best_j = -1;
      double best_a = -1.0;
      double max_covered = -1.0;

      for(int j=0; j<n_cols; ++j) {
        for(double a : L) {
          if (a <= D[j]) continue;
          if (a == 0) continue;

          NumericVector D_temp = clone(D);
          D_temp[j] = a;

          // Calcular concepto formal (o equivalente según conexión)
          // 1. C = D^down
          NumericVector C_temp = arrow_down(D_temp, I, tnorm, implication);
          // 2. D_closed = C^up
          NumericVector D_closed = arrow_up(C_temp, I, tnorm, implication);

          // Calcular cobertura de residuos
          double coverage_gain = 0.0;
          for(int r=0; r<n_rows; ++r) {
            for(int c=0; c<n_cols; ++c) {
              if (I(r, c) > CurrentRec(r, c)) {
                // El producto A o B siempre usa T-norma para la factorización
                double factor_val = tnorm(C_temp[r], D_closed[c]);
                double new_rec = std::max(CurrentRec(r, c), factor_val);

                double effective_new = std::min(new_rec, I(r, c));
                double effective_old = std::min(CurrentRec(r, c), I(r, c));

                coverage_gain += (effective_new - effective_old);
              }
            }
          }

          if (coverage_gain > V && coverage_gain > max_covered) {
            max_covered = coverage_gain;
            best_j = j;
            best_a = a;
          }
        }
      }

      if (max_covered > V + 1e-9) {
        V = max_covered;
        NumericVector D_temp = clone(D);
        D_temp[best_j] = best_a;
        NumericVector C_temp = arrow_down(D_temp, I, tnorm, implication);
        D = arrow_up(C_temp, I, tnorm, implication);
        improved = true;
      }
    }

    C = arrow_down(D, I, tnorm, implication);
    Factor nucleus;
    nucleus.extent = C;
    nucleus.intent = D;

    // --- Paso 2: Expansión (Siempre usa Tnorm para consistencia con factorización) ---
    Factor extended_factor = expansion(nucleus, I, F, w, tnorm);
    F.push_back(extended_factor);

    CurrentRec = reconstruct_matrix(F, n_rows, n_cols, tnorm);

    // --- Paso 3: Refinamiento ---
    // Calcular error total: sum(1 - (I <-> Rec))
    auto calc_total_error = [&](const NumericMatrix& Rec) {
      double error = 0.0;
      for(int i=0; i<n_rows; ++i)
        for(int j=0; j<n_cols; ++j)
          error += (1.0 - biresiduum(I(i,j), Rec(i,j)));
      return error;
    };

    double current_total_error = calc_total_error(CurrentRec);

    for (int k = F.size() - 1; k >= 0; --k) {
      std::vector<Factor> F_temp = F;
      F_temp.erase(F_temp.begin() + k);
      NumericMatrix Rec_temp = reconstruct_matrix(F_temp, n_rows, n_cols, tnorm);
      double error_temp = calc_total_error(Rec_temp);

      if (error_temp <= current_total_error) {
        F = F_temp;
        CurrentRec = Rec_temp;
        current_total_error = error_temp;
      } else {
        // Reducción al núcleo (usando arrow_up para recuperar el núcleo teórico del extent actual)
        NumericVector D_nucleus = arrow_up(F[k].extent, I, tnorm, implication);
        bool modified = false;
        for(int j=0; j<n_cols; ++j) {
          if (F[k].intent[j] > D_nucleus[j]) {
            double original_val = F[k].intent[j];
            F[k].intent[j] = D_nucleus[j];
            NumericMatrix Rec_check = reconstruct_matrix(F, n_rows, n_cols, tnorm);
            double error_check = calc_total_error(Rec_check);

            if (error_check <= current_total_error) {
              current_total_error = error_check;
              CurrentRec = Rec_check;
              modified = true;
            } else {
              F[k].intent[j] = original_val;
            }
          }
        }
      }
    }

    // Condición de parada
    double e_u = 0.0;
    double total_mass = 0.0;
    for(int i=0; i<n_rows; ++i)
      for(int j=0; j<n_cols; ++j) {
        total_mass += I(i,j);
        if (I(i,j) > CurrentRec(i,j)) {
          e_u += (I(i,j) - CurrentRec(i,j));
        }
      }

      if (e_u < 1e-9 || (stop_threshold_ratio > 0 && (e_u / total_mass) < stop_threshold_ratio)) {
        u_not_empty = false;
      }
  }

  List result;
  for(const auto& f : F) {
    result.push_back(List::create(Named("extent") = f.extent, Named("intent") = f.intent));
  }
  return result;
}
