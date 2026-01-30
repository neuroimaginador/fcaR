#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <functional>
#include "Logics.h" // Asegúrate de que este archivo existe y tiene get_tnorm/get_implication

using namespace Rcpp;

// --- Estructuras de Datos ---

struct Factor {
  NumericVector extent;
  NumericVector intent;
};

// --- Helpers de Reconstrucción ---

// Reconstrucción de Matriz: R_ij = sup_k (A_ik T-Norm B_kj)
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

// --- Operadores de Derivación (Flechas) ---

// Flecha Arriba (Extent -> Intent)
NumericVector arrow_up(const NumericVector& extent, const NumericMatrix& I, LogicOperator implication) {
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

// Flecha Abajo (Intent -> Extent)
NumericVector arrow_down(const NumericVector& intent, const NumericMatrix& I, LogicOperator implication) {
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

// --- Lógica GreConD+ Corregida ---

// Calcula la ganancia exacta de un concepto candidato (extent, intent)
double calculate_gain_concept(const NumericMatrix& I, const NumericMatrix& BaseRec,
                              const NumericVector& cand_extent, const NumericVector& cand_intent,
                              double w, LogicOperator tnorm) {
  double gain = 0.0;
  int n = I.nrow();
  int m = I.ncol();

  // Solo iteramos sobre las celdas afectadas por el nuevo intent (donde cand_intent > 0)
  // Optimización: recorrer solo atributos > 0 si fuera sparse, aquí recorremos todo.
  for(int j=0; j<m; ++j) {
    if (cand_intent[j] == 0) continue;

    for(int i=0; i<n; ++i) {
      // Si el objeto no está en el extent, no aporta nada
      if (cand_extent[i] == 0) continue;

      double I_ij = I(i, j);
      double base_val = BaseRec(i, j); // Lo que ya cubrían los factores anteriores

      // Valor que aporta este nuevo factor candidato
      double factor_val = tnorm(cand_extent[i], cand_intent[j]);

      // Nuevo valor reconstruido
      double new_rec = std::max(base_val, factor_val);

      // Ganancia = (Lo que cubrimos de nuevo) - w * (Lo que nos pasamos/sobre-cubrimos)

      if (new_rec <= I_ij) {
        // Caso ideal: estamos llenando hueco sin pasarnos
        gain += (new_rec - base_val);
      } else {
        // Caso nos pasamos (overcovering) o ya estaba cubierto
        // Parte útil (hasta I_ij)
        double useful = std::max(0.0, I_ij - base_val);
        // Parte mala (exceso sobre I_ij)
        double excess = new_rec - std::max(base_val, I_ij);

        gain += useful - (w * excess);
      }
    }
  }
  return gain;
}

// Fase de Expansión Corregida: Actualiza Extent dinámicamente
Factor expansion_corrected(Factor nucleus, const NumericMatrix& I, const NumericMatrix& BaseRec,
                           double w, LogicOperator tnorm, LogicOperator implication) {

  Factor candidate = nucleus;
  int n_rows = I.nrow();
  int n_cols = I.ncol();

  // Valores de verdad posibles (grados)
  std::vector<double> L = {0.0, 1.0};
  for(double val : I) L.push_back(val);
  std::sort(L.begin(), L.end());
  L.erase(std::unique(L.begin(), L.end()), L.end());

  bool improved = true;
  while(improved) {
    improved = false;
    double max_gain = 0.0; // Necesitamos ganancia positiva estricta para seguir
    int best_j = -1;
    double best_a = -1.0;

    // Calcular ganancia base del candidato actual para ver si mejoramos
    double current_gain = calculate_gain_concept(I, BaseRec, candidate.extent, candidate.intent, w, tnorm);

    // Intentar refinar añadiendo/subiendo un atributo
    for(int j=0; j<n_cols; ++j) {
      double current_a = candidate.intent[j];

      for(double a : L) {
        if (a <= current_a) continue; // Solo buscamos aumentar grado o añadir atributo

        // 1. Crear Intent temporal
        NumericVector temp_intent = clone(candidate.intent);
        temp_intent[j] = a;

        // 2. ACTUALIZACIÓN INCREMENTAL DEL EXTENT (CLAVE)
        // A_new(g) = A_old(g) AND (a -> I(g, j))
        // Esto mantiene la propiedad de concepto formal: A = B down
        NumericVector temp_extent(n_rows);
        bool empty_extent = true;

        for(int i=0; i<n_rows; ++i) {
          double imp_val = implication(a, I(i, j)); // a -> I_ij
          // Intersección (Min) con el extent anterior
          temp_extent[i] = std::min(candidate.extent[i], imp_val);
          if(temp_extent[i] > 0) empty_extent = false;
        }

        if (empty_extent) continue;

        // 3. Calcular ganancia con el par (temp_extent, temp_intent) CONSISTENTE
        double g = calculate_gain_concept(I, BaseRec, temp_extent, temp_intent, w, tnorm);

        // Buscamos el que maximice la ganancia respecto al estado actual
        if (g > current_gain && g > max_gain) {
          max_gain = g;
          best_j = j;
          best_a = a;
        }
      }
    }

    if (best_j != -1) {
      // Aplicar el mejor cambio
      candidate.intent[best_j] = best_a;
      // Actualizar extent permanentemente
      for(int i=0; i<n_rows; ++i) {
        double imp_val = implication(best_a, I(i, best_j));
        candidate.extent[i] = std::min(candidate.extent[i], imp_val);
      }
      improved = true;
    }
  }
  return candidate;
}

// [[Rcpp::export]]
List grecond_plus_cpp(NumericMatrix I, double w = 1.0, double stop_threshold_ratio = 0.0, String logic_name = "Lukasiewicz") {

  LogicOperator tnorm = get_tnorm(logic_name);
  LogicOperator implication = get_implication(logic_name);

  if (tnorm == NULL || implication == NULL) stop("Invalid logic name.");

  int n_rows = I.nrow();
  int n_cols = I.ncol();
  std::vector<Factor> F;

  NumericMatrix CurrentRec(n_rows, n_cols);
  std::fill(CurrentRec.begin(), CurrentRec.end(), 0.0);

  // Valores de verdad
  std::vector<double> L = {0.0, 1.0};
  for(double val : I) L.push_back(val);
  std::sort(L.begin(), L.end());
  L.erase(std::unique(L.begin(), L.end()), L.end());

  bool u_not_empty = true;

  while (u_not_empty) {
    // 1. Encontrar Núcleo (Mejor columna simple)
    NumericVector D_seed(n_cols, 0.0);
    double best_col_gain = -1.0;
    int best_j = -1;
    double best_a = -1.0;

    // Buscar la columna 'j' con valor 'a' que, por sí sola (cerrada), cubra más
    for(int j=0; j<n_cols; ++j) {
      for(double a : L) {
        if (a == 0) continue;

        // Crear concepto semilla
        NumericVector D_temp(n_cols, 0.0);
        D_temp[j] = a;
        NumericVector C_temp = arrow_down(D_temp, I, implication);
        // Cerrar semilla (opcional, pero recomendado en GreConD)
        NumericVector D_closed = arrow_up(C_temp, I, implication);

        double g = calculate_gain_concept(I, CurrentRec, C_temp, D_closed, w, tnorm);
        if (g > best_col_gain) {
          best_col_gain = g;
          best_j = j;
          best_a = a;
        }
      }
    }

    if (best_col_gain <= 1e-9) break; // No se puede cubrir nada más

    NumericVector D_nucleus(n_cols, 0.0);
    D_nucleus[best_j] = best_a;
    NumericVector C_nucleus = arrow_down(D_nucleus, I, implication);
    // Asegurar cierre inicial
    D_nucleus = arrow_up(C_nucleus, I, implication);

    Factor nucleus;
    nucleus.extent = C_nucleus;
    nucleus.intent = D_nucleus;

    // 2. Expansión (Optimizada)
    Factor new_factor = expansion_corrected(nucleus, I, CurrentRec, w, tnorm, implication);
    F.push_back(new_factor);

    // 3. Actualizar Residuo
    CurrentRec = reconstruct_matrix(F, n_rows, n_cols, tnorm);

    // Check parada
    double e_u = 0.0;
    double total_mass = 0.0;
    for(int i=0; i<n_rows; ++i) {
      for(int j=0; j<n_cols; ++j) {
        total_mass += I(i,j);
        if (I(i,j) > CurrentRec(i,j)) e_u += (I(i,j) - CurrentRec(i,j));
      }
    }

    if (total_mass == 0 || (e_u / total_mass) <= stop_threshold_ratio || e_u < 1e-9) {
      u_not_empty = false;
    }
  }

  List result;
  for(const auto& f : F) {
    result.push_back(List::create(Named("extent") = f.extent, Named("intent") = f.intent));
  }
  return result;
}

// --- ALGORITMO 2: ASSO (Binary-Consistent) ---

// [[Rcpp::export]]
List asso_cpp(NumericMatrix I, double threshold = 0.7, double w_pos = 1.0, double w_neg = 1.0) {
  int n = I.nrow();
  int m = I.ncol();

  // 1. Matriz de Asociación (Confianza de reglas de asociación)
  // A_jk = Confianza(k -> j) = Supp(k,j) / Supp(k)
  NumericMatrix A(m, m);
  for(int j=0; j<m; ++j) {
    for(int k=0; k<m; ++k) {
      double supp_k = 0;
      double supp_jk = 0;

      for(int i=0; i<n; ++i) {
        // Tratamos fuzzy > 0 como presencia para la estructura base de ASSO
        bool has_k = (I(i, k) > 0);
        bool has_j = (I(i, j) > 0);

        if (has_k) {
          supp_k++;
          if (has_j) supp_jk++;
        }
      }

      if (j == k) A(j, k) = 1.0;
      else A(j, k) = (supp_k > 0) ? (supp_jk / supp_k) : 0.0;
    }
  }

  // 2. Generar Candidatos (Intents basados en umbral)
  std::vector<NumericVector> candidates;
  // Cada columna k de A genera un candidato
  for(int k=0; k<m; ++k) {
    NumericVector cand_intent(m);
    bool valid = false;
    for(int j=0; j<m; ++j) {
      // Si la confianza k->j supera el umbral, j es parte del patrón k
      if (A(j, k) >= threshold) {
        cand_intent[j] = 1.0;
        valid = true;
      } else {
        cand_intent[j] = 0.0;
      }
    }
    if(valid) candidates.push_back(cand_intent);
  }

  // 3. Selección Greedy
  std::vector<Factor> factors;
  NumericMatrix Covered(n, m); // Matriz de lo que ya hemos explicado
  std::fill(Covered.begin(), Covered.end(), 0.0);

  bool improving = true;
  while(improving && !candidates.empty()) {
    improving = false;
    double best_score = -1e9;
    int best_idx = -1;
    NumericVector best_extent(n);

    // Evaluar cada candidato contra el residuo actual
    for(size_t k=0; k<candidates.size(); ++k) {
      NumericVector intent = candidates[k];
      NumericVector extent(n);

      // Calcular extent óptimo para este intent (Objetos que lo cumplen)
      // En ASSO binario: objeto tiene todos los atributos del intent
      for(int i=0; i<n; ++i) {
        bool fits = true;
        for(int j=0; j<m; ++j) {
          if (intent[j] > 0 && I(i, j) == 0) {
            fits = false; break;
          }
        }
        extent[i] = fits ? 1.0 : 0.0;
      }

      // Calcular Score: (Peso cubierto nuevo) - (Peso error)
      double score = 0.0;
      bool covers_something = false;

      for(int i=0; i<n; ++i) {
        if (extent[i] == 0) continue;
        for(int j=0; j<m; ++j) {
          if (intent[j] == 0) continue;

          // Si cubrimos una celda que es 1 y no estaba cubierta
          if (I(i, j) > 0) {
            if (Covered(i, j) == 0) {
              score += w_pos;
              covers_something = true;
            }
            // Si ya estaba cubierta, gain 0 (o penalización leve opcional)
          } else {
            // Estamos cubriendo un 0 (Error)
            score -= w_neg;
          }
        }
      }

      if (covers_something && score > best_score) {
        best_score = score;
        best_idx = k;
        best_extent = extent;
      }
    }

    if (best_idx != -1 && best_score > 0) {
      Factor f;
      f.extent = best_extent;
      f.intent = candidates[best_idx];
      factors.push_back(f);

      // Actualizar matriz de cubiertos
      for(int i=0; i<n; ++i) {
        if (f.extent[i] > 0) {
          for(int j=0; j<m; ++j) {
            if (f.intent[j] > 0) Covered(i, j) = 1.0;
          }
        }
      }

      // Eliminar el candidato usado para no repetirlo
      candidates.erase(candidates.begin() + best_idx);
      improving = true;
    }
  }

  List result;
  for(const auto& f : factors) {
    result.push_back(List::create(Named("extent") = f.extent, Named("intent") = f.intent));
  }
  return result;
}
// #include <Rcpp.h>
// #include <vector>
// #include <cmath>
// #include <algorithm>
// #include <functional>
// #include "Logics.h" // Integración con tu sistema de Lógica Difusa
//
// using namespace Rcpp;
//
// // --- Estructuras de Datos ---
//
// struct Factor {
//   NumericVector extent;
//   NumericVector intent;
// };
//
// // --- Helpers de Álgebra Difusa ---
//
// // Reconstrucción de Matriz: R_ij = sup_k (A_ik T-Norm B_kj)
// NumericMatrix reconstruct_matrix(const std::vector<Factor>& factors, int n_rows, int n_cols, LogicOperator tnorm) {
//   NumericMatrix R(n_rows, n_cols);
//   std::fill(R.begin(), R.end(), 0.0);
//
//   if (factors.empty()) return R;
//
//   for (int i = 0; i < n_rows; ++i) {
//     for (int j = 0; j < n_cols; ++j) {
//       double val = 0.0;
//       for (const auto& f : factors) {
//         // Usamos la T-Norma seleccionada (Zadeh, Lukasiewicz, etc.)
//         double prod = tnorm(f.extent[i], f.intent[j]);
//         if (prod > val) val = prod; // Supremo (Max)
//       }
//       R(i, j) = val;
//     }
//   }
//   return R;
// }
//
// // --- Operadores de Derivación (Flechas) ---
//
// // Flecha Arriba (Extent -> Intent): B(m) = inf_g (A(g) -> I(g,m))
// NumericVector arrow_up(const NumericVector& extent, const NumericMatrix& I, LogicOperator implication) {
//   int n = I.nrow();
//   int m = I.ncol();
//   NumericVector intent(m);
//
//   for(int j=0; j<m; ++j) {
//     double min_val = 1.0;
//     for(int i=0; i<n; ++i) {
//       // Usamos la Implicación Residuo correspondiente
//       double val = implication(extent[i], I(i, j));
//       if (val < min_val) min_val = val;
//     }
//     intent[j] = min_val;
//   }
//   return intent;
// }
//
// // Flecha Abajo (Intent -> Extent): A(g) = inf_m (B(m) -> I(g,m))
// NumericVector arrow_down(const NumericVector& intent, const NumericMatrix& I, LogicOperator implication) {
//   int n = I.nrow();
//   int m = I.ncol();
//   NumericVector extent(n);
//
//   for(int i=0; i<n; ++i) {
//     double min_val = 1.0;
//     for(int j=0; j<m; ++j) {
//       // Usamos la Implicación Residuo correspondiente
//       double val = implication(intent[j], I(i, j));
//       if (val < min_val) min_val = val;
//     }
//     extent[i] = min_val;
//   }
//   return extent;
// }
//
// // Cálculo de Ganancia para GreConD+ (Logic-Aware)
// double calculate_gain(const NumericMatrix& I, const NumericMatrix& BaseRec,
//                       const NumericVector& C, const NumericVector& D_curr,
//                       int j_cand, double a_cand, double w,
//                       LogicOperator tnorm) {
//
//   double gain = 0.0;
//   int n = I.nrow();
//
//   for(int i=0; i<n; ++i) {
//     double I_ij = I(i, j_cand);
//     double base_val = BaseRec(i, j_cand);
//
//     // Valor con el factor actual (usando T-Norma)
//     double factor_val_curr = tnorm(C[i], D_curr[j_cand]);
//     double cur_ij = std::max(base_val, factor_val_curr);
//
//     // Valor con la nueva propuesta (usando T-Norma)
//     double factor_val_new = tnorm(C[i], a_cand);
//     double new_ij = std::max(base_val, factor_val_new);
//
//     if (new_ij <= I_ij) {
//       gain += (new_ij - cur_ij); // Ganancia pura (cubrir lo que falta)
//     } else if (cur_ij < I_ij && I_ij < new_ij) {
//       gain += (I_ij - cur_ij) - w * (new_ij - I_ij); // Ganancia parcial con penalización
//     } else if (I_ij <= cur_ij) {
//       gain += -w * (new_ij - cur_ij); // Solo penalización (sobre-cobertura)
//     }
//   }
//   return gain;
// }
//
// // Fase de Expansión de GreConD+
// Factor expansion(Factor nucleus, const NumericMatrix& I, const std::vector<Factor>& F_set,
//                  double w, LogicOperator tnorm, LogicOperator implication) {
//
//   Factor candidate = nucleus;
//   int n_attrs = I.ncol();
//
//   // Reconstruimos lo que ya tenemos cubierto con los factores anteriores
//   NumericMatrix BaseRec = reconstruct_matrix(F_set, I.nrow(), I.ncol(), tnorm);
//
//   // Valores únicos presentes en la matriz (grados de verdad relevantes)
//   std::vector<double> L = {0.0, 1.0};
//   for(auto val : I) L.push_back(val);
//   std::sort(L.begin(), L.end());
//   L.erase(std::unique(L.begin(), L.end()), L.end());
//
//   bool change = true;
//   while(change) {
//     change = false;
//     double max_gain = 0.0;
//     int best_j = -1;
//     double best_a = -1.0;
//
//     // Buscamos el mejor atributo y grado para añadir al concepto
//     for(int j=0; j<n_attrs; ++j) {
//       double current_val = candidate.intent[j];
//       for(double a : L) {
//         if (a <= current_val) continue;
//
//         NumericVector proposed_intent = clone(candidate.intent);
//         proposed_intent[j] = a;
//
//         // Recalcular Extent para mantener la propiedad de Concepto
//         NumericVector proposed_extent = arrow_down(proposed_intent, I, implication);
//
//         // Calcular ganancia con el PAR (extent, intent) consistente
//         double g = calculate_gain(I, BaseRec, proposed_extent, proposed_intent, j, a, w, tnorm);
//
//         // double g = calculate_gain(I, BaseRec, candidate.extent, candidate.intent, j, a, w, tnorm);
//
//         if (g > max_gain) {
//           max_gain = g;
//           best_j = j;
//           best_a = a;
//         }
//       }
//     }
//
//     if (max_gain > 1e-9) {
//       candidate.intent[best_j] = best_a;
//       change = true;
//     }
//   }
//   return candidate;
// }
//
// // --- ALGORITMO PRINCIPAL: GreConD+ ---
//
// // [[Rcpp::export]]
// List grecond_plus_cpp(NumericMatrix I, double w = 1.0, double stop_threshold_ratio = 0.0, String logic_name = "Lukasiewicz") {
//
//   // 1. OBTENER OPERADORES DE LÓGICA DIFUSA
//   // Usamos los getters definidos en Logics.h / Logics.cpp
//   LogicOperator tnorm = get_tnorm(logic_name);
//   LogicOperator implication = get_implication(logic_name);
//
//   if (tnorm == NULL || implication == NULL) {
//     stop("Logic operator not found or invalid logic name.");
//   }
//
//   int n_rows = I.nrow();
//   int n_cols = I.ncol();
//   std::vector<Factor> F;
//
//   // Grados de verdad disponibles
//   std::vector<double> L = {0.0, 1.0};
//   for(auto val : I) L.push_back(val);
//   std::sort(L.begin(), L.end());
//   L.erase(std::unique(L.begin(), L.end()), L.end());
//
//   bool u_not_empty = true;
//   NumericMatrix CurrentRec(n_rows, n_cols);
//   std::fill(CurrentRec.begin(), CurrentRec.end(), 0.0);
//
//   while (u_not_empty) {
//
//     // --- Paso 1: Encontrar un concepto óptimo inicial (Núcleo) ---
//     NumericVector D(n_cols, 0.0);
//     NumericVector C(n_rows);
//
//     double V = -1.0;
//     bool improved = true;
//
//     while(improved) {
//       improved = false;
//       int best_j = -1;
//       double best_a = -1.0;
//       double max_covered = -1.0;
//
//       for(int j=0; j<n_cols; ++j) {
//         for(double a : L) {
//           if (a <= D[j]) continue;
//           if (a == 0) continue;
//
//           NumericVector D_temp = clone(D);
//           D_temp[j] = a;
//
//           // Calcular cierre difuso (D -> C -> D_closed)
//           NumericVector C_temp = arrow_down(D_temp, I, implication);
//           NumericVector D_closed = arrow_up(C_temp, I, implication);
//
//           // Calcular ganancia de cobertura
//           double coverage_gain = 0.0;
//           for(int r=0; r<n_rows; ++r) {
//             for(int c=0; c<n_cols; ++c) {
//               if (I(r, c) > CurrentRec(r, c)) {
//                 // Importante: Usar T-Norma para calcular el valor del factor
//                 double factor_val = tnorm(C_temp[r], D_closed[c]);
//                 double new_rec = std::max(CurrentRec(r, c), factor_val);
//
//                 double effective_new = std::min(new_rec, I(r, c));
//                 double effective_old = std::min(CurrentRec(r, c), I(r, c));
//
//                 coverage_gain += (effective_new - effective_old);
//               }
//             }
//           }
//
//           if (coverage_gain > V && coverage_gain > max_covered) {
//             max_covered = coverage_gain;
//             best_j = j;
//             best_a = a;
//           }
//         }
//       }
//
//       if (max_covered > V + 1e-9) {
//         V = max_covered;
//         NumericVector D_temp = clone(D);
//         D_temp[best_j] = best_a;
//         // Cerrar el conjunto
//         NumericVector C_temp = arrow_down(D_temp, I, implication);
//         D = arrow_up(C_temp, I, implication);
//         improved = true;
//       }
//     }
//
//     C = arrow_down(D, I, implication);
//     Factor nucleus;
//     nucleus.extent = C;
//     nucleus.intent = D;
//
//     // --- Paso 2: Expansión (Añadir atributos para maximizar ganancia neta) ---
//     Factor extended_factor = expansion(nucleus, I, F, w, tnorm, implication);
//     F.push_back(extended_factor);
//
//     // Actualizar matriz reconstruida
//     CurrentRec = reconstruct_matrix(F, n_rows, n_cols, tnorm);
//
//     // --- Paso 3: Comprobación de parada ---
//     double e_u = 0.0;
//     double total_mass = 0.0;
//     for(int i=0; i<n_rows; ++i) {
//       for(int j=0; j<n_cols; ++j) {
//         total_mass += I(i,j);
//         // Suma de residuos no cubiertos
//         if (I(i,j) > CurrentRec(i,j)) {
//           e_u += (I(i,j) - CurrentRec(i,j));
//         }
//       }
//     }
//
//     if (e_u < 1e-9 || (stop_threshold_ratio > 0 && (e_u / total_mass) < stop_threshold_ratio)) {
//       u_not_empty = false;
//     }
//   }
//
//   List result;
//   for(const auto& f : F) {
//     result.push_back(List::create(Named("extent") = f.extent, Named("intent") = f.intent));
//   }
//   return result;
// }
//
// // --- ALGORITMO 2: ASSO (Versión Binaria) ---
// // Nota: ASSO es inherentemente binario/heurístico. Lo mantenemos simple.
// // [[Rcpp::export]]
// List asso_cpp(NumericMatrix I, double threshold = 0.7, double w_pos = 1.0, double w_neg = 1.0) {
//   int n = I.nrow();
//   int m = I.ncol();
//
//   // 1. Association Matrix
//   NumericMatrix A(m, m);
//   for(int j=0; j<m; ++j) {
//     for(int k=0; k<m; ++k) {
//       if (j == k) { A(j, k) = 1.0; continue; }
//       double support_j = 0, support_jk = 0;
//       for(int i=0; i<n; ++i) {
//         if (I(i, j) > 0) {
//           support_j++;
//           if (I(i, k) > 0) support_jk++;
//         }
//       }
//       A(j, k) = (support_j > 0) ? (support_jk / support_j) : 0.0;
//     }
//   }
//
//   // 2. Candidates
//   std::vector<NumericVector> candidates;
//   for(int k=0; k<m; ++k) {
//     NumericVector cand(m);
//     bool non_empty = false;
//     for(int j=0; j<m; ++j) {
//       if (A(j, k) >= threshold) { cand[j] = 1.0; non_empty = true; }
//       else cand[j] = 0.0;
//     }
//     if(non_empty) candidates.push_back(cand);
//   }
//
//   // 3. Greedy Selection
//   std::vector<Factor> factors;
//   NumericMatrix Covered(n, m);
//   std::fill(Covered.begin(), Covered.end(), 0.0);
//
//   bool improving = true;
//   while(improving) {
//     improving = false;
//     double best_score = 0.0;
//     int best_idx = -1;
//     NumericVector best_extent(n);
//
//     for(size_t k=0; k<candidates.size(); ++k) {
//       NumericVector intent = candidates[k];
//       NumericVector extent(n);
//
//       for(int i=0; i<n; ++i) {
//         bool fits = true;
//         for(int j=0; j<m; ++j) {
//           if(intent[j] > 0 && I(i, j) == 0) { fits = false; break; }
//         }
//         extent[i] = fits ? 1.0 : 0.0;
//       }
//
//       double score = 0.0;
//       for(int i=0; i<n; ++i) {
//         for(int j=0; j<m; ++j) {
//           double val = std::min(extent[i], intent[j]);
//           if (val > Covered(i, j)) {
//             if (I(i, j) > 0) score += w_pos;
//             else score -= w_neg;
//           }
//         }
//       }
//
//       if (score > best_score) {
//         best_score = score;
//         best_idx = k;
//         best_extent = extent;
//       }
//     }
//
//     if (best_score > 0) {
//       Factor f; f.extent = best_extent; f.intent = candidates[best_idx];
//       factors.push_back(f);
//
//       for(int i=0; i<n; ++i)
//         for(int j=0; j<m; ++j)
//           if (std::min(f.extent[i], f.intent[j]) > Covered(i, j))
//             Covered(i, j) = std::min(f.extent[i], f.intent[j]);
//
//           improving = true;
//           candidates.erase(candidates.begin() + best_idx);
//     }
//   }
//
//   List result;
//   for(const auto& f : factors) {
//     result.push_back(List::create(Named("extent") = f.extent, Named("intent") = f.intent));
//   }
//   return result;
// }
