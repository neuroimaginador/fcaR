#include <Rcpp.h>
#include "Logics.h" // Importante: Incluir cabecera de logicas
#include <chrono>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>
#include <set>
#include <deque>
#include <algorithm>
#include <sstream>
#include <iomanip>

// --- 1. ARQUITECTURA OPTIMIZADA ---
using FuzzySet = std::vector<double>;
using SigmaMap = std::map<FuzzySet, FuzzySet>;

// Estructura para pasar los operadores matematicos sin overhead de strings
struct LogicContext {
  LogicOperator tnorm;
  LogicOperator implication;
};

struct AttributeManager {
  std::vector<std::string> idx_to_attr;
  std::map<std::string, int> attr_to_idx;
};

struct Metrics {
  double execution_time = 0; int iterations = 0; int iterations_saturate = 0;
  int iterations_prune = 0; int pi_calculations = 0; int add_derived_calls = 0;
  int final_implication_count = 0; int worklist_max_size = 0;
};

// --- 2. LÓGICA DIFUSA DINÁMICA ---

FuzzySet set_union(const FuzzySet& A, const FuzzySet& B) {
  FuzzySet result = A;
  for (size_t i = 0; i < A.size(); ++i) {
    result[i] = std::max(A[i], B[i]);
  }
  return result;
}

FuzzySet set_setminus(FuzzySet A, const FuzzySet& B) {
  const double epsilon = 1e-12;
  for (size_t i = 0; i < A.size(); ++i) {
    // La diferencia de conjuntos suele ser estructural, pero si dependiera de la lógica
    // específica, habría que usar el contexto. Aquí mantenemos la definición estándar.
    if (B[i] >= A[i] - epsilon) {
      A[i] = 0.0;
    }
  }
  return A;
}

// Ahora otimes usa el tnorm del contexto
FuzzySet otimes(double alpha, const FuzzySet& A, const LogicContext& ctx) {
  FuzzySet result = A;
  for (size_t i = 0; i < A.size(); ++i) {
    result[i] = ctx.tnorm(alpha, A[i]);
  }
  return result;
}

// Ahora S usa la implicacion del contexto
double S(const FuzzySet& X, const FuzzySet& Y, const LogicContext& ctx) {
  double min_val = 1.0;
  for (size_t i = 0; i < X.size(); ++i) {
    min_val = std::min(min_val, ctx.implication(X[i], Y[i]));
  }
  return min_val;
}

double set_sum(const FuzzySet& A) {
  double total = 0.0;
  for (double val : A) {
    total += val;
  }
  return total;
}

std::string print_set_cpp(const FuzzySet& A, const AttributeManager& am) {
  std::stringstream ss;
  ss << std::fixed << std::setprecision(2);
  bool first = true;
  for (size_t i = 0; i < A.size(); ++i) {
    if (A[i] > 1e-9) {
      if (!first) ss << ", ";
      ss << am.idx_to_attr[i];
      if (A[i] < 1.0) ss << "(" << A[i] << ")";
      first = false;
    }
  }
  if (first) return "{}";
  return ss.str();
}

std::string print_implication_cpp(const FuzzySet& A, const FuzzySet& B, const AttributeManager& am) {
  return "{" + print_set_cpp(A, am) + "} -> {" + print_set_cpp(B, am) + "}";
}

void print_sigma_cpp(const SigmaMap& sigma, const AttributeManager& am, const std::string& indent = "") {
  if (sigma.empty()) {
    Rcpp::Rcout << indent << "(Empty system)\n";
    return;
  }
  for(const auto& pair : sigma) {
    Rcpp::Rcout << indent << "- " << print_implication_cpp(pair.first, pair.second, am) << "\n";
  }
}

// --- 3. COMPONENTES REUTILIZABLES DE ALGORITMOS ---

FuzzySet calculate_pi_operator_internal(FuzzySet A, const SigmaMap& sigma, Metrics& metrics, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  metrics.pi_calculations++;
  if (sigma.empty()) return A;
  if (verbose) Rcpp::Rcout << "  -> Calculating pi(" << print_set_cpp(A, am) << ")...\n";
  FuzzySet closure = A;
  for(const auto& pair : sigma) {
    double degree = S(pair.first, A, ctx);
    if (degree > 1e-9) {
      FuzzySet update = otimes(degree, pair.second, ctx);
      closure = set_union(closure, update);
    }
  }
  if (verbose) Rcpp::Rcout << "  <- Pi result: " << print_set_cpp(closure, am) << "\n";
  return closure;
}

void add_derived_internal(
    const FuzzySet& A, const FuzzySet& B, const FuzzySet& C, const FuzzySet& D,
    std::vector<FuzzySet>& derived_lhs, std::vector<FuzzySet>& derived_rhs,
    bool use_pruning, Metrics& metrics, const std::vector<double>& L_vec, bool verbose,
    const AttributeManager& am, const LogicContext& ctx
) {
  metrics.add_derived_calls++;
  for(double alpha : L_vec) {
    for(double beta : L_vec) {
      FuzzySet alphaA = otimes(alpha, A, ctx), alphaB = otimes(alpha, B, ctx);
      FuzzySet betaC = otimes(beta, C, ctx), betaD = otimes(beta, D, ctx);

      FuzzySet H = set_setminus(betaD, alphaA);
      if (set_sum(H) == 0) continue;

      FuzzySet G = set_union(alphaA, set_setminus(betaC, alphaB));

      if (verbose) Rcpp::Rcout << "    -> Candidate (a=" << alpha << ",b=" << beta << "): " << print_implication_cpp(G, H, am) << "\n";

      FuzzySet H_sem_reduced = H;
      if (use_pruning) {
        SigmaMap parents;
        parents[A] = B; parents[C] = D;
        // Recursion con contexto
        H_sem_reduced = set_setminus(H, calculate_pi_operator_internal(G, parents, metrics, false, am, ctx));
      }

      FuzzySet H_final = set_setminus(H_sem_reduced, G);
      if (set_sum(H_final) > 0) {
        if (verbose) Rcpp::Rcout << "       - SURVIVED.\n";
        derived_lhs.push_back(G);
        derived_rhs.push_back(H_final);
      }
    }
  }
}

void saturate_system(SigmaMap& sigma, Metrics& metrics, bool use_pruning, const std::vector<double>& L_vec, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  std::map<FuzzySet, std::map<FuzzySet, bool>> pairwise_matrix;
  for(const auto& p1 : sigma) {
    for(const auto& p2 : sigma) {
      pairwise_matrix[p1.first][p2.first] = false;
    }
  }
  while (true) {
    metrics.iterations_saturate++;
    if (verbose) {
      Rcpp::Rcout << "\n--- Saturation Pass #" << metrics.iterations_saturate << " ---\n";
      print_sigma_cpp(sigma, am, "  ");
    }
    std::vector<std::pair<FuzzySet, FuzzySet>> pairs_to_process;
    for (const auto& p1 : pairwise_matrix) {
      for (const auto& p2 : p1.second) {
        if (!p2.second) pairs_to_process.push_back(std::make_pair(p1.first, p2.first));
      }
    }
    if (pairs_to_process.empty()) break;

    std::vector<FuzzySet> derived_lhs_pass, derived_rhs_pass;
    for (const auto& pair : pairs_to_process) {
      const FuzzySet& A = pair.first;
      const FuzzySet& C = pair.second;
      pairwise_matrix[A][C] = true;
      if (A != C) pairwise_matrix[C][A] = true;
      if (sigma.find(A) == sigma.end() || sigma.find(C) == sigma.end()) continue;

      const FuzzySet& B = sigma.at(A);
      const FuzzySet& D = sigma.at(C);

      if (verbose) Rcpp::Rcout << "  > Combining " << print_implication_cpp(A, B, am) << " AND " << print_implication_cpp(C, D, am) << "\n";

      add_derived_internal(A, B, C, D, derived_lhs_pass, derived_rhs_pass, use_pruning, metrics, L_vec, verbose, am, ctx);
    }

    bool added_in_pass = false;
    if (!derived_lhs_pass.empty()) {
      for (size_t i = 0; i < derived_lhs_pass.size(); ++i) {
        const FuzzySet& G = derived_lhs_pass[i];
        const FuzzySet& H = derived_rhs_pass[i];
        auto it = sigma.find(G);
        if (it == sigma.end()) {
          added_in_pass = true;
          sigma[G] = H;
          for (const auto& existing_pair : sigma) {
            pairwise_matrix[G][existing_pair.first] = false;
            pairwise_matrix[existing_pair.first][G] = false;
          }
        } else {
          FuzzySet B_old = it->second;
          FuzzySet B_merged = set_union(B_old, H);
          if (B_merged != B_old) {
            added_in_pass = true;
            sigma[G] = B_merged;
            for (const auto& existing_pair : sigma) {
              pairwise_matrix[G][existing_pair.first] = false;
              pairwise_matrix[existing_pair.first][G] = false;
            }
          }
        }
      }
    }
    if (!added_in_pass) break;
  }
}

void prune_system(SigmaMap& sigma, Metrics& metrics, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  while(true) {
    metrics.iterations_prune++;
    if (verbose) {
      Rcpp::Rcout << "\n--- Pruning Pass #" << metrics.iterations_prune << " ---\n";
      print_sigma_cpp(sigma, am, "  ");
    }
    SigmaMap sigma_old = sigma;
    SigmaMap sigma_f;
    for(const auto& pair : sigma_old) {
      const FuzzySet& A = pair.first;
      SigmaMap sigma_prime = sigma_old;
      sigma_prime.erase(A);
      if (verbose) Rcpp::Rcout << "  > Checking " << print_implication_cpp(A, pair.second, am) << "\n";

      FuzzySet D = calculate_pi_operator_internal(A, sigma_prime, metrics, verbose, am, ctx);
      FuzzySet B_new = set_setminus(pair.second, D);

      if(set_sum(B_new) > 0) {
        sigma_f[A] = B_new;
      } else {
        if (verbose) Rcpp::Rcout << "    >> PURGED (redundant).\n";
      }
    }
    if(sigma == sigma_f) break;
    sigma = sigma_f;
  }
}

// --- 4. IMPLEMENTACIÓN DE ALGORITMOS COMPLETOS ---

SigmaMap internal_run_final_ts(SigmaMap sigma_in, Metrics& metrics, bool use_pruning, const std::vector<double>& L_vec, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  saturate_system(sigma_in, metrics, use_pruning, L_vec, verbose, am, ctx);
  prune_system(sigma_in, metrics, verbose, am, ctx);
  return sigma_in;
}

SigmaMap internal_run_dosp(SigmaMap sigma_in, Metrics& metrics, bool use_pruning, const std::vector<double>& L_vec, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  while(true) {
    metrics.iterations++;
    if (verbose) Rcpp::Rcout << "\n\n==================\n--- DO-SP Pass #" << metrics.iterations << " ---\n==================\n";
    SigmaMap sigma_old = sigma_in;
    saturate_system(sigma_in, metrics, use_pruning, L_vec, verbose, am, ctx);
    prune_system(sigma_in, metrics, verbose, am, ctx);
    if(sigma_in == sigma_old) break;
  }
  return sigma_in;
}

struct SigmaRule { FuzzySet B; bool is_redundant; };
using MonotonicSigma = std::map<FuzzySet, SigmaRule>;

SigmaMap internal_run_monotonic(std::deque<std::pair<FuzzySet, FuzzySet>> W, Metrics& metrics, bool use_pruning, const std::vector<double>& L_vec, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  MonotonicSigma sigma_do;
  while(!W.empty()) {
    metrics.iterations++;
    if (W.size() > (size_t)metrics.worklist_max_size) metrics.worklist_max_size = W.size();
    std::pair<FuzzySet, FuzzySet> imp = W.front(); W.pop_front();
    const FuzzySet& A = imp.first; const FuzzySet& B = imp.second;
    if (verbose) { Rcpp::Rcout << "\n--- Iteration #" << metrics.iterations << " (WS size: " << W.size()+1 << ") ---\n" << "1. PROC: " << print_implication_cpp(A,B,am) << "\n"; }

    SigmaMap current_sigma;
    for(const auto& pair : sigma_do) current_sigma[pair.first] = pair.second.B;

    FuzzySet B_new = set_setminus(B, calculate_pi_operator_internal(A, current_sigma, metrics, verbose, am, ctx));

    if (verbose) Rcpp::Rcout << "2. REDUCTION: B_new = " << print_set_cpp(B_new, am) << "\n";
    bool propagate = false;
    FuzzySet B_for_propagation = B_new;

    auto it = sigma_do.find(A);
    if (it == sigma_do.end()) {
      if (verbose) Rcpp::Rcout << "3. DECISION: Premise is new.\n";
      if (set_sum(B_new) > 0) {
        if (verbose) Rcpp::Rcout << "   >> ADDING rule to system: " << print_implication_cpp(A, B_new, am) << "\n";
        sigma_do[A] = {B_new, false}; propagate = true;
      } else {
        if (verbose) Rcpp::Rcout << "   >> DISCARDED (trivial after reduction).\n";
      }
    } else {
      if (verbose) Rcpp::Rcout << "3. DECISION: Premise exists. Merging...\n";
      FuzzySet B_old = it->second.B; bool is_redundant_old = it->second.is_redundant;
      FuzzySet B_merged = set_union(B_old, B_new); B_for_propagation = B_merged;
      if (verbose) { Rcpp::Rcout << "   - B_old: " << print_set_cpp(B_old, am) << "\n" << "   - B_merged: " << print_set_cpp(B_merged, am) << "\n"; }

      SigmaMap sigma_prime = current_sigma;
      if(sigma_prime.count(A) > 0) sigma_prime.erase(A);
      bool is_redundant_new = (set_sum(set_setminus(B_merged, calculate_pi_operator_internal(A, sigma_prime, metrics, false, am, ctx))) == 0);
      if (verbose) Rcpp::Rcout << "   - Redundancy check: is_redundant_new = " << (is_redundant_new ? "true" : "false") << "\n";

      if (B_merged != B_old || is_redundant_new != is_redundant_old) {
        if (verbose) Rcpp::Rcout << "   >> UPDATING rule in system.\n";
        sigma_do[A] = {B_merged, is_redundant_new}; propagate = true;
      } else {
        if (verbose) Rcpp::Rcout << "   >> NO CHANGE needed.\n";
      }
    }
    if(propagate) {
      if (verbose) Rcpp::Rcout << "4. PROPAGATING changes...\n";
      for(const auto& pair : sigma_do) {
        if (pair.first != A) {
          if (set_sum(set_setminus(otimes(S(A, pair.first, ctx), B_new, ctx), pair.first)) > 0) {
            W.push_back(std::make_pair(pair.first, pair.second.B));
            if (verbose) Rcpp::Rcout << "    - Re-queueing: " << print_implication_cpp(pair.first, pair.second.B, am) << "\n";
          }
        }
        std::vector<FuzzySet> derived_lhs, derived_rhs;
        add_derived_internal(A, B_for_propagation, pair.first, pair.second.B, derived_lhs, derived_rhs, use_pruning, metrics, L_vec, false, am, ctx);
        add_derived_internal(pair.first, pair.second.B, A, B_for_propagation, derived_lhs, derived_rhs, use_pruning, metrics, L_vec, false, am, ctx);
        for(size_t k=0; k<derived_lhs.size(); ++k) W.push_back(std::make_pair(derived_lhs[k], derived_rhs[k]));
      }
    }
  }
  SigmaMap final_sigma;
  for(const auto& pair : sigma_do) {
    if (!pair.second.is_redundant) final_sigma[pair.first] = pair.second.B;
  }
  prune_system(final_sigma, metrics, verbose, am, ctx);
  return final_sigma;
}

SigmaMap internal_run_priority(std::deque<std::pair<FuzzySet, FuzzySet>> W, Metrics& metrics, bool use_pruning, const std::vector<double>& L_vec, bool verbose, const AttributeManager& am, const LogicContext& ctx) {
  SigmaMap sigma_do;
  while(!W.empty()) {
    metrics.iterations++;
    if (W.size() > (size_t)metrics.worklist_max_size) metrics.worklist_max_size = W.size();
    std::pair<FuzzySet, FuzzySet> imp = W.front(); W.pop_front();
    const FuzzySet& A = imp.first; const FuzzySet& B = imp.second;
    if (verbose) { Rcpp::Rcout << "\n--- Iteration #" << metrics.iterations << " (WS size: " << W.size()+1 << ") ---\n" << "1. PROC: " << print_implication_cpp(A,B,am) << "\n"; }

    SigmaMap sigma_prime = sigma_do;
    if(sigma_prime.count(A) > 0) sigma_prime.erase(A);

    FuzzySet B_new = set_setminus(B, calculate_pi_operator_internal(A, sigma_prime, metrics, verbose, am, ctx));

    bool propagate = false;
    if (set_sum(B_new) == 0) {
      if (sigma_do.count(A) > 0) {
        if(verbose) Rcpp::Rcout << "  >> PURGING obsolete rule.\n";
        sigma_do.erase(A);
      }
      continue;
    }
    auto it = sigma_do.find(A);
    if (it != sigma_do.end()) {
      FuzzySet B_old = it->second;
      FuzzySet B_merged = set_union(B_old, B_new);
      if(B_merged != B_old) {
        if(verbose) Rcpp::Rcout << "  >> UPDATING rule in place and propagating.\n";
        sigma_do[A] = B_merged;
        propagate = true;
      }
    } else {
      if(verbose) Rcpp::Rcout << "  >> ADDING new rule and propagating.\n";
      sigma_do[A] = B_new;
      propagate = true;
    }
    if(propagate) {
      for (const auto& pair : sigma_do) {
        if (pair.first == A) continue;
        const FuzzySet& C = pair.first;
        if (set_sum(set_setminus(otimes(S(A, C, ctx), B_new, ctx), C)) > 0) {
          W.push_back(std::make_pair(C, pair.second));
        }
        std::vector<FuzzySet> d_lhs, d_rhs;
        add_derived_internal(A, sigma_do.at(A), C, pair.second, d_lhs, d_rhs, use_pruning, metrics, L_vec, false, am, ctx);
        add_derived_internal(C, pair.second, A, sigma_do.at(A), d_lhs, d_rhs, use_pruning, metrics, L_vec, false, am, ctx);
        for(size_t k=0; k<d_lhs.size(); ++k) W.push_back(std::make_pair(d_lhs[k], d_rhs[k]));
      }
    }
  }
  prune_system(sigma_do, metrics, verbose, am, ctx);
  return sigma_do;
}

// --- 5. INTERFAZ Rcpp (ADAPTADA A S4 Sparse Matrix) ---

SigmaMap Sparse_to_Sigma(Rcpp::S4 lhs_mat, Rcpp::S4 rhs_mat, AttributeManager& am) {
  SigmaMap s;
  // Extraer vectores de la matriz LHS (dgCMatrix o ngCMatrix)
  Rcpp::IntegerVector i_lhs = lhs_mat.slot("i");
  Rcpp::IntegerVector p_lhs = lhs_mat.slot("p");
  Rcpp::IntegerVector dim_lhs = lhs_mat.slot("Dim");
  int ncols = dim_lhs[1];
  int nrows = dim_lhs[0]; // Número de atributos
  bool has_x_lhs = lhs_mat.hasSlot("x");
  Rcpp::NumericVector x_lhs;
  if (has_x_lhs) x_lhs = lhs_mat.slot("x");

  Rcpp::IntegerVector i_rhs = rhs_mat.slot("i");
  Rcpp::IntegerVector p_rhs = rhs_mat.slot("p");
  bool has_x_rhs = rhs_mat.hasSlot("x");
  Rcpp::NumericVector x_rhs;
  if (has_x_rhs) x_rhs = rhs_mat.slot("x");

  for (int col = 0; col < ncols; ++col) {
    FuzzySet current_lhs(nrows, 0.0);
    int start_idx_lhs = p_lhs[col];
    int end_idx_lhs = p_lhs[col + 1];
    for (int k = start_idx_lhs; k < end_idx_lhs; ++k) {
      int row_idx = i_lhs[k];
      double val = has_x_lhs ? x_lhs[k] : 1.0;
      current_lhs[row_idx] = val;
    }

    FuzzySet current_rhs(nrows, 0.0);
    int start_idx_rhs = p_rhs[col];
    int end_idx_rhs = p_rhs[col + 1];
    for (int k = start_idx_rhs; k < end_idx_rhs; ++k) {
      int row_idx = i_rhs[k];
      double val = has_x_rhs ? x_rhs[k] : 1.0;
      current_rhs[row_idx] = val;
    }
    if (set_sum(current_rhs) > 0) {
      s[current_lhs] = current_rhs;
    }
  }
  return s;
}

Rcpp::List Sigma_to_R(const SigmaMap& s, const AttributeManager& am) {
  std::vector<int> lhs_i, lhs_j, rhs_i, rhs_j;
  std::vector<double> lhs_x, rhs_x;
  int col_idx = 0;

  for (const auto& pair : s) {
    const FuzzySet& lhs = pair.first;
    const FuzzySet& rhs = pair.second;

    for (size_t r = 0; r < lhs.size(); ++r) {
      if (lhs[r] > 0) {
        lhs_i.push_back(r); lhs_j.push_back(col_idx); lhs_x.push_back(lhs[r]);
      }
    }
    for (size_t r = 0; r < rhs.size(); ++r) {
      if (rhs[r] > 0) {
        rhs_i.push_back(r); rhs_j.push_back(col_idx); rhs_x.push_back(rhs[r]);
      }
    }
    col_idx++;
  }
  return Rcpp::List::create(
    Rcpp::Named("lhs_i") = lhs_i, Rcpp::Named("lhs_j") = lhs_j, Rcpp::Named("lhs_x") = lhs_x,
                Rcpp::Named("rhs_i") = rhs_i, Rcpp::Named("rhs_j") = rhs_j, Rcpp::Named("rhs_x") = rhs_x,
                            Rcpp::Named("n_rules") = col_idx
  );
}

Rcpp::List Metrics_to_R(const Metrics& m) {
  return Rcpp::List::create(
    Rcpp::Named("execution_time") = m.execution_time, Rcpp::Named("iterations") = m.iterations,
    Rcpp::Named("iterations_saturate") = m.iterations_saturate, Rcpp::Named("iterations_prune") = m.iterations_prune,
    Rcpp::Named("pi_calculations") = m.pi_calculations, Rcpp::Named("add_derived_calls") = m.add_derived_calls,
    Rcpp::Named("final_implication_count") = m.final_implication_count, Rcpp::Named("worklist_max_size") = m.worklist_max_size
  );
}

void setup_am(AttributeManager& am, std::vector<std::string> attrs) {
  am.idx_to_attr = attrs;
  for(size_t i=0; i<attrs.size(); ++i) am.attr_to_idx[attrs[i]] = i;
}

// --- EXPORTED FUNCTIONS ---
// Ahora aceptan un parámetro 'logic_name'

// [[Rcpp::export]]
Rcpp::List run_final_ts_rcpp_optimized(Rcpp::S4 lhs_in, Rcpp::S4 rhs_in, std::vector<std::string> attributes, Rcpp::NumericVector L, std::string logic_name, bool use_pruning = true, bool verbose = false) {
  auto start_time = std::chrono::high_resolution_clock::now();
  Metrics metrics;
  AttributeManager am;
  setup_am(am, attributes);

  // Configurar contexto lógico
  LogicContext ctx;
  ctx.tnorm = get_tnorm(logic_name);
  ctx.implication = get_implication(logic_name);
  if (ctx.tnorm == NULL || ctx.implication == NULL) Rcpp::stop("Logic not found or invalid.");

  std::vector<double> L_vec = Rcpp::as<std::vector<double>>(L);
  SigmaMap sigma_in = Sparse_to_Sigma(lhs_in, rhs_in, am);
  SigmaMap sigma_final = internal_run_final_ts(sigma_in, metrics, use_pruning, L_vec, verbose, am, ctx);

  metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
  metrics.final_implication_count = sigma_final.size();
  return Rcpp::List::create(Rcpp::Named("Sigma") = Sigma_to_R(sigma_final, am), Rcpp::Named("metrics") = Metrics_to_R(metrics));
}

// [[Rcpp::export]]
Rcpp::List run_direct_optimal_sp_rcpp_optimized(Rcpp::S4 lhs_in, Rcpp::S4 rhs_in, std::vector<std::string> attributes, Rcpp::NumericVector L, std::string logic_name, bool use_pruning = true, bool verbose = false) {
  auto start_time = std::chrono::high_resolution_clock::now();
  Metrics metrics;
  AttributeManager am;
  setup_am(am, attributes);

  LogicContext ctx;
  ctx.tnorm = get_tnorm(logic_name);
  ctx.implication = get_implication(logic_name);
  if (ctx.tnorm == NULL || ctx.implication == NULL) Rcpp::stop("Logic not found or invalid.");

  std::vector<double> L_vec = Rcpp::as<std::vector<double>>(L);
  SigmaMap sigma_in = Sparse_to_Sigma(lhs_in, rhs_in, am);
  SigmaMap sigma_final = internal_run_dosp(sigma_in, metrics, use_pruning, L_vec, verbose, am, ctx);

  metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
  metrics.final_implication_count = sigma_final.size();
  return Rcpp::List::create(Rcpp::Named("Sigma") = Sigma_to_R(sigma_final, am), Rcpp::Named("metrics") = Metrics_to_R(metrics));
}

// [[Rcpp::export]]
Rcpp::List run_monotonic_incremental_rcpp_optimized(Rcpp::S4 lhs_in, Rcpp::S4 rhs_in, std::vector<std::string> attributes, Rcpp::NumericVector L, std::string logic_name, bool use_pruning = true, bool verbose = false) {
  auto start_time = std::chrono::high_resolution_clock::now();
  Metrics metrics;
  AttributeManager am;
  setup_am(am, attributes);

  LogicContext ctx;
  ctx.tnorm = get_tnorm(logic_name);
  ctx.implication = get_implication(logic_name);
  if (ctx.tnorm == NULL || ctx.implication == NULL) Rcpp::stop("Logic not found or invalid.");

  std::vector<double> L_vec = Rcpp::as<std::vector<double>>(L);
  SigmaMap sigma_in = Sparse_to_Sigma(lhs_in, rhs_in, am);
  std::deque<std::pair<FuzzySet, FuzzySet>> W;
  for(const auto& pair : sigma_in) { W.push_back(std::make_pair(pair.first, pair.second)); }

  SigmaMap sigma_final = internal_run_monotonic(W, metrics, use_pruning, L_vec, verbose, am, ctx);

  metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
  metrics.final_implication_count = sigma_final.size();
  return Rcpp::List::create(Rcpp::Named("Sigma") = Sigma_to_R(sigma_final, am), Rcpp::Named("metrics") = Metrics_to_R(metrics));
}

// [[Rcpp::export]]
Rcpp::List run_priority_refinement_rcpp_optimized(Rcpp::S4 lhs_in, Rcpp::S4 rhs_in, std::vector<std::string> attributes, Rcpp::NumericVector L, std::string logic_name, bool use_pruning = true, bool verbose = false) {
  auto start_time = std::chrono::high_resolution_clock::now();
  Metrics metrics;
  AttributeManager am;
  setup_am(am, attributes);

  LogicContext ctx;
  ctx.tnorm = get_tnorm(logic_name);
  ctx.implication = get_implication(logic_name);
  if (ctx.tnorm == NULL || ctx.implication == NULL) Rcpp::stop("Logic not found or invalid.");

  std::vector<double> L_vec = Rcpp::as<std::vector<double>>(L);
  SigmaMap sigma_in = Sparse_to_Sigma(lhs_in, rhs_in, am);
  std::deque<std::pair<FuzzySet, FuzzySet>> W;
  for(const auto& pair : sigma_in) { W.push_back(std::make_pair(pair.first, pair.second)); }

  SigmaMap sigma_final = internal_run_priority(W, metrics, use_pruning, L_vec, verbose, am, ctx);

  metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
  metrics.final_implication_count = sigma_final.size();
  return Rcpp::List::create(Rcpp::Named("Sigma") = Sigma_to_R(sigma_final, am), Rcpp::Named("metrics") = Metrics_to_R(metrics));
}
