#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <chrono>
#include "fcaR_bitset.h"

using namespace Rcpp;
using namespace fcaR; // FastBitset

class Algo1OptSolver {
public:
    int m; // |G1| (objects in C1)
    int n; // |M2| (attributes in C2)
    int num_extents;
    int num_intents;
    
    std::vector<FastBitset> ext_mat; // extents of C1 (each of length m)
    std::vector<FastBitset> int_mat; // intents of C2 (each of length n)
    std::vector<FastBitset> sorted_intents;

    std::vector<FastBitset> R_cols; // R columns (length m each)
    std::vector<FastBitset> R_rows; // R rows (length n each)

    std::vector<std::vector<int>> bonds_found; 

    Algo1OptSolver(IntegerMatrix extents, IntegerMatrix intents_mat) {
        m = extents.nrow();
        num_extents = extents.ncol();
        n = intents_mat.nrow();
        num_intents = intents_mat.ncol();

        ext_mat.resize(num_extents);
        for (int i = 0; i < num_extents; i++) {
            ext_mat[i].init(m);
            for (int j = 0; j < m; j++) {
                if (extents(j, i) > 0) ext_mat[i].set(j);
            }
        }

        int_mat.resize(num_intents);
        for (int i = 0; i < num_intents; i++) {
            int_mat[i].init(n);
            for (int j = 0; j < n; j++) {
                if (intents_mat(j, i) > 0) int_mat[i].set(j);
            }
        }
        sorted_intents = int_mat;
        std::sort(sorted_intents.begin(), sorted_intents.end());

        R_cols.resize(n);
        for (int j = 0; j < n; j++) R_cols[j].init(m);
        
        R_rows.resize(m);
        for (int i = 0; i < m; i++) R_rows[i].init(n);
    }

    void fill_bond(int k) {
        if (k < n - 1) {
            for (int i = 0; i < num_extents; i++) {
                bool match = true;
                // Xk \cap {0..k-1} == Rak \cap {0..k-1}
                // Prefix check: ext_mat[i] and R_cols[k] must match up to k-1
                for (int x = 0; x < k; x++) {
                    if (ext_mat[i].test(x) != R_cols[k].test(x)) {
                        match = false;
                        break;
                    }
                }
                
                if (match) {
                    R_cols[k] = ext_mat[i];
                    // Update R_rows across these objects for attribute k
                    for (int x = 0; x < m; x++) {
                        if (R_cols[k].test(x)) R_rows[x].set(k);
                        else R_rows[x].reset(k);
                    }
                    
                    for (int j = 0; j < num_intents; j++) {
                        bool match_int = true;
                        // Ak \cap {0..k} == Rxk \cap {0..k}
                        // Prefix check: int_mat[j] and R_rows[k] must match up to k
                        for (int a = 0; a <= k; a++) {
                            if (int_mat[j].test(a) != R_rows[k].test(a)) {
                                match_int = false;
                                break;
                            }
                        }
                        
                        if (match_int) {
                            R_rows[k] = int_mat[j];
                            // Update R_cols across these attributes for object k
                            for (int a = k + 1; a < n; a++) {
                                if (R_rows[k].test(a)) R_cols[a].set(k);
                                else R_cols[a].reset(k);
                            }
                            fill_bond(k + 1);
                        }
                    }
                }
            }
        } else {
            // k == n - 1
            for (int i = 0; i < num_extents; i++) {
                bool match = true;
                for (int x = 0; x < n - 1; x++) {
                    if (ext_mat[i].test(x) != R_cols[n - 1].test(x)) {
                        match = false;
                        break;
                    }
                }
                
                if (match) {
                    R_cols[n - 1] = ext_mat[i];
                    for (int x = 0; x < m; x++) {
                        if (R_cols[n - 1].test(x)) R_rows[x].set(n - 1);
                        else R_rows[x].reset(n - 1);
                    }

                    bool all_in_intents = true;
                    // Check rows n-1 up to m-1
                    for (int x = n - 1; x < m; x++) {
                        if (!std::binary_search(sorted_intents.begin(), sorted_intents.end(), R_rows[x])) {
                            all_in_intents = false;
                            break;
                        }
                    }

                    if (all_in_intents) {
                        std::vector<int> bond;
                        bond.reserve(m * n);
                        for (int attr = 0; attr < n; attr++) {
                            for (int obj = 0; obj < m; obj++) {
                                if (R_cols[attr].test(obj)) {
                                    bond.push_back(attr * m + obj);
                                }
                            }
                        }
                        bonds_found.push_back(bond);
                    }
                }
            }
        }
    }
};

// [[Rcpp::export]]
List bonds_mcis_cpp(IntegerMatrix extents, IntegerMatrix intents, bool verbose = false) {
    if (intents.nrow() > extents.nrow()) {
        stop("Algorithm 1 assumes number of attributes in C2 <= number of objects in C1.");
    }

    auto start_time = std::chrono::high_resolution_clock::now();

    Algo1OptSolver solver(extents, intents);
    solver.fill_bond(0);

    int num_bonds = solver.bonds_found.size();
    int m = solver.m;
    int n = solver.n;
    
    IntegerVector p(num_bonds + 1);
    p[0] = 0;
    std::vector<int> i_vec;
    for (int k = 0; k < num_bonds; k++) {
        for (int idx : solver.bonds_found[k]) i_vec.push_back(idx);
        p[k + 1] = i_vec.size();
    }

    S4 dgC("dgCMatrix");
    dgC.slot("i") = wrap(i_vec);
    dgC.slot("p") = p;
    dgC.slot("x") = NumericVector(i_vec.size(), 1.0);
    dgC.slot("Dim") = IntegerVector::create(m * n, num_bonds);

    auto end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end_time - start_time;

    return List::create(
        _["intents"] = dgC,
        _["elapsed"] = elapsed.count()
    );
}
