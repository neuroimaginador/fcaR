#include <Rcpp.h>
#include <string>
#include <sstream>
#include <iomanip>
#include <cmath>

using namespace Rcpp;

// Helper para formatear valores numéricos (solo si != 1)
inline std::string format_val_string(double val, int precision) {
  // Si es 1 (o muy cercano), devolvemos string vacía
  if (std::abs(val - 1.0) < 1e-9) {
    return "";
  }
  std::ostringstream ss;
  ss << " [" << std::fixed << std::setprecision(precision) << val << "]";
  return ss.str();
}

// Convierte una columna CSC a string, manejando dgCMatrix y ngCMatrix
std::string sparse_col_to_string_robust(const IntegerVector& p,
                                        const IntegerVector& i,
                                        const NumericVector& x, // Puede estar vacío
                                        bool has_x,             // ¿Es dgCMatrix?
                                        const CharacterVector& names,
                                        int col_idx,
                                        int precision) {

  int start = p[col_idx];
  int end = p[col_idx + 1];

  if (start == end) {
    return "{}";
  }

  std::ostringstream ss;
  ss << "{";

  for (int k = start; k < end; ++k) {
    if (k > start) ss << ", ";

    int row_idx = i[k];

    // Imprimir nombre del atributo/objeto
    if (row_idx < names.size()) {
      ss << std::string(names[row_idx]);
    }

    // Manejo del valor
    if (has_x) {
      // dgCMatrix: Miramos el valor real
      ss << format_val_string(x[k], precision);
    }
    // Si es ngCMatrix (!has_x), el valor es implícitamente 1,
    // así que no imprimimos nada extra.
  }

  ss << "}";
  return ss.str();
}

// [[Rcpp::export]]
CharacterVector get_concept_strings_cpp(S4 extents,
                                        S4 intents,
                                        CharacterVector objects,
                                        CharacterVector attributes,
                                        int precision) {

  // --- PROCESAR EXTENTS ---
  IntegerVector ext_p = extents.slot("p");
  IntegerVector ext_i = extents.slot("i");
  NumericVector ext_x;
  bool ext_has_x = extents.hasSlot("x");
  if (ext_has_x) {
    ext_x = extents.slot("x");
  }

  // --- PROCESAR INTENTS ---
  IntegerVector int_p = intents.slot("p");
  IntegerVector int_i = intents.slot("i");
  NumericVector int_x;
  bool int_has_x = intents.hasSlot("x");
  if (int_has_x) {
    int_x = intents.slot("x");
  }

  // Asumimos que ncol es igual en ambos (propiedad de conceptos FCA)
  int n_cols = ext_p.size() - 1;
  CharacterVector result(n_cols);

  for (int j = 0; j < n_cols; ++j) {
    std::string str_ext = sparse_col_to_string_robust(
      ext_p, ext_i, ext_x, ext_has_x, objects, j, precision
    );

    std::string str_int = sparse_col_to_string_robust(
      int_p, int_i, int_x, int_has_x, attributes, j, precision
    );

    // Construcción final: "ID: (Ext, Int)"
    std::string final_str = std::to_string(j + 1) + ": (" + str_ext + ", " + str_int + ")";
    result[j] = final_str;
  }

  return result;
}
