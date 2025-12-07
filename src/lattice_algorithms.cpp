#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <map>
#include <queue>
#include <string>
#include <vector>

// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(Rcpp)]]

using namespace Rcpp;

// ============================================================================
// DATA STRUCTURES
// ============================================================================

struct Node {
  int id;
  int grade = 0;
  double x_coord = 0.0;
  double y_coord = 0.0;

  // Force-Directed
  double dx = 0.0;
  double dy = 0.0;

  std::vector<int>
      predecessors; // Lower neighbors (parents in the hasse diagram sense?)
  std::vector<int> successors; // Upper neighbors (children)

  Node() : id(0), grade(-1), x_coord(0.0), y_coord(0.0) {}
  Node(int _id) : id(_id), grade(-1), x_coord(0.0), y_coord(0.0) {}
  Node(int _id, int _grade, double _x)
      : id(_id), grade(_grade), x_coord(_x), y_coord((double)_grade) {}
};

struct BaricenterComparator {
  const std::map<int, double> &baricenter_values;
  BaricenterComparator(const std::map<int, double> &values)
      : baricenter_values(values) {}
  bool operator()(int a_id, int b_id) const {
    return baricenter_values.at(a_id) < baricenter_values.at(b_id);
  }
};

// ============================================================================
// 1. GRADE CALCULATION (Y-AXIS)
// ============================================================================

// [[Rcpp::export]]
IntegerVector calculate_grades_rcpp(const IntegerVector &concept_ids,
                                    const IntegerVector &edge_from,
                                    const IntegerVector &edge_to) {
  int N = concept_ids.size();
  if (N == 0)
    return IntegerVector::create();

  std::map<int, Node> nodes_map;
  std::map<int, int> in_degree;

  for (int id : concept_ids) {
    nodes_map[id] = Node(id);
    in_degree[id] = 0;
  }

  int E = edge_from.size();
  for (int i = 0; i < E; ++i) {
    int u_id = edge_from[i];
    int v_id = edge_to[i];
    if (u_id == v_id)
      continue;
    if (nodes_map.count(u_id) && nodes_map.count(v_id)) {
      nodes_map.at(u_id).successors.push_back(v_id);
      nodes_map.at(v_id).predecessors.push_back(u_id);
      in_degree.at(v_id)++;
    }
  }

  std::queue<int> q;
  int processed_count = 0;

  for (int id : concept_ids) {
    if (in_degree.at(id) == 0) {
      nodes_map.at(id).grade = 0;
      q.push(id);
    }
  }

  while (!q.empty()) {
    int u_id = q.front();
    q.pop();
    processed_count++;

    int u_grade = nodes_map.at(u_id).grade;

    for (int v_id : nodes_map.at(u_id).successors) {
      if (u_grade + 1 > nodes_map.at(v_id).grade) {
        nodes_map.at(v_id).grade = u_grade + 1;
      }
      in_degree.at(v_id)--;
      if (in_degree.at(v_id) == 0)
        q.push(v_id);
    }
  }

  if (processed_count < N) {
    for (int id : concept_ids) {
      if (nodes_map.at(id).grade == -1)
        nodes_map.at(id).grade = 0;
    }
  }

  IntegerVector calculated_grades(N);
  for (int i = 0; i < N; ++i) {
    calculated_grades[i] = nodes_map.at(concept_ids[i]).grade;
  }
  return calculated_grades;
}

// ============================================================================
// 2. LAYOUT ALGORITHMS
// ============================================================================

// --- A. IMPROVED SUGIYAMA FRAMEWORK (Centered) ---
// Based on: Sugiyama, K., Tagawa, S., & Toda, M. (1981). Methods for visual
// understanding of hierarchical system structures. This implementation focuses
// on minimizing crossings by barycenter ordering and centering nodes.
void apply_sugiyama_layout(std::map<int, Node> &nodes_map,
                           const IntegerVector &concept_ids, int max_grade) {

  // 1. Layering
  std::vector<std::vector<int>> layers(max_grade + 1);
  for (int id : concept_ids) {
    int g = nodes_map.at(id).grade;
    if (g < 0)
      g = 0;
    if (g > max_grade)
      g = max_grade;
    layers[g].push_back(id);
  }

  // Initial Centering
  // Place each node at its index, but then subtract half the width to center
  // the layer around 0
  for (auto &layer : layers) {
    double width = layer.size();
    for (size_t i = 0; i < width; ++i) {
      // Example width 3: Indices 0, 1, 2 -> Coords -1, 0, 1
      nodes_map.at(layer[i]).x_coord = (double)i - (width - 1.0) / 2.0;
    }
  }

  // 2. Crossing Minimization + Vertical Alignment (Barycenter Method)
  int iterations = 20;

  for (int iter = 0; iter < iterations; ++iter) {

    // Top-Down (Improve X positions based on parents/predecessors)
    for (int k = 1; k <= max_grade; ++k) {
      std::vector<int> &current_layer = layers[k];
      if (current_layer.empty())
        continue;

      std::map<int, double> baricenters;
      for (int u_id : current_layer) {
        double sum_x = 0.0;
        int count = 0;
        for (int v_id : nodes_map.at(u_id).predecessors) {
          sum_x += nodes_map.at(v_id).x_coord;
          count++;
        }
        // If it has parents, try to place below their average.
        // If not, stay put (to maintain initial centering).
        baricenters[u_id] =
            (count > 0) ? sum_x / count : nodes_map.at(u_id).x_coord;
      }

      // Sort layer by these ideal positions
      std::sort(current_layer.begin(), current_layer.end(),
                BaricenterComparator(baricenters));

      // CENTERED REASSIGNMENT:
      // Once sorted to minimize crossings, distribute them uniformly centered
      // on 0-axis, to avoid bunching up.
      double width = current_layer.size();
      for (size_t i = 0; i < width; ++i) {
        // We use a mix between rigid grid position (to strictly avoid overlap)
        // and barycentric position (for visual alignment).
        // Here we stick to rigid positioning to guarantee cleanliness (like
        // hasseDiagram).
        nodes_map.at(current_layer[i]).x_coord =
            (double)i - (width - 1.0) / 2.0;
      }
    }

    // Bottom-Up (Improve X positions based on children/successors)
    for (int k = max_grade - 1; k >= 0; --k) {
      std::vector<int> &current_layer = layers[k];
      if (current_layer.empty())
        continue;

      std::map<int, double> baricenters;
      for (int u_id : current_layer) {
        double sum_x = 0.0;
        int count = 0;
        for (int v_id : nodes_map.at(u_id).successors) {
          sum_x += nodes_map.at(v_id).x_coord;
          count++;
        }
        baricenters[u_id] =
            (count > 0) ? sum_x / count : nodes_map.at(u_id).x_coord;
      }

      std::sort(current_layer.begin(), current_layer.end(),
                BaricenterComparator(baricenters));

      double width = current_layer.size();
      for (size_t i = 0; i < width; ++i) {
        nodes_map.at(current_layer[i]).x_coord =
            (double)i - (width - 1.0) / 2.0;
      }
    }
  }

  // Finalize Y coordinates
  for (auto &pair : nodes_map) {
    pair.second.y_coord = (double)pair.second.grade;
  }
}

// --- B. FORCE-DIRECTED (Organic) ---
// Implementation of Fruchterman-Reingold algorithm.
// Fruchterman, T. M., & Reingold, E. M. (1991). Graph drawing by force-directed
// placement.
void apply_force_directed_layout(std::map<int, Node> &nodes_map,
                                 const IntegerVector &concept_ids) {
  int N = concept_ids.size();
  double area = N * N;
  double width = std::sqrt(area);
  double height = std::sqrt(area);
  double k = std::sqrt(area / (double)N);
  double temperature = width / 10.0;
  int iterations = 100;

  RNGScope scope;
  NumericVector rand_x = runif(N, -width / 2.0, width / 2.0);
  NumericVector rand_y = runif(N, -height / 2.0, height / 2.0);

  for (int i = 0; i < N; ++i) {
    nodes_map.at(concept_ids[i]).x_coord = rand_x[i];
    nodes_map.at(concept_ids[i]).y_coord = rand_y[i];
  }

  for (int iter = 0; iter < iterations; ++iter) {
    for (int i = 0; i < N; ++i) {
      Node &v = nodes_map.at(concept_ids[i]);
      v.dx = 0;
      v.dy = 0;
      for (int j = 0; j < N; ++j) {
        if (i == j)
          continue;
        Node &u = nodes_map.at(concept_ids[j]);
        double delta_x = v.x_coord - u.x_coord;
        double delta_y = v.y_coord - u.y_coord;
        double dist = std::sqrt(delta_x * delta_x + delta_y * delta_y);
        if (dist < 0.0001)
          dist = 0.0001;
        double force = (k * k) / dist;
        v.dx += (delta_x / dist) * force;
        v.dy += (delta_y / dist) * force;
      }
    }
    for (int i = 0; i < N; ++i) {
      Node &v = nodes_map.at(concept_ids[i]);
      for (int succ_id : v.successors) {
        Node &u = nodes_map.at(succ_id);
        double delta_x = v.x_coord - u.x_coord;
        double delta_y = v.y_coord - u.y_coord;
        double dist = std::sqrt(delta_x * delta_x + delta_y * delta_y);
        if (dist < 0.0001)
          dist = 0.0001;
        double force = (dist * dist) / k;
        double dx = (delta_x / dist) * force;
        double dy = (delta_y / dist) * force;
        v.dx -= dx;
        v.dy -= dy;
        u.dx += dx;
        u.dy += dy;
      }
    }
    for (int i = 0; i < N; ++i) {
      Node &v = nodes_map.at(concept_ids[i]);
      double d = std::sqrt(v.dx * v.dx + v.dy * v.dy);
      if (d < 0.0001)
        continue;
      double limit = std::min(d, temperature);
      v.x_coord += (v.dx / d) * limit;
      v.y_coord += (v.dy / d) * limit;
    }
    temperature *= 0.95;
  }
}

// ============================================================================
// 3. MAIN DISPATCHER
// ============================================================================

// [[Rcpp::export]]
DataFrame calculate_lattice_layout_rcpp(const IntegerVector &concept_ids,
                                        const IntegerVector &grades,
                                        const IntegerVector &edge_from,
                                        const IntegerVector &edge_to,
                                        const std::string &method) {
  int N = concept_ids.size();
  if (N == 0) {
    return DataFrame::create(_["id"] = IntegerVector::create(),
                             _["x"] = NumericVector::create(),
                             _["y"] = NumericVector::create());
  }

  std::map<int, Node> nodes_map;
  int max_grade = 0;

  for (int i = 0; i < N; ++i) {
    int id = concept_ids[i];
    int g = grades[i];
    if (g < 0)
      g = 0;
    nodes_map[id] = Node(id, g, 0.0);
    if (g > max_grade)
      max_grade = g;
  }

  int E = edge_from.size();
  for (int i = 0; i < E; ++i) {
    int u_id = edge_from[i];
    int v_id = edge_to[i];
    if (u_id == v_id)
      continue;
    if (nodes_map.count(u_id) && nodes_map.count(v_id)) {
      nodes_map.at(v_id).predecessors.push_back(u_id);
      nodes_map.at(u_id).successors.push_back(v_id);
    }
  }

  if (method == "force") {
    apply_force_directed_layout(nodes_map, concept_ids);
  } else {
    // Sugiyama mejorado (centrado)
    apply_sugiyama_layout(nodes_map, concept_ids, max_grade);
  }

  NumericVector x_coords(N);
  NumericVector y_coords(N);

  for (int i = 0; i < N; ++i) {
    x_coords[i] = nodes_map.at(concept_ids[i]).x_coord;
    y_coords[i] = nodes_map.at(concept_ids[i]).y_coord;
  }

  return DataFrame::create(_["id"] = concept_ids, _["x"] = x_coords,
                           _["y"] = y_coords);
}
