# Author: Andrew Disher
# Date: 11/09/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`],
  # simplextree[...],
  threejs[...],
  igraph[...]
)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

nodes <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/simplicial_complex_data/simplex-data-1.csv")
edges <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/simplicial_complex_data/simplex-data-2.csv")
triangles <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/simplicial_complex_data/simplex-data-3.csv")
tetrahedron <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/simplicial_complex_data/simplex-data-4.csv")

ig_object <- graph_from_data_frame(d = edges[, 1:2],
                                   vertices = nodes[, 1]) 

ig_object <- ig_object %>% 
  add_layout_(
    # on_sphere()
    # nicely(dim = 3)
    # with_fr(dim = 3,
    #         weights = edges[, 3])
    with_graphopt(charge = .005,
                  mass = 10)
    )

graphjs(ig_object)

