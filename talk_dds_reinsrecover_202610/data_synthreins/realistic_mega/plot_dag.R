# plot_dag.R -- deliberately naive full-graph plot of the realistic mega exhibit.
# Default layout, no label tuning: the point is that it does not scale.
#
# Usage: Rscript plot_dag.R <realistic_mega_dir>
# Reads  <dir>/{nodes_tbl,edges_tbl}.csv ; writes <dir>/realistic_mega_dag.png
suppressPackageStartupMessages({
  library(readr); library(tidygraph); library(ggraph); library(ggplot2)
})

build_graph <- function(dir) {
  nodes <- read_csv(file.path(dir, "nodes_tbl.csv"), show_col_types = FALSE, progress = FALSE)
  edges <- read_csv(file.path(dir, "edges_tbl.csv"), show_col_types = FALSE, progress = FALSE)
  tbl_graph(
    nodes    = nodes[, c("treaty_id", "treaty_type")],
    edges    = edges[, c("from_treaty", "to_treaty")],
    directed = TRUE,
    node_key = "treaty_id"
  )
}

plot_graph <- function(g) {
  ggraph(g, layout = "auto") +
    geom_edge_link(arrow = arrow(length = unit(1.5, "mm")), end_cap = circle(2, "mm")) +
    geom_node_point(aes(colour = treaty_type), size = 4) +
    geom_node_text(aes(label = treaty_id), size = 2, repel = FALSE) +
    theme_graph()
}

dir <- commandArgs(trailingOnly = TRUE)[1]
ggsave(file.path(dir, "realistic_mega_dag.png"), plot_graph(build_graph(dir)),
       width = 12, height = 9, dpi = 100)
