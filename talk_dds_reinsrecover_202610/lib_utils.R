resolve_conflicts <- function(pkg_priority) {
  get_index <- function(pkg_name) {
    idx <- str_which(pkg_priority, pkg_name)

    if(length(idx) == 0) {
      idx <- 0L
    }

    return(idx)
  }

  conflict_lst <- conflict_scout()

  for(func_name in names(conflict_lst)) {
    pkg_index <- map_int(conflict_lst[[func_name]], get_index)

    pkg_index <- pkg_index[pkg_index > 0]

    if(length(pkg_index) == 0) {
      pkg_use <- conflict_lst[[func_name]][1]
    } else {
      pkg_use <- pkg_index %>%
        min() %>%
        pkg_priority[.]

    }

    conflict_prefer(func_name, pkg_use)
  }

  return(conflict_lst)
}


# Inuring DAG helpers: build a tbl_graph from nodes/edges tables, lay it out one component
# at a time (layered, top to bottom) and pack the components into rows, then plot.
build_inuring_tblgraph <- function(nodes_tbl, edges_tbl) {
  tbl_graph(
    nodes    = nodes_tbl,
    edges    = edges_tbl,
    node_key = "treaty_id",
    directed = TRUE
  )
}

layout_packed_components <- function(graph, max_width = 13, gap = 1.5) {
  ig   <- igraph::as.igraph(graph)
  memb <- igraph::components(ig, mode = "weak")$membership

  part_lst <- unique(memb) |>
    map(\(k) {
      ids <- which(memb == k)
      lay <- igraph::layout_with_sugiyama(igraph::induced_subgraph(ig, ids))$layout

      list(
        ids = ids,
        x   = lay[, 1] - min(lay[, 1]),
        y   = max(lay[, 2]) - lay[, 2]
      )
    })

  part_lst <- part_lst[order(-map_int(part_lst, \(p) length(p$ids)))]

  layout_mat <- matrix(0, nrow = igraph::vcount(ig), ncol = 2)
  x_cur <- 0
  y_top <- 0
  row_h <- 0

  for(p in part_lst) {
    width  <- max(p$x)
    height <- max(p$y)

    if(x_cur > 0 && x_cur + width > max_width) {
      x_cur <- 0
      y_top <- y_top + row_h + gap
      row_h <- 0
    }

    layout_mat[p$ids, 1] <- x_cur + p$x
    layout_mat[p$ids, 2] <- -(y_top + p$y)

    x_cur <- x_cur + width + gap
    row_h <- max(row_h, height)
  }

  layout_mat
}

# Force-directed (Fruchterman-Reingold) layout with a fixed seed, so the figure is reproducible.
layout_fr_seeded <- function(graph, seed) {
  set.seed(seed)

  igraph::layout_with_fr(graph)
}

plot_inuring_dag <- function(graph, layout, label_col = "treaty_id", label_size = 3) {
  ggraph(graph, layout = layout) +
    geom_edge_link(
      arrow   = arrow(angle = 14, length = unit(2.5, "mm"), type = "closed"),
      end_cap = circle(4, "mm"),
      colour  = "darkgrey"
    ) +
    geom_node_label(
      aes(label = .data[[label_col]], fill = treaty_type),
      size = label_size,
      show.legend = FALSE
    ) +
    theme_graph()
}


# Print the source of a function as a fenced code block (for results: asis chunks).
# Needs the file to have been sourced with keep.source = TRUE. Optionally elides the lines
# from the first line matching `skip_from` to the first one matching `skip_to` inclusive.
show_function_source <- function(fn_name, skip_from = NULL, skip_to = NULL,
                                 skip_note = "  # ...", keep_from = NULL, keep_to = NULL) {
  src_lines <- deparse(get(fn_name), control = "useSource")
  src_lines[1] <- paste0(fn_name, " <- ", src_lines[1])

  if(!is.null(skip_from)) {
    from_idx <- str_which(src_lines, fixed(skip_from))[1]
    to_idx   <- str_which(src_lines, fixed(skip_to))[1]

    src_lines <- c(
      src_lines[seq_len(from_idx - 1)],
      skip_note,
      src_lines[-seq_len(to_idx)]
    )
  }

  # Optionally keep only the lines from the first one matching `keep_from` to the first one
  # matching `keep_to`, both inclusive (the heading slide then carries the narration).
  if(!is.null(keep_from)) {
    from_idx <- str_which(src_lines, fixed(keep_from))[1]
    to_idx   <- str_which(src_lines, fixed(keep_to))[1]

    src_lines <- src_lines[from_idx:to_idx]
  }

  cat("```r", src_lines, "```", sep = "\n")
}
