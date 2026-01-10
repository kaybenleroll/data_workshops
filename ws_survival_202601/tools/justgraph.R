#!/usr/bin/env Rscript

#' justgraph - Justfile Dependency Visualization Tool
#'
#' Parses Justfiles and creates dependency visualizations in multiple formats,
#' similar to makefile2graph but for Just command runner.
#'
#' @usage
#' Rscript tools/justgraph.R [options] [justfile]
#'
#' @examples
#' # Generate PNG from default Justfile
#' Rscript tools/justgraph.R --format png -o deps.png
#'
#' # Generate Mermaid with private targets
#' Rscript tools/justgraph.R --format mermaid --show-private -o deps.mmd
#'
#' # Multiple formats
#' Rscript tools/justgraph.R --format dot,png,svg -o deps
#'
#' @author Mick Cooney <mickcooney@gmail.com>

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(jsonlite)
  library(glue)
  library(argparse)
  })


# =============================================================================
# PARSING FUNCTIONS
# =============================================================================

#' Parse Justfile and Extract Targets with Dependencies
#'
#' Parses a Justfile to extract all targets, their dependencies, parameters,
#' and identifies private targets (prefixed with _).
#'
#' @param justfile_path Character: Path to the Justfile
#'
#' @return Tibble with columns:
#'   - target: Target name
#'   - dependencies: List column of dependency names
#'   - parameters: List column of parameter names
#'   - is_private: Logical, TRUE if target starts with _
#'
#' @examples
#' targets <- parse_justfile("Justfile")
#'
parse_justfile <- function(justfile_path) {

  # Validate file exists
  if (!file.exists(justfile_path)) {
    stop(glue("Justfile not found: {justfile_path}"))
    }

  # Read file
  lines <- read_lines(justfile_path, lazy = FALSE)

  # Storage
  targets_lst <- list()
  variables_lst <- list()

  # Parsing state
  in_recipe <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Skip empty lines and comments
    if (str_detect(line, "^\\s*$") || str_detect(line, "^\\s*#")) {
      next
      }

    # Recipe line (starts with tab or spaces after target)
    if (str_detect(line, "^\\t") || (in_recipe && str_detect(line, "^\\s{2,}"))) {
      in_recipe <- TRUE
      next
      }

    # Variable assignment (UPPERCASE := value)
    if (str_detect(line, "^[A-Z_][A-Z0-9_]*\\s*:=")) {
      var_match <- str_match(line, "^([A-Z_][A-Z0-9_]*)\\s*:=\\s*(.+)$")
      if (!is.na(var_match[1, 2])) {
        variables_lst[[var_match[1, 2]]] <- str_trim(var_match[1, 3])
        }
      in_recipe <- FALSE
      next
      }

    # Target definition: name [params]: [deps]
    target_pattern <- "^(_?[a-z][a-z0-9_-]*)(.*):\\s*(.*)$"
    if (str_detect(line, target_pattern)) {
      in_recipe <- FALSE

      target_match <- str_match(line, target_pattern)
      target_name <- target_match[1, 2]
      middle_part <- str_trim(target_match[1, 3])
      deps_part <- str_trim(target_match[1, 4])

      # Extract parameters (words between target name and colon)
      params <- if (nchar(middle_part) > 0) {
        str_split(middle_part, "\\s+")[[1]]
        } else {
        character(0)
        }

      # Extract dependencies (words after colon)
      deps <- if (nchar(deps_part) > 0) {
        str_split(deps_part, "\\s+")[[1]]
        } else {
        character(0)
        }

      # Store target
      targets_lst[[target_name]] <- tibble(
        target = target_name,
        dependencies = list(deps),
        parameters = list(params),
        is_private = str_starts(target_name, "_")
        )
      }
    }

  # Convert to tibble
  if (length(targets_lst) == 0) {
    return(
      tibble(
        target = character(0),
        dependencies = list(),
        parameters = list(),
        is_private = logical(0)
        )
      )
    }

  targets_tbl <- bind_rows(targets_lst)

  return(targets_tbl)
}


# =============================================================================
# GRAPH CONSTRUCTION
# =============================================================================

#' Build igraph Object from Parsed Targets
#'
#' Constructs a directed acyclic graph (DAG) from parsed Justfile targets,
#' where edges represent dependencies (from dependency to target).
#'
#' @param targets_tbl Tibble from parse_justfile()
#' @param show_private Logical: Include private targets (default: FALSE)
#'
#' @return igraph directed graph object with vertex attributes
#'
#' @examples
#' targets <- parse_justfile("Justfile")
#' graph <- build_dependency_graph(targets, show_private = TRUE)
#'
build_dependency_graph <- function(targets_tbl, show_private = FALSE) {

  # Filter private targets if requested
  if (!show_private) {
    targets_tbl <- targets_tbl |>
      filter(!is_private)
    }

  # Check for empty result
  if (nrow(targets_tbl) == 0) {
    return(make_empty_graph(directed = TRUE))
    }

  # Build edge list from dependencies
  edges_tbl <- targets_tbl |>
    select(target, dependencies) |>
    unnest(dependencies) |>
    filter(dependencies != "") |>
    rename(from = dependencies, to = target)  # dep -> target

  # Filter edges to only include known targets
  valid_targets <- targets_tbl$target
  edges_tbl <- edges_tbl |>
    filter(from %in% valid_targets, to %in% valid_targets)

  # Create vertex attributes
  vertices_tbl <- targets_tbl |>
    select(name = target, is_private) |>
    mutate(
      label = name,
      color = if_else(is_private, "gray80", "lightblue"),
      shape = "rectangle"
      )

  # Handle case with no edges
  if (nrow(edges_tbl) == 0) {
    # Graph with vertices but no edges
    g <- graph_from_data_frame(
      d = data.frame(from = character(0), to = character(0)),
      directed = TRUE,
      vertices = vertices_tbl
      )
    } else {
    # Normal case with edges
    g <- graph_from_data_frame(
      d = edges_tbl,
      directed = TRUE,
      vertices = vertices_tbl
      )
    }

  return(g)
}


# =============================================================================
# EXPORT FUNCTIONS - GRAPH FORMATS
# =============================================================================

#' Export Graph to DOT Format (Graphviz)
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#'
export_dot <- function(graph, output_path) {
  write_graph(graph, output_path, format = "dot")
}


#' Export Graph to GraphML Format
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#'
export_graphml <- function(graph, output_path) {
  write_graph(graph, output_path, format = "graphml")
}


#' Export Graph to JSON Format
#'
#' Custom JSON format with nodes and edges arrays for web applications.
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#'
export_json <- function(graph, output_path) {

  # Extract nodes
  nodes <- V(graph)$name
  node_list <- map(nodes, ~list(id = .x, label = .x))

  # Extract edges
  edges_df <- as_data_frame(graph, what = "edges")

  if (nrow(edges_df) > 0) {
    edge_list <- map2(
      edges_df$from,
      edges_df$to,
      ~list(source = .x, target = .y)
      )
    } else {
    edge_list <- list()
    }

  # Build JSON structure
  json_data <- list(
    nodes = node_list,
    edges = edge_list
    )

  # Write to file
  write_json(json_data, output_path, pretty = TRUE, auto_unbox = TRUE)
}


#' Export Graph to Mermaid Format
#'
#' Generates Mermaid diagram syntax for embedding in Markdown/documentation.
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#'
export_mermaid <- function(graph, output_path) {

  edges_df <- as_data_frame(graph, what = "edges")

  if (nrow(edges_df) > 0) {
    edge_lines <- map2_chr(
      edges_df$from,
      edges_df$to,
      ~paste0("    ", .x, " --> ", .y)
      )
    } else {
    edge_lines <- character(0)
    }

  mermaid_lines <- c(
    "graph TD",
    edge_lines
    )

  write_lines(mermaid_lines, output_path)
}


#' Export Graph to PlantUML Format
#'
#' Generates PlantUML diagram syntax.
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#'
export_plantuml <- function(graph, output_path) {

  edges_df <- as_data_frame(graph, what = "edges")

  if (nrow(edges_df) > 0) {
    edge_lines <- map2_chr(
      edges_df$from,
      edges_df$to,
      ~paste0(.x, " --> ", .y)
      )
    } else {
    edge_lines <- character(0)
    }

  plantuml_lines <- c(
    "@startuml",
    edge_lines,
    "@enduml"
    )

  write_lines(plantuml_lines, output_path)
}


# =============================================================================
# EXPORT FUNCTIONS - IMAGE FORMATS
# =============================================================================

#' Plot Graph with Hierarchical Layout
#'
#' Internal function to plot igraph with Sugiyama (hierarchical) layout,
#' suitable for DAGs.
#'
#' @param graph igraph object
#'
plot_graph <- function(graph) {

  # Check for empty graph
  if (vcount(graph) == 0) {
    plot.new()
    text(0.5, 0.5, "No targets to display", cex = 1.5)
    return(invisible())
    }

  # Use Sugiyama layout for hierarchical DAG visualization
  tryCatch(
    {
      layout_result <- layout_with_sugiyama(graph)
      layout_matrix <- layout_result$layout
      },
    error = function(e) {
      # Fallback to simple layout if Sugiyama fails
      layout_matrix <- layout_nicely(graph)
      }
    )

  # Plot
  plot(
    graph,
    layout = layout_matrix,
    vertex.label = V(graph)$name,
    vertex.shape = "rectangle",
    vertex.size = 30,
    vertex.size2 = 15,
    vertex.label.cex = 0.8,
    vertex.label.color = "black",
    vertex.color = V(graph)$color,
    vertex.frame.color = "gray40",
    edge.arrow.size = 0.5,
    edge.arrow.width = 1.5,
    edge.color = "gray40",
    edge.curved = 0.1,
    main = "Justfile Target Dependencies",
    margin = c(0, 0, 0, 0)
    )
}


#' Export Graph to PNG Image
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#' @param width Numeric: Width in pixels (default: 1200)
#' @param height Numeric: Height in pixels (default: 800)
#'
export_png <- function(graph, output_path, width = 1200, height = 800) {
  png(output_path, width = width, height = height, res = 150)
  plot_graph(graph)
  dev.off()
}


#' Export Graph to SVG Image
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#' @param width Numeric: Width in inches (default: 12)
#' @param height Numeric: Height in inches (default: 8)
#'
export_svg <- function(graph, output_path, width = 12, height = 8) {
  svg(output_path, width = width, height = height)
  plot_graph(graph)
  dev.off()
}


#' Export Graph to PDF Image
#'
#' @param graph igraph object
#' @param output_path Character: Output file path
#' @param width Numeric: Width in inches (default: 12)
#' @param height Numeric: Height in inches (default: 8)
#'
export_pdf <- function(graph, output_path, width = 12, height = 8) {
  pdf(output_path, width = width, height = height)
  plot_graph(graph)
  dev.off()
}


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Print Targets Table
#'
#' Displays a formatted table of targets and their dependencies.
#'
#' @param targets_tbl Tibble from parse_justfile()
#' @param show_private Logical: Include private targets
#'
print_targets_table <- function(targets_tbl, show_private = FALSE) {

  # Filter private if requested
  if (!show_private) {
    targets_tbl <- targets_tbl |>
      filter(!is_private)
    }

  # Format dependencies as comma-separated
  display_tbl <- targets_tbl |>
    mutate(
      dependencies_str = map_chr(
        dependencies,
        ~if (length(.x) > 0) paste(.x, collapse = ", ") else "(none)"
        ),
      params_str = map_chr(
        parameters,
        ~if (length(.x) > 0) paste(.x, collapse = ", ") else ""
        )
      ) |>
    select(Target = target, Dependencies = dependencies_str, Parameters = params_str)

  # Print summary
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("JUSTFILE TARGETS SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  cat(glue("Total targets: {nrow(display_tbl)}"), "\n")
  if (!show_private) {
    cat("(Private targets excluded. Use --show-private to include)\n")
    }
  cat("\n")

  # Print table
  print(display_tbl, n = Inf)
}


# =============================================================================
# COMMAND-LINE INTERFACE
# =============================================================================

# Define CLI parser using argparse
parser <- ArgumentParser(
  description = paste(
    "justgraph - Justfile Dependency Visualization Tool\n",
    "Parses Justfiles and creates dependency visualizations."
  ),
  epilog = paste(
    sep = "\n",
    "",
    "EXAMPLES:",
    "  Rscript tools/justgraph.R --format png -o deps.png",
    "  Rscript tools/justgraph.R --format mermaid --show-private -o deps.mmd",
    "  Rscript tools/justgraph.R --format dot,png,svg -o deps Justfile"
  )
)

parser$add_argument(
  "-f", "--format",
  type = "character",
  default = "png",
  help = paste(
    "Output format(s), comma-separated.",
    "Graph formats: dot, json, graphml, plantuml, mermaid;",
    "Image formats: png, svg, pdf [default: %(default)s]"
  )
)

parser$add_argument(
  "-o", "--output",
  type = "character",
  default = "justfile_deps",
  help = "Output file path (without extension for multiple formats) [default: %(default)s]"
)

parser$add_argument(
  "--show-private",
  action = "store_true",
  default = FALSE,
  help = "Include private targets (prefixed with _) [default: %(default)s]"
)

parser$add_argument(
  "--width",
  type = "integer",
  default = 1200,
  help = "Image width in pixels (for PNG) [default: %(default)s]"
)

parser$add_argument(
  "--height",
  type = "integer",
  default = 800,
  help = "Image height in pixels (for PNG) [default: %(default)s]"
)

parser$add_argument(
  "-l", "--list-targets",
  action = "store_true",
  default = FALSE,
  help = "List targets and dependencies, then exit"
)

parser$add_argument(
  "-v", "--verbose",
  action = "store_true",
  default = FALSE,
  help = "Print verbose output"
)

parser$add_argument(
  "justfile",
  nargs = "?",
  default = "Justfile",
  help = "Path to Justfile [default: Justfile]"
)


# =============================================================================
# MAIN FUNCTION
# =============================================================================

main <- function() {

  # Parse arguments
  args <- parser$parse_args()
  justfile_path <- args$justfile

  # Validate Justfile exists
  if (!file.exists(justfile_path)) {
    stop(glue("Error: Justfile not found: {justfile_path}"))
    }

  # Parse Justfile
  if (isTRUE(args$verbose)) message("Parsing Justfile: ", justfile_path)
  targets_tbl <- parse_justfile(justfile_path)

  if (nrow(targets_tbl) == 0) {
    stop("Error: No targets found in Justfile")
    }

  if (isTRUE(args$verbose)) {
    message(glue("Found {nrow(targets_tbl)} targets"))
    }

  # Handle --list-targets
  if (isTRUE(args$list_targets)) {
    print_targets_table(targets_tbl, isTRUE(args$show_private))
    quit(status = 0)
    }

  # Build graph
  if (isTRUE(args$verbose)) message("Building dependency graph...")
  graph <- build_dependency_graph(targets_tbl, isTRUE(args$show_private))

  # Check for empty graph
  if (vcount(graph) == 0) {
    stop("Error: No targets to visualize (try --show-private?)")
    }

  if (isTRUE(args$verbose)) {
    message(glue("Graph: {vcount(graph)} nodes, {ecount(graph)} edges"))
    }

  # Parse output formats
  formats <- str_split(args$format, ",")[[1]] |> str_trim()

  # Export each format
  for (fmt in formats) {
    # Determine output filename
    output_file <- if (length(formats) > 1 || !str_detect(args$output, "\\.")) {
      paste0(args$output, ".", fmt)
      } else {
      args$output
      }

    if (isTRUE(args$verbose)) message("Exporting to ", fmt, ": ", output_file)

    # Export
    tryCatch(
      {
        switch(
          fmt,
          "dot"      = export_dot(graph, output_file),
          "graphml"  = export_graphml(graph, output_file),
          "json"     = export_json(graph, output_file),
          "mermaid"  = export_mermaid(graph, output_file),
          "plantuml" = export_plantuml(graph, output_file),
          "png"      = export_png(graph, output_file, args$width, args$height),
          "svg"      = export_svg(graph, output_file),
          "pdf"      = export_pdf(graph, output_file),
          stop(glue("Unknown format: {fmt}"))
          )

        message("Created: ", output_file)
        },
      error = function(e) {
        stop(glue("Error exporting {fmt}: {e$message}"))
        }
      )
    }

  if (isTRUE(args$verbose)) message("Done!")
}


# =============================================================================
# EXECUTION
# =============================================================================

# Run main function
main()
