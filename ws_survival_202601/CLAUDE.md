# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Educational workshop on survival analysis using R and the telco churn dataset. Primary output is rendered HTML workshop materials from Quarto notebooks.

**Key technologies:** R 4.5.1, Quarto, survival/survminer packages, tidyverse, Docker (rocker/tidyverse)

## Common Commands

```bash
# Build and run Docker container
just docker-build-image     # Build Docker image
just docker-run-image       # Start container with RStudio Server
just docker-bash            # Enter container shell

# Render Quarto notebooks
just worksheet              # Render worksheet_survival.qmd
just classic                # Render classic_survival_models.qmd
just all                    # Render all notebooks

# Clean generated files
just clean-html             # Remove HTML files
just clean-cache            # Remove Quarto cache
just nuke                   # Remove all generated files
```

Access RStudio at http://localhost:8787 (user: rstudio, password: CHANGEME)

## Project Structure

```
worksheet_survival.qmd      # Main workshop notebook
classic_survival_models.qmd # Classical survival models notebook
lib_utils.R                 # Shared utility functions
data/telcochurn.csv         # Primary dataset
Justfile                    # Build automation
Dockerfile                  # Container definition
```

## Critical R Code Patterns

### Always specify join relationships (tidyverse 1.1.0+)
```r
left_join(data1, data2, by = "key", relationship = "many-to-one")  # Required
```

### Use native pipe and tidyverse patterns
```r
result <- data_tbl |>
  filter(condition) |>
  mutate(new_col = value)
```

### Naming conventions
- Tibbles: `_tbl` suffix (e.g., `telco_churn_tbl`)
- Plots: `_plot` suffix
- Constants: `UPPER_SNAKE_CASE`
- Functions: `snake_case` with verb-noun pattern

### Closing parentheses indentation
```r
data_tbl |>
  summarise(
    count = n(),
    mean_val = mean(value)
    )  # Closing ) indented to match opening line
```

## Markdown List Formatting (Critical for Quarto)

- Blank line BEFORE lists (separates from paragraph)
- NO blank lines BETWEEN list items
- Two-space indentation for all list items
- Use `1.` for ALL numbered items (auto-numbered)

```markdown
Intro text:

  - Item 1
  - Item 2
```

## Key Files

- `AGENTS.md` - Comprehensive AI agent guidelines with full code style, Docker setup, and patterns
- `lib_utils.R` - Utility functions (all have roxygen2 documentation)

## Dependencies

Core packages: survival, survminer, muhaz, tidyverse, arrow, qs, cowplot, furrr

See AGENTS.md for complete R code style guidelines, Docker configuration details, and troubleshooting.
