# AI Agent Guidelines for Survival Analysis Workshop

This document provides guidance for AI assistants (GitHub Copilot, etc.) working with this codebase.

## 🎯 Quick Context

**What is this?** Educational workshop on survival analysis using R and the telco churn dataset  
**Primary tool:** RStudio Server in Docker (rocker/tidyverse)  
**Key technologies:** R, Quarto (qmd), survival package, survminer, tidyverse  
**Main challenge:** Creating pedagogical content that balances statistical rigor with accessibility  
**Primary output:** Rendered HTML workshop materials (worksheet_survival.html) with embedded visualizations and explanations

## 🚨 Critical Rules - Read These First!

### 1. Threading Environment Variables (IMPORTANT for parallel processing)
**Required for parallel processing with furrr/future to prevent crashes:**

```bash
-e OPENBLAS_NUM_THREADS=1
-e OMP_NUM_THREADS=1  
-e MKL_NUM_THREADS=1
```

**Why:** Linear algebra libraries spawn multiple threads per worker. This compounds in parallel processing and can exhaust system resources, causing "Resource temporarily unavailable" errors.

### 2. Join Relationship Specification (REQUIRED in tidyverse 1.1.0+)
```r
# ✅ CORRECT - Explicit relationship prevents warnings
left_join(data1, data2, by = "key", relationship = "many-to-many")
left_join(data1, data2, by = "key", relationship = "many-to-one")

# ❌ WRONG - Will generate warnings
left_join(data1, data2, by = "key")
```

### 3. Function Documentation Standards
**All library files (`lib_*.R`) must have complete roxygen2 documentation.** Check function signatures in the files before asking about parameters.

### 4. Reproducibility
- **Always set seeds**: Use consistent seed (e.g., `seed = 42`) for reproducible results
- **Document package versions**: Use renv or specify versions in Dockerfile
- **Cache expensive computations**: Save fitted models and large results

### 5. Markdown/Quarto Formatting (CRITICAL for this project)

**List Formatting Rules:**

```markdown
# ✅ CORRECT - Proper list formatting
Some introductory text explaining the list:

  - First item with two-space indentation
  - Second item
  - Multi-line item continues on next line
    with proper indentation (four spaces total)

# ❌ WRONG - Missing blank line before list
Some introductory text:
  - First item (will merge with paragraph above!)

# ❌ WRONG - Blank lines between items
  - First item

  - Second item (creates separate lists!)

# ❌ WRONG - No indentation
- First item (inconsistent with rest of document)
```

**Critical Rules:**
1. **Blank line BEFORE lists**: Always have one blank line between intro text and first list item
2. **NO blank lines BETWEEN items**: List items should be consecutive (no blank lines)
3. **Two-space indentation**: All list items start with two spaces, then the marker (- or 1.)
4. **Continuation lines**: Multi-line list items need proper indentation (4 spaces for continuation)
5. **Auto-numbering**: Use `1.` for ALL numbered list items (Markdown auto-numbers them)

**Why This Matters:**
- Without blank line before: List merges with preceding paragraph
- With blank lines between items: Renders as separate lists
- Without proper indentation: Markdown doesn't recognize as list
- Manual numbering (1. 2. 3.): Fragile when reordering/adding items

## 🐳 Docker/Podman Setup

### Standard Container Launch
```bash
# Use Justfile command (recommended)
just docker-run-image

# Manual command (for reference - uses Docker, not Podman)
docker run --rm -d \
  --userns=keep-id \
  -e RUNROOTLESS=false \
  -p "127.0.0.1:8787:8787" \
  -e USER=rstudio \
  -e PASSWORD=CHANGEME \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -v "$(pwd):/home/rstudio/surv_workshop:z" \
  -v "$(pwd)/.rstudio_copilot:/home/rstudio/.config/github-copilot:rw" \
  --name survival-workshop \
  kaybenleroll/ws_survival_202601:latest
```

### Key Configuration Notes
- **RUNROOTLESS=false** - rocker images need root for initial setup
- **Port binding to 127.0.0.1** - Never use 0.0.0.0 (security risk)
- **SELinux :z suffix** - Required for volume mounts on SELinux systems (lowercase :z for shared)
- **--userns=keep-id** - Maintains user ID mapping in rootless mode
- **--rm flag** - Auto-removes container when stopped
- **GitHub Copilot mount** - Persists authentication across container restarts
- **Project mount** - Maps to /home/rstudio/surv_workshop (not /project)

### Docker Image Management
```bash
# Build Docker image
just docker-build-image

# Rebuild from scratch (no cache)
just docker-rebuild-image

# Stop container
just docker-stop-image

# Remove container
just docker-rm

# Restart container
just docker-restart

# Enter container shell
just docker-bash

# View container logs
just docker-logs

# Check container status
just docker-status
```

### SSH Tunneling for Remote Access
```bash
# Display tunnel command with your username and hostname
just ssh-tunnel

# Example output:
# Run this command on your local machine:
#   ssh -L 8787:localhost:8787 username@hostname
# Then access RStudio at: http://localhost:8787
```

### Quarto Rendering Targets
```bash
# Render main workshop notebook
just worksheet_survival
# or
just worksheet

# Render classical survival models
just classic_survival_models
# or
just classic

# Render all notebooks
just all

# Clean generated files
just clean-html        # Remove HTML files
just clean-cache       # Remove Quarto cache
just clean-all         # Remove everything
just nuke             # Nuclear option
```

## 📊 Project Structure

```
project_name/
├── data/                           # Input data files (often read-only)
│   ├── raw/                       # Original source data
│   └── reference/                 # Reference tables/lookups
├── output/                         # Processing results / analysis outputs
├── models/                         # Saved model objects (Optional - for modeling projects)
│   └── fitted_models/             # Cached fitted models
├── stan_code/                      # Stan model definitions (Optional - for Bayesian projects)
├── build/                          # Docker build configurations
│   ├── Dockerfile
│   └── install_packages.R
├── lib_*.R                         # Shared library functions (ALL documented with roxygen2!)
├── *.qmd                          # Quarto analysis notebooks (Optional - for analysis projects)
├── scripts/                        # Processing scripts (Optional - for ETL projects)
├── chunk_timings/                  # Notebook execution timings (Optional - for analysis projects)
├── Justfile                        # Task automation
├── .env                           # Environment variables (gitignored)
└── README.md                      # Project documentation
```

## 🛠️ R Code Style

### Pipe Operators
```r
# ✅ Use native pipe |> for most operations
result <- data |>
  filter(condition) |>
  mutate(new_col = value) |>
  select(columns)

# ✅ Use magrittr %>% ONLY when you need (.) functionality
data %>% set_colnames(names(.) |> to_snake_case())

# Why? Native |> is faster and built into R 4.1+
```

### Naming Conventions
```r
# Functions: snake_case with verb-noun pattern
calculate_metric <- function() {...}
extract_features <- function() {...}

# Variables: snake_case with type suffix
customer_data_tbl    # tibble
order_df             # data frame  
account_ids_vec      # vector
config_lst           # list

# Plots: _plot suffix
distribution_plot <- ggplot(...) + ...
survival_plot <- ggplot(...) + ...

# Models: descriptive_name_modeltype
baseline_lm          # linear model
complex_brmsfit      # brms Bayesian model
model1_coxph         # Cox proportional hazards

# Shared vs Model-Specific objects:
# NO prefix for shared objects: training_tbl, validation_tbl, lookup_tbl
# modelN_ prefix for model-specific: model1_fit, model2_predictions

# Constants: UPPER_SNAKE_CASE
MAX_RETRIES <- 5
BATCH_SIZE <- 1000
RANDOM_SEED <- 4000

# Temporary: temp_ prefix
temp_calculated_age <- year(birthdate) - current_year
```

### Column Name Standardization
```r
# ✅ ALWAYS convert to snake_case immediately after reading
data <- read_excel("file.xlsx") |>
  set_colnames(names(.) |> to_snake_case())

# Standard conventions:
# - All lowercase: customer_id (not CustomerID)
# - No spaces: product_name (not "Product Name")
# - Underscores for clarity: date_created, is_active
# - Minimal abbreviations: email (not eml), category (not cat)
# - Boolean prefix: is_, has_, should_, can_
```

### Joins (Always Specify Relationship!)
```r
# ✅ CORRECT - Explicit relationship prevents warnings
left_join(data1, data2, by = "key", relationship = "many-to-many")
left_join(data1, data2, by = "key", relationship = "many-to-one")
left_join(data1, data2, by = "key", relationship = "one-to-one")

# ❌ WRONG - Generates warnings in tidyverse >= 1.1.0
left_join(data1, data2, by = "key")
```

### Avoid Base R Shortcuts - Use Tidyverse Pipelines
```r
# ❌ BAD - Using $ accessor
sum(data$column == value)
mean(data$column)

# ✅ GOOD - Tidyverse pipeline
data |> filter(column == value) |> nrow()
data |> pull(column) |> mean(na.rm = TRUE)
```

### Use `summarise()` for Aggregations
```r
# ✅ Group multiple statistics in a single summarise() block
summary_tbl <- data_tbl |>
  summarise(
    count = n(),
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE)
    )
```

### Parentheses Indentation (CRITICAL for readability)
```r
# Function arguments indented 2 spaces from opening parenthesis
# Closing ) on own line at same indentation as function call

# ❌ BAD - Wrong indentation
data_tbl |>
  select(
    id, value
  )

# ✅ GOOD - Proper indentation
data_tbl |>
  select(
    id, value
    )

# For ggplot - same rule for each layer
ggplot(data_tbl) +
  geom_point(
    aes(x = var1, y = var2, color = group),
    alpha = 0.5,
    size  = 2
    ) +
  labs(
    title = "Plot Title",
    x     = "X Label",
    y     = "Y Label"
    )
```

### Command Line Arguments (Optional - for scripts)
```r
# Use argparse package for all R scripts
library(argparse)

parser <- ArgumentParser(
  description = "Script description",
  formatter_class = "argparse.RawDescriptionHelpFormatter",
  epilog = paste(
    sep = "\n",
    "ENVIRONMENT VARIABLES:",
    "  VAR_NAME    Description of variable",
    "",
    "EXAMPLES:",
    "  Rscript script.R --option value"
    )
  )

parser$add_argument("--option", dest = "option_name", help = "Description")
args <- parser$parse_args()

# Access with underscores (not dashes)
value <- args$option_name
```

### Logging Standards
```r
# Prefer structured logging over print()
write_log_entry <- function(section, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(glue("[{timestamp}] [{section}] {message}\n"))
}

# Usage
write_log_entry("STARTUP", "Starting data processing")
write_log_entry("PROCESSING", glue("Processed {nrow(data)} rows"))
write_log_entry("COMPLETE", "Processing finished successfully")
```

### Output Formatting in Quarto (Optional - for notebooks)
```r
# Use write_lines() from readr - works regardless of chunk options
write_lines("Summary Statistics:", stdout())

# With glue for formatting
summary_tbl <- data_tbl |>
  summarise(
    count = n(),
    mean_val = mean(value, na.rm = TRUE)
    )

write_lines(glue(
  "Summary Statistics:
    Count: {summary_tbl$count}
    Mean: {format(summary_tbl$mean_val, digits = 4)}"
  ), stdout())
```

## 📚 Data Persistence

### File Formats
```r
# Use parquet for tibbles (fast, portable, compressed)
data_tbl |> write_parquet_compressed("output/data.parquet")

# Use qs for complex objects (2-10x faster than RDS)
model_results_lst |> qsave("models/results.qs", preset = "balanced")

# Helper function in lib_utils.R
write_parquet_compressed <- function(data, path) {
  arrow::write_parquet(data, path, compression = "zstd", compression_level = 3)
}
```

### Always Use Pipe Notation
```r
# ✅ GOOD
object_tbl |> write_parquet_compressed(path)
object_lst |> qsave(path, preset = "balanced")

# ❌ BAD
write_parquet(object_tbl, path)
```

### Manual Caching Pattern
```r
# For expensive operations (models, large computations)
cache_file <- "models/model1_results.qs"

if (file_exists(cache_file)) {
  model1_results <- qread(cache_file)
} else {
  model1_results <- expensive_computation(...)
  model1_results |> qsave(cache_file, preset = "balanced")
}
```

### Save Before Remove
```r
# Save large objects before clearing from memory
large_tbl |> write_parquet_compressed("output/large_tbl.parquet")
rm(large_tbl)
gc()  # Trigger garbage collection
```

## 📚 Library File Organization

### lib_utils.R - Common Utilities
```r
#' Write Log Entry
#'
#' Appends a timestamped log message to console
#'
#' @param section Character string for log section (e.g., "STARTUP", "PROCESSING")
#' @param message Log message to write
#'
#' @examples
#' \dontrun{
#' write_log_entry("PROCESSING", "Starting analysis")
#' }
write_log_entry <- function(section, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(glue("[{timestamp}] [{section}] {message}\n"))
}

#' Write Compressed Parquet
#'
#' Writes parquet with zstd compression level 3
#'
#' @param data Tibble or data frame to write
#' @param path Output file path
#'
#' @examples
#' \dontrun{
#' data_tbl |> write_parquet_compressed("output/data.parquet")
#' }
write_parquet_compressed <- function(data, path) {
  arrow::write_parquet(data, path, compression = "zstd", compression_level = 3)
}
```

### Domain-Specific Libraries
Create separate library files for different domains:
- **`lib_data_import.R`** - Data loading and import functions
- **`lib_data_quality.R`** - Validation and quality checks
- **`lib_modeling.R`** - Model fitting and prediction (Optional)
- **`lib_visualization.R`** - Plotting functions (Optional)

### roxygen2 Documentation Template
```r
#' Brief One-Line Title
#'
#' More detailed description of what the function does and why it exists.
#' Explain use cases and important context.
#'
#' @param param1 Description with type (e.g., "Numeric vector of customer IDs")
#' @param param2 Description with defaults (e.g., "Maximum records, default 1000")
#' @param output_path Optional output file path (character string), default NULL
#'
#' @return Tibble with columns: col1 (numeric), col2 (character), col3 (date)
#'
#' @details Processing steps:
#'   1. Step one explanation
#'   2. Step two explanation
#'   3. Final output generation
#'
#' @note Performance: Processes ~10K records/second. Uses parallel processing.
#'
#' @examples
#' \dontrun{
#' result <- function_name(
#'   param1 = sample_data,
#'   param2 = 100,
#'   output_path = "output.parquet"
#'   )
#' glimpse(result)
#' }
function_name <- function(param1, param2 = 1000, output_path = NULL) {
  # Implementation
}
```

**Before asking "what parameters does X take?" → Read the roxygen2 docs in the file!**

## 🧪 Common Patterns

### Data Loading and Validation
```r
# Load data
data_tbl <- read_parquet("data/processed.parquet")

# Validate structure
glimpse(data_tbl)
summary(data_tbl)

# Check for nulls
data_tbl |>
  summarise(
    across(everything(), ~sum(is.na(.)))
    )

# Verify dimensions
write_log_entry("DATA", glue("Loaded {nrow(data_tbl)} rows, {ncol(data_tbl)} columns"))
```

### Join Pattern with Validation
```r
# Join with explicit relationship
combined_tbl <- left_data_tbl |>
  left_join(
    right_data_tbl,
    by = "key",
    relationship = "many-to-one"
    ) |>
  # Validate immediately after join
  filter(!is.na(key))

# Check join success
combined_tbl |>
  summarise(
    total_rows = n(),
    missing_values = sum(is.na(joined_column)),
    match_rate = 100 * (1 - sum(is.na(joined_column)) / n())
    )
```

### Parallel Processing Pattern (Optional - for ETL projects)
```r
library(furrr)

# Set up parallel processing (8 workers max)
plan(multisession, workers = 8)

# Process in parallel
results_tbl <- items |>
  future_map_dfr(
    ~process_item(.x),
    .progress = TRUE
    )

# Reset to sequential
plan(sequential)
```

### Testing with Small Samples
```r
# Always test with small sample first
test_size <- 100
test_data <- full_data |> slice(1:test_size)

# Run function on test data
test_result <- process_function(test_data)

# Validate structure
glimpse(test_result)
summary(test_result)

# If successful, run on full data
full_result <- process_function(full_data)
```

## 🎨 Quarto Notebooks (Optional - for analysis projects)

### Chunk Options
```r
#| echo: false     # Hide code by default
#| message: false  # Suppress messages
#| warning: false  # Suppress warnings
#| fig-width: 10   # Figure width
#| fig-height: 6   # Figure height
```

### Descriptive Chunk Labels
Use descriptive labels: `load_data`, `fit_model1`, `visualize_results`

### Narrative Text
Add explanatory text before and after code chunks explaining purpose and results.

### Markdown List Formatting in Narrative Sections

**CRITICAL**: Lists in Quarto documents follow standard Markdown rules:

```markdown
# ✅ CORRECT
Explanatory paragraph about what comes next:

  1. First numbered item
  1. Second numbered item (auto-numbered by Markdown)
  1. Third item with continuation
     that wraps to next line

Another paragraph after the list.

# ❌ WRONG - No blank line before list
This paragraph introduces:
  1. List item (will merge with paragraph!)

# ❌ WRONG - Blank lines between items
  1. First item

  1. Second item (creates two separate lists!)

# ❌ WRONG - Manual numbering
  1. First item
  2. Second item (fragile when reordering)
  3. Third item
```

**Common Patterns in This Workshop:**
- Bulleted lists for examples, properties, characteristics
- Numbered lists for step-by-step procedures, key findings
- Always use `1.` for numbered items (let Markdown handle numbering)
- Ensure blank line before list, no blank lines between items
- Use two-space indentation consistently

### Multi-Model Organization
```r
# Load Data section
training_tbl <- read_parquet(...)
validation_tbl <- read_parquet(...)

# Create Shared Data Objects section
lookup_tbl <- tibble(...)
subset_tbl <- training_tbl |> filter(...)

# Build Model 1 section
model1_fit <- fit_model(..., data = training_tbl)
model1_predictions <- predict(model1_fit, newdata = subset_tbl)

# Build Model 2 section
model2_fit <- fit_model(..., data = training_tbl)
model2_predictions <- predict(model2_fit, newdata = subset_tbl)
```

### Chunk Timing (Optional but recommended)
```r
# Setup timing hooks
chunk_times <- list()

knitr::knit_hooks$set(
  time_it = function(before, options, envir) {
    if (before) {
      chunk_times[[options$label]] <<- list(start = Sys.time())
    } else {
      chunk_times[[options$label]]$end <<- Sys.time()
      chunk_times[[options$label]]$elapsed <<-
        chunk_times[[options$label]]$end |>
        difftime(chunk_times[[options$label]]$start) |>
        as.numeric()
    }
  }
  )

knitr::opts_chunk$set(time_it = TRUE)

# Add timing summary section before R Environment
```

## 🎯 Statistical Modeling (Optional - remove if not applicable)

### Model Naming and Caching
```r
# Descriptive names with model type
baseline_lm <- lm(...)
model1_brmsfit <- brm(...)

# Cache fitted models
model1_brmsfit <- brm(
  formula,
  data = training_tbl,
  family = gaussian(),
  backend = "cmdstanr",
  seed = 4000,
  file = "models/model1_brmsfit",      # Auto-caching
  output_dir = "stan_output",          # CSV location
  output_basename = "model1_brmsfit"   # Readable names
  )
```

### Always Set Seed
```r
# For reproducibility
set.seed(4000)

# For Stan/brms models
brm(..., seed = 4000)
```

### Model Diagnostics Checklist
When adding model diagnostics:
- Convergence checks (Rhat, ESS)
- Residual plots
- Posterior predictive checks (for Bayesian)
- Model comparison metrics (AIC, BIC, LOO-CV)
- Visualization of fitted vs. actual

## 📊 Visualization Guidelines

### Standard Theme
```r
# Set theme at start of notebook/script
library(cowplot)
theme_set(theme_cowplot())
```

### Proper Labeling
```r
ggplot(data_tbl) +
  geom_point(
    aes(x = var1, y = var2, color = group),
    alpha = 0.5
    ) +
  labs(
    title = "Descriptive Title",
    subtitle = "Additional context",
    x = "X Axis Label",
    y = "Y Axis Label",
    caption = "Source: Data description"
    ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_brewer(palette = "Set1")
```

### Multi-Panel Plots
```r
library(cowplot)

# Combine plots
plot_grid(
  plot1, plot2, plot3,
  ncol = 2,
  labels = c("A", "B", "C")
  )
```

## 🚫 Common Mistakes to Avoid

### 1. Missing Join Relationships
```r
# ❌ WRONG - Generates warnings
left_join(data1, data2, by = "key")

# ✅ CORRECT - Explicit relationship
left_join(data1, data2, by = "key", relationship = "many-to-many")
```

### 2. Markdown List Formatting Errors
```markdown
# ❌ BAD - No blank line before list
Text introducing the list:
  - Item 1 (merges with paragraph!)

# ❌ BAD - Blank lines between items
  - Item 1

  - Item 2 (separate lists!)

# ❌ BAD - No indentation
- Item 1 (inconsistent)

# ❌ BAD - Manual numbering
  1. First
  2. Second
  3. Third (fragile when reordering)

# ✅ GOOD - Proper formatting
Text introducing the list:

  - Item 1
  - Item 2
  - Item 3 continues on next line
    with proper indentation

Next paragraph.

# ✅ GOOD - Auto-numbered list
Numbered list example:

  1. First item
  1. Second item
  1. Third item
```

### 3. Too Many Parallel Workers
```r
# ❌ DANGEROUS - May crash
plan(multisession, workers = 16)

# ✅ SAFE - 8 workers maximum
plan(multisession, workers = 8)
```

### 4. Using print() Instead of Logging
```r
# ❌ BAD - Clutters output
print(paste("Processing", nrow(data), "records"))

# ✅ GOOD - Structured logging
write_log_entry("PROCESSING", glue("Processing {nrow(data)} records"))
```

### 5. Not Converting Column Names on Import
```r
# ❌ BAD - Inconsistent names
data <- read_csv("messy_data.csv")

# ✅ GOOD - Standardize immediately
data <- read_csv("messy_data.csv") |>
  set_colnames(names(.) |> to_snake_case())
```

### 6. Skipping roxygen2 Documentation
```r
# ❌ BAD - Undocumented
process_data <- function(x, y) {...}

# ✅ GOOD - Complete documentation
#' Process Data with Validation
#'
#' @param x Numeric vector of values
#' @param y Character vector of labels
#' @return Tibble with processed results
process_data <- function(x, y) {...}
```

### 7. Not Testing Before Full Run
```r
# ❌ BAD - Running on full dataset without testing
result <- process_all_items(million_items)

# ✅ GOOD - Test with sample first
test_sample <- million_items |> slice(1:100)
test_result <- process_all_items(test_sample)
glimpse(test_result)  # Verify before full run
```

### 8. Changing Seeds Without Documentation
```r
# ❌ BAD - Breaks reproducibility
set.seed(sample(1:10000, 1))

# ✅ GOOD - Consistent, documented seed
set.seed(42)  # Project standard seed
```

## 🔧 Troubleshooting

### "Resource temporarily unavailable" with furrr
**Symptom:** Parallel processing crashes  
**Cause:** Thread explosion from linear algebra libraries  
**Solution:** Set threading environment variables in Docker:
```bash
-e OPENBLAS_NUM_THREADS=1
-e OMP_NUM_THREADS=1
-e MKL_NUM_THREADS=1
```

### "Permission denied" on volume mounts
**Symptom:** Container can't read/write files  
**Cause:** SELinux blocking access  
**Solution 1:** Add `:Z` suffix: `-v ./data:/data:Z`  
**Solution 2:** Disable SELinux: `--security-opt label=disable`

### Joins creating unexpectedly large output
**Symptom:** `left_join()` produces many more rows than input  
**Cause:** Many-to-many join without filtering  
**Solution:** Add filtering or check relationship:
```r
combined <- data1 |>
  left_join(data2, by = "key", relationship = "many-to-one") |>
  filter(!is.na(key))
```

### Container won't start with RUNROOTLESS=true
**Symptom:** rocker/tidyverse fails to start  
**Cause:** Image needs root for setup  
**Solution:** Use `RUNROOTLESS=false`

## 📦 Recommended R Packages

### Core Data Wrangling
- `tidyverse` - Data manipulation (dplyr, tidyr, readr, ggplot2)
- `arrow` - Parquet and columnar formats
- `qs` - Fast object serialization
- `lubridate` - Date/time operations
- `glue` - String interpolation
- `fs` - Cross-platform filesystem

### Data Import/Export
- `readxl` - Excel files
- `haven` - SPSS, Stata, SAS
- `jsonlite` - JSON data

### Parallel Processing (Optional - for ETL)
- `furrr` - Parallel functional programming
- `future` - Parallel execution backend

### Statistical Modeling (Optional - for analysis)
- `survival` - Survival analysis
- `brms` - Bayesian regression
- `rstanarm` - Applied regression modeling
- `tidybayes` - Tidy Bayesian analysis

### Visualization
- `cowplot` - Publication-quality plots
- `patchwork` - Combine plots
- `scales` - Scale formatting

### Utilities
- `conflicted` - Namespace conflict management
- `argparse` - Command-line arguments
- `tictoc` - Timing benchmarks

### Performance Optimization
- **Arrow threading**: `arrow::set_cpu_count()`, `arrow::set_io_thread_count()`
- **Parquet compression**: zstd level 3 (via `write_parquet_compressed()`)
- **qs package**: `preset = "balanced"` for speed/compression
- **Manual caching**: Use `file_exists()` for >2GB objects

## ✅ Pre-Flight Checklist

### Before Running Analysis/Processing
- [ ] All library files have roxygen2 documentation
- [ ] Column names standardized to snake_case
- [ ] Join relationships explicitly specified
- [ ] Threading environment variables set in Docker
- [ ] Tested with small sample first
- [ ] Random seeds set for reproducibility
- [ ] Expensive computations cached
- [ ] Docker container running with proper volumes

### Data Quality Checks
```r
# Structure validation
glimpse(data)
summary(data)

# Null checks
data |> summarise(across(everything(), ~sum(is.na(.))))

# Dimension checks
write_log_entry("VALIDATION", glue("{nrow(data)} rows, {ncol(data)} cols"))

# Duplicate checks (if applicable)
data |> summarise(duplicates = n() - n_distinct(key_column))
```

## 🎯 Tips for AI Assistants

1. **Context matters**: Check existing code for patterns before suggesting new approaches
2. **Preserve style**: Match existing naming and formatting conventions
3. **Document thoroughly**: Add roxygen2 headers and explanatory text
4. **Consider performance**: Large datasets may need sampling or parallelization
5. **Validate assumptions**: Test assumptions explicitly with data checks
6. **Reproducibility first**: Always set seeds, document versions
7. **Read existing docs**: Check roxygen2 headers before asking about parameters
8. **Use tidyverse patterns**: Avoid base R shortcuts, use pipelines
9. **Explicit relationships**: Always specify join relationships
10. **Test incrementally**: Small samples before full runs
11. **Output formatting**: Use `write_lines(text, stdout())` in Quarto
12. **Proper indentation**: Closing `)` on own line, 2 spaces from opening `(`
13. **Markdown lists**: Blank line before, no blanks between, two-space indent, use `1.` for auto-numbering
14. **Check list formatting**: Always verify lists will render as single unified lists

## ❓ Questions to Ask Before Making Changes

- Have I read the relevant existing code?
- Am I following the project's naming conventions?
- Do I understand the domain context?
- Have I included roxygen2 documentation for new functions?
- Will this work with the Docker environment?
- Are there similar implementations I can learn from?
- Have I considered computational cost?
- Is this change consistent with the project's goals?
- Am I using `write_lines(text, stdout())` for Quarto output?
- Are all tibbles named with `_tbl` suffix?
- Are join relationships explicitly specified?
- Are function arguments properly indented?
- Have I set seeds for reproducibility?
- **Do lists have blank line before but not between items?**
- **Are list items using two-space indentation?**
- **Are numbered lists using `1.` for auto-numbering?**

## �️ Justfile Commands Reference

### Available Commands
```bash
# List all commands
just --list
# or
just

# Project information
just info              # Show project configuration
just check-quarto      # Verify Quarto installation
just check-r           # Check R in container
just check-data        # Validate data files exist

# Development
just watch worksheet   # Auto-render on file changes (requires entr)
just validate          # Check all QMD files without rendering
just list-notebooks    # Show all available notebooks
just list-html         # Show rendered HTML files

# Data management
just list-data         # List CSV data files
just data-size         # Show data directory size

# Utilities
just disk-usage        # Show project disk usage
just count-code        # Count lines of code
just show-logs         # View recent rendering logs
just preview <file>    # Preview HTML in terminal (needs w3m/lynx)
just open-rstudio      # Open RStudio in browser (Linux)
```

### Hash-Based Smart Rendering
The Justfile implements content-based caching:
- Only re-renders when QMD file or dependencies (lib_utils.R, data_setup.R) change
- Tracks changes via MD5 hashes in `.just-cache/`
- Prevents unnecessary re-renders when only comments/whitespace change

## 📝 Environment Variables

Standard environment variables in Dockerfile:

```bash
# Timezone
TZ=Europe/Dublin

# RStudio credentials (set in Justfile)
USER=rstudio
PASSWORD=CHANGEME

# User/Group mapping
USERID=$(id -u)
GROUPID=$(id -g)
```

## 🚀 Quick Start

```bash
# 1. Build Docker image (first time only)
just docker-build-image

# 2. Start Docker container
just docker-run-image

# 3. Setup SSH tunnel (if remote)
just ssh-tunnel
# Copy the displayed command, run on local machine

# 4. Access RStudio
# Open: http://localhost:8787
# User: rstudio / Password: CHANGEME

# 5. In RStudio, load libraries and test
library(tidyverse)
library(survival)
library(survminer)
source("lib_utils.R")

# 6. Run quick validation
telco_churn_tbl <- read_csv("data/telcochurn.csv")
glimpse(telco_churn_tbl)

# 7. Render workshop notebook
just worksheet
```

## 🔄 Recent Updates & Known Issues

### Markdown List Formatting (2026-01-08)
- **Critical formatting rules established**: All lists now follow strict Markdown conventions
  - Blank line required BEFORE list (separates from preceding paragraph)
  - NO blank lines BETWEEN list items (prevents splitting into multiple lists)
  - Two-space indentation for all list items
  - Use `1.` for all numbered items (Markdown auto-numbers sequentially)
  - Four-space indentation for continuation lines
- **Workshop-wide formatting audit completed**: All ~30+ lists verified and corrected
- **Impact**: Proper rendering across all Markdown processors (Quarto, GitHub, Pandoc)

### Survival Analysis Workshop Content (2026-01-08)
- **Expanded pedagogical sections**: Added detailed explanations for Cox PH model sections
  - Model syntax and output interpretation
  - Assessment metrics (pseudo-R², concordance index) with benchmarks
  - Model building narrative for predictor selection
  - Comprehensive residual diagnostics (martingale, deviance)
  - Proportional hazards assumption testing
- **Cross-references added**: Theory sections now link to diagnostic validation sections
- **80-column formatting maintained** throughout document

---

**Last Updated**: 2026-01-08  
**Maintainer**: Mick Cooney (mcooney@describedata.com)

---

## 📖 Template Usage Guide

When using this template:

1. **Replace all placeholders** in `[BRACKETS]` with project-specific information
2. **Remove inapplicable sections**: 
   - Remove "Statistical Modeling" section for pure ETL projects
   - Remove "Parallel Processing" section if not doing batch processing
   - Remove "Quarto Notebooks" section for script-only projects
3. **Add domain-specific sections**: Include domain knowledge crucial for understanding
4. **Customize conventions**: Adjust to match existing team standards
5. **Document gotchas**: Add common mistakes specific to your domain
6. **Keep updated**: Treat as living document that evolves with project
7. **Be specific**: Generic guidelines less helpful than concrete examples
8. **Include examples**: Show actual code patterns to follow
9. **Think like an AI**: What context helps understand this codebase quickly?

**Goal**: Make AI assistants maximally effective by providing clear conventions, domain context, common patterns, and anti-patterns.
