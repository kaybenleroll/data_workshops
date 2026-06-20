# Workshop: Multiple Correspondence Analysis

A comprehensive introduction to Multiple Correspondence Analysis (MCA) for categorical data, with emphasis on building intuition through simulation and real-world examples.

## Workshop Materials

- **ws_mca_part1_foundations.qmd**: Part 1 (foundations + simulation)
- **ws_mca_part2_real_data_and_advanced.qmd**: Part 2 (real data + advanced topics + practice)
- **lib_utils.R**: Supporting utility functions
- **Dockerfile**: Container configuration with all required R packages
- **Justfile**: Build and rendering automation

## Quick Start

### Prerequisites

- Podman (or Docker)
- Just (task runner)
- The workshop runs entirely in a container, so no local R setup needed

### Build and Run

1. **Build the container image:**
   ```bash
   just podman-build-image
   ```

2. **Start the container:**
   ```bash
   just podman-run-image
   ```

3. **Render the workshop (HTML):**
   ```bash
   just basics
   ```

This creates:

- `ws_mca_part1_foundations.html`
- `ws_mca_part2_real_data_and_advanced.html`

### Access RStudio

Once the container is running:
- **URL**: http://localhost:8791
- **User**: rstudio
- **Password**: CHANGEME (set via `DOCKER_PASS` environment variable)

To set the password:
```bash
export DOCKER_PASS="your_password"
just podman-run-image
```

## Workshop Structure

### Part 1: Building Blocks
- The humble contingency table
- Chi-square distance and profiles
- Eigenvector decomposition

### Part 2: Multiple Variables
- Indicator matrices
- Inertia (total "spreadedness")
- Contributions (which categories drive dimensions)

### Part 3: Building Intuition with Simulation
- Synthetic coffee shop archetypes
- How MCA recovers hidden structure
- Scree plots and dimension selection

### Part 4: Real-World Example
- The tea dataset (300 respondents, 11 variables)
- Interpreting dimensions
- Category biplots

### Part 5-8: Advanced Topics
- Handling rare categories and missing data
- Supplementary variables and mixed data
- Post-MCA clustering
- Full case study (Pokémon-like dataset)

### Part 9-10: Practical Guidance
- When to use MCA vs. alternatives
- Best practices checklist
- Common pitfalls and solutions
- Theoretical foundations

## Key R Packages

- **FactoMineR**: Core MCA implementation
- **factoextra**: Beautiful visualizations
- **ca**: Classical correspondence analysis
- **cluster** / **dendextend**: Clustering and dendrograms
- **tidyverse**: Data manipulation and visualization

## Rendering Options

Render a single document:
```bash
just render-container ws_mca_part1_foundations.qmd
just render-container ws_mca_part2_real_data_and_advanced.qmd
```

Render all documents:
```bash
just render-container-all
```

Access the container bash:
```bash
just podman-bash
```

## Cleaning Up

Remove generated HTML and cache files:
```bash
just clean-all
```

Stop and remove the container:
```bash
just podman-rm
```

Remove the container image:
```bash
podman rmi kaybenleroll/ws_mca_202605:latest
```

## Notes

- The workshop uses the `tea` dataset from `FactoMineR` (built-in, no downloads needed)
- All examples are synthetic or use public datasets
- HTML output includes code folding — click "Show code" to see R implementations
- RStudio Server inside the container runs on port 8791

## Author

**Mick Cooney** <mickcooney@gmail.com>

Part of the [Data Workshops](https://github.com/kaybenleroll/data_workshops) series.
