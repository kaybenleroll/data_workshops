# Workshop: Introduction to Copulas

This workshop covers the basic concepts of copulas using R.

## Content

- **Part 1: Introduction to Copulas**
  - Sklar's Theorem
  - Probability Integral Transform (PIT)
  - Elliptical Copulas (Gaussian and t)
- **Part 2: Copula Modelling**
  - Archimedean Copulas (Clayton, Gumbel, Frank)
  - Tail Dependence
  - Fitting and Selection using `VineCopula`

## How to use this workshop

This workshop is designed to be run in a containerized environment using Podman (or Docker) and `just`.

### Prerequisites

- `podman` (or `docker`)
- `just`

### Getting Started

1.  **Build the container image:**
    ```bash
    just docker-build-image
    ```

2.  **Run the container:**
    ```bash
    just docker-run-image
    ```
    This will start RStudio Server on `http://localhost:8787`. The default username is `rstudio` and the password is `CHANGEME`.

3.  **Render the notebooks:**
    You can render the notebooks from within the container or using `just`:
    ```bash
    just all
    ```

## Structure

- `01_copula_intro.qmd`: Part 1 of the workshop.
- `02_copula_modelling.qmd`: Part 2 of the workshop.
- `lib_utils.R`: Utility functions.
- `Dockerfile`: Container definition.
- `Justfile`: Automation tasks.
