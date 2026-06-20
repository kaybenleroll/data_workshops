# Workshop: Introduction to Copulas

This workshop covers the basic concepts of copulas using R, focusing on
elliptical and Archimedean copulas, and the modeling process.

## Content

- **Introduction to Copulas**
  - Sklar's Theorem
  - Probability Integral Transform (PIT)
  - Elliptical Copulas (Gaussian and t)
  - Archimedean Copulas (Clayton, Gumbel, Frank)
  - Tail Dependence
  - Fitting and Selection using `VineCopula`

## How to use this workshop

This workshop is designed to be run in a containerized environment using Podman
and `just`.

### Prerequisites

- `podman`
- `just`

### Getting Started

1.  **Build the container image:**
    ```bash
    just podman-build-image
    ```

2.  **Run the container:**
    ```bash
    just podman-run-image
    ```
    This will start RStudio Server on `http://localhost:8787`. The default username is `rstudio` and the password is `CHANGEME`.

3.  **Render the notebooks:**
    You can render the notebooks from within the container or using `just`:
    ```bash
    just all
    ```

## Structure

- `ws_copula_basics.qmd`: Main workshop notebook.
- `lib_utils.R`: Utility functions.
- `Dockerfile`: Container definition.
- `Justfile`: Automation tasks (using podman).
