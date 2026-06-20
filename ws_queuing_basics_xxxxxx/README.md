# Workshop: Introduction to Queuing Theory

This workshop covers the fundamental concepts of queuing theory using R, analytical
calculations, and discrete-event simulation.

## Content

- **Part 1: Queuing Theory Basics**
  - Kendall Notation
  - Poisson Processes and Exponential Distributions
  - M/M/1, M/M/c, M/D/1, and M/G/1 analytical models
  - Little's Law and the "Hockey Stick" effect
  - Discrete-Event Simulation (DES) using the `simmer` package
- **Part 2: Advanced Queuing Theory**
  - Finite Capacity (M/M/1/K) and Blocking Probability
  - Customer Impatience (Balking and Reneging)
  - Priority Queues (SLA Management)
  - Tandem Queues and Simple Networks
  - Staffing Optimization (Wait Costs vs. Server Costs)

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

- `01_queuing_basics.qmd`: Main workshop notebook.
- `lib_queueing.R`: Custom functions for queuing calculations and simulation.
- `lib_utils.R`: General utility functions.
- `Dockerfile`: Container definition.
- `Justfile`: Automation tasks (using podman).
