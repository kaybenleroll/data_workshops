# Workshop: Introduction to Information Theory

This workshop covers the foundations and applications of information theory
using R, simulation, and code-first demonstrations.

## Content

- **Part 1: Foundations**
  - Self-information and Shannon entropy
  - Joint and conditional entropy
  - Mutual information
  - Cross-entropy and KL divergence
  - Differential entropy
  - Rényi entropy family

- **Part 2: Coding, Channels, and Applications**
  - Shannon source coding theorem
  - Huffman coding and compression efficiency
  - Shannon noisy channel theorem
  - BSC, BEC, and AWGN channel capacities
  - Information theory in machine learning
  - Information theory in finance/insurance
  - Cross-domain sampler (genomics and NLP)

- **Worksheet**
  - Guided exercises with starter code and checks

## How to Use This Workshop

This workshop is designed to run locally or in a containerized environment.

### Prerequisites

- `podman`
- `just`

### Getting Started

1. Build the container image:

```bash
just image-build
```

2. Run the container:

```bash
just image-run
```

This starts RStudio Server at `http://localhost:8787`.
Default credentials are:

- user: `rstudio`
- password: `CHANGEME`

3. Render all workshop documents inside the container:

```bash
just all-container
```

### Local Rendering (without container)

If you already have the required R/Quarto setup:

```bash
just all
```

Or render a specific document:

```bash
just basics
just applications
just worksheet
```

## Structure

- `01_it_basics.qmd`: Part 1 foundations
- `02_it_applications.qmd`: Part 2 applications and coding/channel results
- `worksheet_information_theory.qmd`: exercise workbook
- `lib_information_theory.R`: helper functions for entropy, divergence,
  channels, and source coding
- `lib_utils.R`: shared utility functions
- `Dockerfile`: container image definition
- `Justfile`: build/render automation tasks
