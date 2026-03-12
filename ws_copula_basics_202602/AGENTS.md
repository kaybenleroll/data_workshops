# Workspace Mandates

## Engineering Standards
- **Indentation**: ALWAYS use 2 spaces for indentation in all project files (including `Justfile`, `Dockerfile`, `.qmd`, and `.R` files). NEVER use tab characters.
- **Terminology**: Use the formal term **Compounding Risk** when referring to the simultaneous escalation of frequency and severity. Avoid colloquialisms like "double whammy".
- **Tooling**: Prioritize using the `Justfile` targets for all container and rendering operations.

## R Coding Standards
- **Variable Naming**: Use `snake_case`. Tibbles should have a `_tbl` suffix (e.g., `data_tbl`). Matrices should have a `_mat` suffix if necessary for clarity.
- **Assignment**: Use the `<-` operator with spaces (e.g., `x <- 10`). Align `<-` in blocks when it improves readability.
- **Function Calls**:
  - For multi-line function definitions or calls, the closing parenthesis `)` must be on its own line, indented to the same level as the start of the call.
  - Multi-line arguments should be aligned or consistently indented.
- **ggplot2**:
  - Use `labs()` for all labeling.
  - Order `labs()` arguments as: `x`, `y`, `title`, `subtitle`.
  - In multi-line `labs()` calls, place the closing `)` on its own line.
- **Piping**: Prefer the native pipe `|>` for standard transformations.

## Rendering and Documentation
- Maintain a formal and technical tone in all workshop documentation.
- Ensure all Markdown lists are preceded by a newline to ensure correct rendering by Quarto.
- When updating `.qmd` files, always verify the output by rendering inside the container.
