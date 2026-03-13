# Workspace Mandates

## Engineering Standards
- **Indentation**: ALWAYS use 2 spaces for indentation in all project files (including `Justfile`, `Dockerfile`, `.qmd`, and `.R` files). NEVER use tab characters.
- **Terminology**: Use the formal term **Compounding Risk** when referring to the simultaneous escalation of frequency and severity. Avoid colloquialisms like "double whammy".
- **Tooling**: Prioritize using the `Justfile` targets for all container and rendering operations.

## R Coding Standards
- **General Style**: Adhere to the [Tidyverse style guide](https://style.tidyverse.org/) for R code.
- **Indentation**: ALWAYS use 2 spaces for indentation.
- **Assignment**: Use the `<-` operator with spaces (e.g., `x <- 10`). Align `<-` in blocks when it improves readability.
- **Function Calls (Multi-line)**: For multi-line function definitions or calls, place the closing parenthesis `)` on its own line, indented to the same level as the *first argument* of the function call. Example:
  ```r
  my_function(
    argument_one = value1,
    argument_two = value2
    ) # Closing parenthesis aligns with 'argument_one'
  ```
- **ggplot2**:
  - Use `labs()` for all labeling.
  - Order `labs()` arguments as: `x`, `y`, `title`, `subtitle`.
- **Piping**: Prefer the native pipe `|>` for standard transformations.
- **`tibble` and `options`**: Ensure clear alignment for arguments.
  ```r
  tibble(
    var_one = value1,
    var_two = value2
    )

  options(
    width = 80L,
    warn  = 1
    )
  ```

## Rendering and Documentation
- Maintain a formal and technical tone in all workshop documentation.
- Ensure all Markdown lists are preceded by a newline to ensure correct rendering by Quarto.
- When updating `.qmd` files, always verify the output by rendering inside the container.
