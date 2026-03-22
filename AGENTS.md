# Agent Instructions for `data_workshops`

## Global Context
- **Repository:** Monorepo containing R and Python data workshops and talks.
- **Technologies:** R, Python, Quarto (.qmd), RMarkdown (.Rmd), Stan, Docker, Make, Just.

## Core Mandates
- **Directory Scoping:** Always limit operations to the specific subdirectory requested by the user. Do not attempt to run tools or builds from the repository root unless explicitly instructed.
- **Data Protection:** The `data/` directories contain read-only source files. Never modify raw data files or commit intermediate/generated files to the repository.
- **Build Artifacts:** Do not modify the `build/`, `*_cache/`, or `*_files/` directories directly. These are auto-generated.
- **Commit Messages:** Use clear, concise commit messages.
- **Standard Tooling:** Refer to the local `Makefile` or `Justfile` within the active directory for specific build and test instructions.

## AI Assistant Configuration
- This file (`AGENTS.md`) is the canonical source of truth for all AI agents working in this repository.
- **OpenCode:** Natively reads `AGENTS.md`.
- **Claude Code:** Reads `CLAUDE.md` (which is symlinked to `AGENTS.md`).
- **Gemini CLI:** Reads `GEMINI.md` (which is symlinked to `AGENTS.md`).
