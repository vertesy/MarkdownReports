# AGENTS

## Overview
This repository contains the **MarkdownReports** R package. It provides helpers to create publication‑ready figures and assemble Markdown reports directly from analysis scripts. Most plotting wrappers begin with the prefix `w` (e.g., `wplot`, `wbarplot`), automatically saving figures and inserting links in a Markdown log.

## Repository structure
- `R/` – Core R functions for plotting and report generation.
- `man/` – Package documentation generated from roxygen comments.
- `Examples/` – Example scripts that show typical usage patterns.
- `Development/` – Experimental or legacy code.
- Other top‑level files (`DESCRIPTION`, `NAMESPACE`, `README.md`) define package metadata and provide background information.

## Dependencies
This package relies on several other @vertesy projects:
- **Stringendo**
- **ReadWriter**
- **CodeAndRoll2**
- **MarkdownHelpers**
Additional CRAN dependencies are listed in `DESCRIPTION`.

## Setup & Testing
1. Ensure R and required packages are installed.
2. Build and check the package before committing:
   ```bash
   R CMD build .
   R CMD check MarkdownReports_*.tar.gz
   ```
   Or, using **devtools**:
   ```r
   R -q -e "devtools::document(); devtools::check()"
   ```
   Run these commands from the repository root.

## Getting started
- Read `README.md` for a high‑level introduction and motivation.
- Explore the scripts under `Examples/` to see common workflows.
- Browse the `R/` directory to learn how functions are organised and how reports are generated.
- Consult `man/` pages via `?function_name` in R for detailed documentation.

Happy plotting!
