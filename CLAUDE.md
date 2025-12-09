# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ramcacher is an R package for in-memory caching of functions and expressions. It stores computation results in RAM and automatically detects when code or arguments change to invalidate stale cache entries.

## Development Commands

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-lib.R')"

# Check package (runs R CMD check)
Rscript -e "devtools::check()"

# Load package for interactive development
Rscript -e "devtools::load_all()"

# Lint code
Rscript -e "lintr::lint_package()"

# Build documentation
Rscript -e "devtools::document()"

# Build README from README.Rmd
Rscript -e "devtools::build_readme()"
```

## Architecture

The package exports these main functions from [R/lib.R](R/lib.R):

- `cache_mem()` - Core caching function that wraps functions or expressions
- `%at%` - Infix operator for applying cache_mem: `cache_mem() %at% function(x) x+1`
- `cache_get/cache_set/cache_list/cache_rm` - Direct cache manipulation utilities
- `hash_str()` - Hash strings using xxhash32 algorithm

Cache storage uses a global list `.cache_memory` in `.GlobalEnv`. The `cache_mem()` function distinguishes between function and expression caching by checking if the input starts with `{`.

## Dependencies

- digest: For hashing (xxhash32)
- glue: String interpolation
- rlang: Expression handling
- stringr: String manipulation
- testthat: Testing (dev dependency)

## Code Style

Line length limit: 100 characters (configured in [.lintr](.lintr))