
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ramcacher

<!-- badges: start -->
<!-- badges: end -->

ramcacher provides in-memory caching for R functions and expressions. It
stores computation results in RAM and automatically invalidates cache
entries when code or arguments change. This is useful for speeding up
repeated expensive computations during interactive data analysis.

## Installation

You can install the development version of ramcacher from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SamDM/ramcacher")
```

## Example

### Caching Functions

Wrap a function with `cache_mem()` to cache its results. The first call
runs the computation, subsequent calls with the same arguments return
the cached result:

``` r
library(ramcacher)

# Clear any existing cache
cache_rm()

# Create a cached function using the %at% operator
slow_computation <- cache_mem() %at% function(x) {
  message("Running computation...")
  Sys.sleep(0.1)  # Simulate slow work
  x^2
}

# First call - runs the computation
slow_computation(4)
#> Running computation...
#> [1] 16

# Second call with same argument - returns cached result (no message printed)
slow_computation(4)
#> [1] 16

# Different argument - runs computation again
slow_computation(5)
#> Running computation...
#> [1] 25
```

### Caching Expressions

You can also cache arbitrary code blocks. The cache key is based on the
code itself:

``` r
cache_rm()

# Cache an expression
result1 <- cache_mem() %at% {
  message("Computing...")
  sum(1:100)
}
#> Computing...

# Same expression returns cached result (no message)
result2 <- cache_mem() %at% {
  message("Computing...")
  sum(1:100)
}

result1 == result2
#> [1] TRUE
```

### Cache Management

Inspect and manage cache entries directly:

``` r
cache_rm()

# Store some cached values
cache_set("my_data", mtcars[1:3, 1:3])
#>                mpg cyl disp
#> Mazda RX4     21.0   6  160
#> Mazda RX4 Wag 21.0   6  160
#> Datsun 710    22.8   4  108
cache_set("my_result", 42)
#> [1] 42

# List all cache entries
cache_list()
#> [1] "my_data"   "my_result"

# Retrieve a cached value
cache_get("my_result")
#> [1] 42

# Remove specific entries
cache_rm("my_data")
cache_list()
#> [1] "my_result"

# Clear all cache entries
cache_rm()
cache_list()
#> NULL
```

### Versioning and Forcing Recomputation

Use the `version` parameter to invalidate cache when your logic changes,
or `force = TRUE` to always recompute:

``` r
cache_rm()

# Version 1 of the computation
cached_fn <- cache_mem(version = 1) %at% function(x) {
  message("v1 running")
  x + 1
}
cached_fn(10)
#> v1 running
#> [1] 11

# Bump version to force new computation
cached_fn_v2 <- cache_mem(version = 2) %at% function(x) {
  message("v2 running")
  x + 1
}
cached_fn_v2(10)
#> v2 running
#> [1] 11

# Or use force = TRUE to always recompute
forced_fn <- cache_mem(force = TRUE) %at% function(x) {
  message("Forced run")
  x + 1
}
forced_fn(10)
#> Forced run
#> [1] 11
```

### Debugging with Verbose Mode

Enable verbose output to see cache hits and misses:

``` r
cache_rm()

cached_fn <- cache_mem(verbose = TRUE) %at% function(x) x * 2

cached_fn(5)  # Cache miss
#> [cache_mem] [fn] [cached: FALSE] [name: fn::hash_fn.60e37a0c-args.eb191209_v.0] [run: TRUE]
#> [1] 10
cached_fn(5)  # Cache hit
#> [cache_mem] [fn] [cached: TRUE] [name: fn::hash_fn.60e37a0c-args.eb191209_v.0] [run: FALSE]
#> [1] 10
```
