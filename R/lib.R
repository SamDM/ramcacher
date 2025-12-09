#' Apply a function in infix style.
#'
#' `f %at% x` is equivalent to `f(x)`.
#' In some cases this is convenient.
#' Especially to prevent having to put brackets around `x` if `x` is a long piece of code.
#'
#' @param f A function to apply.
#' @param x The argument to pass to `f`.
#'
#' @return The result of `f(x)`.
#'
#' @export
`%at%` <- function(f, x) {
  f({{ x }})
}

#' Turn an arbitrary string into a hash digest.
#'
#' @param x The string to hash.
#'
#' @return A hash digest string.
#'
#' @export
hash_str <- function(x) digest::digest(x, algo = "xxhash32")

#' Cache a function or expression in memory.
#'
#' The cached function/expression is executed once, the output is saved in RAM.
#' Subsequent calls will return the saved value, skipping the computation.
#'
#' `cache_mem` tries to detect when code or arguments change,
#' but it will fail in many situations.
#' To make sure a computation is re-run,
#' set `force` to TRUE and re-run the code wrapped by `cache_mem`.
#'
#' When `cache_mem` caches an expression,
#' the expression will be evaluated in the frame of `cache_mem`,
#' as such it can read/write/modify the local environment.
#' In-place updates in cached code are discouraged,
#' because in-place updates combined with caching can get very confusing.
#'
#' @param version Change this value to make the cache run its computation again.
#'   Only re-computes one time per version.
#' @param name Manually set the name of the cache entry.
#'   Watch out with colliding names if you set this for multiple caches.
#'   The version number will still be appended to the name.
#' @param verbose Output information about caching mechanism.
#' @param force Force a re-run of the cached code.
#'
#' @return A cached function or the result of a cached expression.
#'
#' @examples
#' # Cache a function
#' test_fn <- cache_mem() %at% function(x) {
#'   message("I was called")
#'   x + 1
#' }
#' test_fn(x = 1) # this will emit a message: "I was called".
#' test_fn(x = 1) # same output, but silent because the function was not ran again.
#' test_fn(x = 2) # now it will again emit the message: "I was called"
#'
#' # Cache an expression
#' test_expr_1 <- cache_mem() %at% {
#'   message("I was called")
#'   1 + 1
#' } # this will emit a message: "I was called"
#' test_expr_2 <- cache_mem() %at% {
#'   message("I was called")
#'   1 + 1
#' } # this will be silent, but same output
#' test_expr_3 <- cache_mem() %at% {
#'   message("I was called")
#'   1 + 2
#' } # code has changed, emits a message again.
#'
#' # Caching expressions can transparently read/write/modify the outer scope.
#' var_x <- 1
#' var_y <- 2
#' cache_mem() %at% {
#'   var_y <- var_x + var_y # in place update, not recommended
#'   var_z <- var_y * 2
#' }
#' stopifnot(var_y == 3)
#' stopifnot(var_z == 6)
#'
#' # Running it again won't do anything, because the computation is cached.
#' cache_mem() %at% {
#'   var_y <- var_x + var_y # in place update, not recommended
#'   var_z <- var_y * 2
#' }
#' stopifnot(var_y == 3)
#' stopifnot(var_z == 6)
#'
#' cache_rm() # clears the cache
#' @export
cache_mem <- function(version = 0, name = NULL, verbose = FALSE, force = FALSE) {
  function(cached_fn_or_expr) {
    .cache_init()

    # If the code starts with a '{' (ignoring whitespace), it is an expression.
    inp_as_str <- deparse(rlang::enexpr(cached_fn_or_expr))
    # When called with %at% there is a leading '~' character, don't know why.
    inp_as_str <- stringr::str_remove(inp_as_str, "^~")
    is_expr <- stringr::str_starts(trimws(inp_as_str[1]), "\\{")

    if (is_expr) {
      .cache_mem_expr(
        version = version, name = name, verbose = verbose, force = force,
        cached_expr = cached_fn_or_expr, expr_code = inp_as_str
      )
    } else {
      .cache_mem_fn(
        version = version, name = name, verbose = verbose, force = force,
        cached_fn = cached_fn_or_expr
      )
    }
  }
}

#' Get one element from the computation cache.
#'
#' @param name Name of cache value.
#'
#' @return The cached value, or NULL if not found.
#'
#' @export
cache_get <- function(name) {
  .cache_init()
  get(".cache_memory", .GlobalEnv)[[name]]
}

#' Set one element of the computation cache.
#'
#' @param name Name of cache value.
#' @param value The new value.
#'
#' @return The value.
#'
#' @export
cache_set <- function(name, value) {
  .cache_init()
  cache <- get(".cache_memory", .GlobalEnv)
  cache[[name]] <- value
  assign(".cache_memory", cache, envir = .GlobalEnv)
  value
}

#' Clear the computation cache.
#'
#' Clears all values from the cache, making it empty again.
#'
#' @param names Remove only the given names.
#'
#' @return NULL, invisibly.
#'
#' @export
cache_rm <- function(names = NULL) {
  .cache_init()
  if (is.null(names)) {
    cache <- list()
  } else {
    cache <- get(".cache_memory", .GlobalEnv)
    for (name in names) {
      cache[[name]] <- NULL
    }
  }
  assign(".cache_memory", cache, envir = .GlobalEnv)
  invisible(NULL)
}

#' List all names in the computation cache.
#'
#' @return A character vector of cache entry names.
#'
#' @export
cache_list <- function() {
  .cache_init()
  names(get(".cache_memory", .GlobalEnv))
}

# ------------------------------------------- private

#' Initialize the cache if it does not exist.
#'
#' @return NULL, invisibly.
#'
#' @noRd
.cache_init <- function() {
  if (!exists(".cache_memory", .GlobalEnv)) {
    assign(".cache_memory", list(), envir = .GlobalEnv)
  }
}

#' Cache a function in memory.
#'
#' @param version Cache version number.
#' @param name Optional name for the cache entry.
#' @param verbose Whether to output verbose messages.
#' @param force Whether to force re-computation.
#' @param cached_fn The function to cache.
#'
#' @return A wrapped function that caches its results.
#'
#' @importFrom utils capture.output
#' @noRd
.cache_mem_fn <- function(version = 0, name = NULL, verbose = FALSE, force = FALSE, cached_fn) {
  function(...) {
    if (is.null(name)) {
      # Make a name based on the hash of the input
      fn_code <- capture.output(print(cached_fn))
      args_code <- capture.output(print(rlang::enquos(...)))
      name <- sprintf("hash_fn.%s-args.%s", hash_str(fn_code), hash_str(args_code))
    }
    cache_name <- sprintf("fn::%s_v.%s", as.character(name), as.character(version))

    is_cached <- !is.null(cache_get(cache_name))
    must_run <- !is_cached | force
    if (verbose) {
      message(glue::glue(
        "[cache_mem] [fn] [cached: {is_cached}] [name: {cache_name}] [run: {must_run}]"
      ))
    }
    if (must_run) {
      cache_set(cache_name, cached_fn(...))
    }

    cache_get(cache_name)
  }
}

#' Cache an expression in memory.
#'
#' @param version Cache version number.
#' @param name Optional name for the cache entry.
#' @param verbose Whether to output verbose messages.
#' @param force Whether to force re-computation.
#' @param cached_expr The expression result to cache.
#' @param expr_code The deparsed expression code for hashing.
#'
#' @return The cached expression result.
#'
#' @noRd
.cache_mem_expr <- function(
  version = 0, name = NULL, verbose = FALSE, force = FALSE, cached_expr, expr_code
) {
  if (is.null(name)) {
    # Make a name based on the hash of the input
    name <- sprintf("hash_expr.%s", hash_str(expr_code))
  }
  cache_name <- sprintf("expr::%s_v.%s", as.character(name), as.character(version))

  is_cached <- !is.null(cache_get(cache_name))
  must_run <- !is_cached | force
  if (verbose) {
    message(glue::glue(
      "[cache_mem] [expr] [cached: {is_cached}] [name: {cache_name}] [run: {must_run}]"
    ))
  }
  if (must_run) {
    cache_set(cache_name, cached_expr)
  }

  cache_get(cache_name)
}
