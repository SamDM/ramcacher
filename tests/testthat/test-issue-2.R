# Tests for reported GitHub issues to prevent regressions

# ------------------------------------------- Issue #2: NULL values not cached
# https://github.com/SamDM/ramcacher/issues/2

test_that("Issue #2: NULL expressions are cached", {
  cache_rm()

  # First call should execute the expression and cache it
  result1 <- cache_mem() %at% {
    NULL
  }
  expect_null(result1)
  expect_length(cache_list(), 1)

  # Second call with same expression should use cache (not re-add to cache)
  result2 <- cache_mem() %at% {
    NULL
  }
  expect_null(result2)
  expect_length(cache_list(), 1)  # Still just 1 entry
})

test_that("Issue #2: NULL expressions show cached status in verbose mode", {
  cache_rm()

  # First call should show cached: FALSE
  expect_message(
    cache_mem(verbose = TRUE) %at% { NULL },
    "cached: FALSE"
  )

  # Second call should show cached: TRUE
  expect_message(
    cache_mem(verbose = TRUE) %at% { NULL },
    "cached: TRUE"
  )
})

test_that("Issue #2: NULL-returning functions are cached", {
  cache_rm()
  call_count <- 0

  null_fn <- cache_mem() %at% function(x) {
    call_count <<- call_count + 1
    NULL
  }

  # First call should execute
  result1 <- null_fn(1)
  expect_null(result1)
  expect_equal(call_count, 1)
  expect_length(cache_list(), 1)

  # Second call with same args should use cache
  result2 <- null_fn(1)
  expect_null(result2)
  expect_equal(call_count, 1)  # Should still be 1, not 2

  # Different args should execute again
  result3 <- null_fn(2)
  expect_null(result3)
  expect_equal(call_count, 2)
})

test_that("Issue #2: NULL-returning functions show cached status in verbose mode", {
  cache_rm()

  null_fn <- cache_mem(verbose = TRUE) %at% function(x) NULL

  # First call should show cached: FALSE
  expect_message(null_fn(1), "cached: FALSE")

  # Second call with same args should show cached: TRUE
  expect_message(null_fn(1), "cached: TRUE")
})

test_that("Issue #2: expressions returning invisible(NULL) are cached", {
  cache_rm()

  result1 <- cache_mem() %at% {
    invisible(NULL)
  }
  expect_null(result1)
  expect_length(cache_list(), 1)

  result2 <- cache_mem() %at% {
    invisible(NULL)
  }
  expect_null(result2)
  expect_length(cache_list(), 1)  # Still just 1 entry
})
