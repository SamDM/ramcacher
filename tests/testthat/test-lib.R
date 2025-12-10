# ------------------------------------------- %at% operator

test_that("%at% applies function to argument", {
  result <- sqrt %at% 16
  expect_equal(result, 4)
})
test_that("%at% works with anonymous functions", {
  expected <- 4
  result <- (function(x) x + 1) %at% 3
  expect_equal(result, expected)
})

test_that("%at% works with complex expressions", {
  result <- sum %at% c(1, 2, 3, 4, 5)
  expect_equal(result, 15)
})

# ------------------------------------------- hash_str

test_that("hash_str returns a string", {
  result <- hash_str("hello")
  expect_type(result, "character")
})

test_that("hash_str returns consistent results", {
  result1 <- hash_str("test input")
  result2 <- hash_str("test input")
  expect_equal(result1, result2)
})

test_that("hash_str returns different hashes for different inputs", {
  result1 <- hash_str("input1")
  result2 <- hash_str("input2")
  expect_false(result1 == result2)
})

test_that("hash_str works with various types", {
  expect_type(hash_str(123), "character")
  expect_type(hash_str(list(a = 1, b = 2)), "character")
  expect_type(hash_str(c(1, 2, 3)), "character")
})

# ------------------------------------------- cache_get / cache_set / cache_list / cache_rm

test_that("cache_set stores and cache_get retrieves values", {
  cache_rm()
  cache_set("test_key", "test_value")
  result <- cache_get("test_key")
  expect_equal(result, "test_value")
})

test_that("cache_get returns NULL for non-existent keys", {
  cache_rm()
  result <- cache_get("non_existent_key")
  expect_null(result)
})

test_that("cache_set returns the value", {
  cache_rm()
  result <- cache_set("key", "value")
  expect_equal(result, "value")
})

test_that("cache_set overwrites existing values", {
  cache_rm()
  cache_set("key", "original")
  cache_set("key", "updated")
  result <- cache_get("key")
  expect_equal(result, "updated")
})

test_that("cache_list returns all cache entry names", {
  cache_rm()
  cache_set("key1", "value1")
  cache_set("key2", "value2")
  cache_set("key3", "value3")
  result <- cache_list()
  expect_setequal(result, c("key1", "key2", "key3"))
})

test_that("cache_list returns empty character vector when cache is empty", {
  cache_rm()
  result <- cache_list()
  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("cache_rm clears all entries when called without arguments", {
  cache_rm()
  cache_set("key1", "value1")
  cache_set("key2", "value2")
  cache_rm()
  expect_length(cache_list(), 0)
})

test_that("cache_rm removes only specified entries", {
  cache_rm()
  cache_set("key1", "value1")
  cache_set("key2", "value2")
  cache_set("key3", "value3")
  cache_rm("key2")
  result <- cache_list()
  expect_setequal(result, c("key1", "key3"))
})

test_that("cache_rm removes multiple specified entries", {
  cache_rm()
  cache_set("key1", "value1")
  cache_set("key2", "value2")
  cache_set("key3", "value3")
  cache_rm(c("key1", "key3"))
  result <- cache_list()
  expect_equal(result, "key2")
})

test_that("cache_rm returns NULL invisibly", {
  cache_rm()
  result <- cache_rm()
  expect_null(result)
})

# ------------------------------------------- cache_mem with functions

test_that("cache_mem caches function results", {
  cache_rm()
  call_count <- 0
  cached_fn <- cache_mem()(function(name) {
    call_count <<- call_count + 1
    paste0("Hello, ", name, "!")
  })

  result1 <- cached_fn("World")
  result2 <- cached_fn("World")
  expect_equal(call_count, 1)

  result3 <- cached_fn("R")
  expect_equal(call_count, 2)

  expect_equal(result1, "Hello, World!")
  expect_equal(result2, "Hello, World!")
  expect_equal(result3, "Hello, R!")
})

test_that("cache_mem works with %at% operator", {
  cache_rm()
  call_count <- 0
  cached_fn <- cache_mem() %at% function(x) {
    call_count <<- call_count + 1
    x * 2
  }

  result1 <- cached_fn(5)
  result2 <- cached_fn(5)
  expect_equal(call_count, 1)
  expect_equal(result1, 10)
  expect_equal(result2, 10)
})

test_that("cache_mem force parameter re-runs computation", {
  cache_rm()
  call_count <- 0
  cached_fn <- cache_mem()(function(x) {
    call_count <<- call_count + 1
    x + 1
  })

  cached_fn(1)
  expect_equal(call_count, 1)

  # Force re-run

  forced_fn <- cache_mem(force = TRUE)(function(x) {
    call_count <<- call_count + 1
    x + 1
  })
  forced_fn(1)
  expect_equal(call_count, 2)
})

test_that("cache_mem version parameter creates separate cache entries", {
  cache_rm()
  call_count <- 0

  fn <- function(x) {
    call_count <<- call_count + 1
    x + 1
  }

  cached_v1 <- cache_mem(version = 1)(fn)
  cached_v2 <- cache_mem(version = 2)(fn)

  cached_v1(1)
  expect_equal(call_count, 1)

  cached_v2(1)
  expect_equal(call_count, 2)

  # Calling v1 again should use cache
  cached_v1(1)
  expect_equal(call_count, 2)
})

test_that("cache_mem name parameter allows custom cache names", {
  cache_rm()
  call_count <- 0

  cached_fn <- cache_mem(name = "my_custom_name")(function(x) {
    call_count <<- call_count + 1
    x
  })

  cached_fn(1)
  cache_names <- cache_list()
  expect_true(any(grepl("my_custom_name", cache_names)))
})

test_that("cache_mem verbose parameter outputs messages", {
  cache_rm()
  cached_fn <- cache_mem(verbose = TRUE)(function(x) x + 1)

  expect_message(cached_fn(1), "cache_mem")
})

# ------------------------------------------- cache_mem with expressions

test_that("cache_mem caches expression results", {
  cache_rm()

  result1 <- cache_mem() %at% {
    1 + 1
  }
  expect_equal(result1, 2)

  # Same expression should return cached result
  result2 <- cache_mem() %at% {
    1 + 1
  }
  expect_equal(result2, 2)
})

test_that("cache_mem expression returns different results for different code", {
  cache_rm()

  result1 <- cache_mem() %at% {
    1 + 1
  }
  expect_equal(result1, 2)

  # Different expression should have different result
  result2 <- cache_mem() %at% {
    1 + 2
  }
  expect_equal(result2, 3)
})

test_that("cache_mem expression can read outer scope", {
  cache_rm()
  outer_var <- 10

  result <- cache_mem() %at% {
    outer_var * 2
  }
  expect_equal(result, 20)
})

test_that("cache_mem expression with version creates separate entries", {
  cache_rm()

  result1 <- cache_mem(version = 1) %at% {
    "v1_result"
  }
  expect_equal(result1, "v1_result")

  result2 <- cache_mem(version = 2) %at% {
    "v2_result"
  }
  expect_equal(result2, "v2_result")

  # Version 1 again should return cached value
  result3 <- cache_mem(version = 1) %at% {
    "v1_result"
  }
  expect_equal(result3, "v1_result")
})

test_that("cache_mem expression with name allows custom cache names", {
  cache_rm()

  cache_mem(name = "my_expr_cache") %at% {
    "cached_value"
  }

  cache_names <- cache_list()
  expect_true(any(grepl("my_expr_cache", cache_names)))
})

test_that("cache_mem expression verbose outputs messages", {
  cache_rm()
  expect_message(
    cache_mem(verbose = TRUE) %at% {
      1 + 1
    },
    "cache_mem"
  )
})

test_that("cache_mem expression stores in cache_list", {
  cache_rm()

  cache_mem() %at% {
    "test_value"
  }

  # Should have at least one entry
  expect_gte(length(cache_list()), 1)
})
