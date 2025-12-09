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
