# Tests for GitHub Issue #3: preserve behavior of invisible
# https://github.com/SamDM/ramcacher/issues/3

test_that("Issue #3: invisible expressions preserve invisibility on first call", {
  cache_rm()

  # withVisible returns a list with value and visible components
  result <- withVisible(cache_mem() %at% {
    invisible("hidden")
  })

  expect_equal(result$value, "hidden")
  expect_false(result$visible)
})

test_that("Issue #3: invisible expressions preserve invisibility on cached call", {

  cache_rm()

  # First call to populate cache
  cache_mem() %at% {
    invisible("hidden")
  }

  # Second call should also be invisible

  result <- withVisible(cache_mem() %at% {
    invisible("hidden")
  })

  expect_equal(result$value, "hidden")
  expect_false(result$visible)
})

test_that("Issue #3: invisible functions preserve invisibility on first call", {
  cache_rm()

  fn <- cache_mem() %at% function() {
    invisible("hidden")
  }

  result <- withVisible(fn())

  expect_equal(result$value, "hidden")
  expect_false(result$visible)
})

test_that("Issue #3: invisible functions preserve invisibility on cached call", {
  cache_rm()

  fn <- cache_mem() %at% function() {
    invisible("hidden")
  }

  # First call to populate cache
  fn()

  # Second call should also be invisible
  result <- withVisible(fn())

  expect_equal(result$value, "hidden")
  expect_false(result$visible)
})

test_that("Issue #3: visible expressions remain visible", {
  cache_rm()

  # First call
  result1 <- withVisible(cache_mem() %at% {
    "visible"
  })
  expect_equal(result1$value, "visible")
  expect_true(result1$visible)


  # Second call (cached)
  result2 <- withVisible(cache_mem() %at% {
    "visible"
  })
  expect_equal(result2$value, "visible")
  expect_true(result2$visible)
})

test_that("Issue #3: visible functions remain visible", {
  cache_rm()

  fn <- cache_mem() %at% function() {
    "visible"
  }

  # First call
  result1 <- withVisible(fn())
  expect_equal(result1$value, "visible")
  expect_true(result1$visible)

  # Second call (cached)
  result2 <- withVisible(fn())
  expect_equal(result2$value, "visible")
  expect_true(result2$visible)
})

test_that("Issue #3: cache_set/cache_get preserve invisibility", {
  cache_rm()

  # Set invisible value
  cache_set("test_invisible", invisible("hidden"))

  result <- withVisible(cache_get("test_invisible"))
  expect_equal(result$value, "hidden")
  expect_false(result$visible)
})

test_that("Issue #3: cache_set/cache_get preserve visibility", {
  cache_rm()

  # Set visible value
  cache_set("test_visible", "shown")

  result <- withVisible(cache_get("test_visible"))
  expect_equal(result$value, "shown")
  expect_true(result$visible)
})