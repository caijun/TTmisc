context("[<-.result")

fun <- function() list(a = 1, b = 2)

test_that("implicitly extract all variabes without renaming", {
  list[] <- fun()  # return a = 1, b = 2
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("explicitly extract all variables without renaming", {
  list[, , all = TRUE] <- fun()  # return a = 1, b = 2
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("no variables extracted", {
  list[, , all = FALSE] <- fun()
  expect_false(exists("a"))
  expect_false(exists("b"))
})

test_that("only extract the first variable", {
  list[, y] <- fun()  # return y = 2
  expect_equal(y, 2)
})

test_that("set all to TRUE to extract all elements", {
  list[x, , all = TRUE] <- fun()  # return x = 1, b = 2
  expect_equal(x, 1)
  expect_equal(b, 2)
})

test_that("only extract the first variable", {
  list[, y] <- fun()  # return y = 2
  expect_equal(y, 2)
})

test_that("explicitly extract all variables with renaming", {
  list[x, y] <- fun()  # return x = 1, y = 2
  expect_equal(x, 1)
  expect_equal(y, 2)
})

test_that("parameter all ignored", {
  list[x, y, all = TRUE] <- fun()  # return x = 1, y = 2
  expect_equal(x, 1)
  expect_equal(y, 2)
})
