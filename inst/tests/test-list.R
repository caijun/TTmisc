context("[<-.result")

fun <- function() list(a = matrix(1, nr = 1, nc = 1), b = 2)

test_that("implicitly extract all variabes without renaming", {
  list[] <- fun()  # return a = 1, b = 2
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("explicitly extract all variables without renaming and dimension dropping", {
  list[, , all = TRUE, drop = FALSE] <- fun()  # return matrix a, b = 2
  expect_true(is.matrix(a))
  expect_equal(b, 2)
})

test_that("only extract the first variable", {
  list[x, ] <- fun()  # return x = 1
  expect_equal(x, 1)
})

test_that("set drop to FALSE without dimension dropping", {
  list[x, , drop = FALSE] <- fun()  # return matrix x
  expect_true(is.matrix(x))
})

test_that("set all to TRUE to extract all elements", {
  list[x, , all = TRUE] <- fun()  # return x = 1, b = 2
  expect_equal(x, 1)
  expect_equal(b, 2)
})

test_that("only extract the second variable", {
  list[, y] <- fun()  # return y = 2
  expect_equal(y, 2)
})

test_that("explicitly extract all variables with renaming", {
  list[x, y] <- fun()  # return x = 1, y = 2
  expect_equal(x, 1)
  expect_equal(y, 2)
})

test_that("parameter all ignored", {
  list[, all = FALSE] <- fun()  # return a = 1, b = 2
  expect_equal(a, 1)
  expect_equal(b, 2)

  list[x, y, all = FALSE] <- fun()  # return x = 1, y = 2
  expect_equal(x, 1)
  expect_equal(y, 2)
})
