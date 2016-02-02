context("rvtable")

test_that("returns correct class object", {
  x <- rnorm(100)
  d <- rvtable(x)

  expect_is(d, "data.table")
  expect_is(d, "rvtable")
})

test_that("test different input classes", {
  n <- 100
  x <- rnorm(n)
  d <- rvtable(x)

  expect_is(d, "data.table")
  expect_is(d, "rvtable")
  expect_equal(nrow(d), 512)
  expect_true(all(c("Val", "Prob") %in% names(d)))

  d <- rvtable(x, density.args=list(n=50))
  expect_equal(nrow(d), 50)
  d <- rvtable(x, discrete=TRUE)
  expect_equal(nrow(d), n)
  expect_equal(sum(d$Prob), 1)

  n <- 5
  x <- 1:n
  probs <- c(0.1, 0.2, 0.3, 0.15, 0.25)
  d <- rvtable(x, y=probs)

  expect_is(d, "data.table")
  expect_is(d, "rvtable")
  expect_equal(nrow(d), n)
  expect_true(all(c("Val", "Prob") %in% names(d)))
  expect_equal(sum(d$Prob), 1)

  attr(x, "probabilities") <- probs
  d <- rvtable(x)

  expect_is(d, "data.table")
  expect_is(d, "rvtable")
  expect_equal(nrow(d), n)
  expect_true(all(c("Val", "Prob") %in% names(d)))
  expect_equal(sum(d$Prob), 1)

  x <- data.table(id=rep(LETTERS[1:2], each=10), v1=rep(1:10, 2), p1=c(rep(0.1, 10), sqrt(1:10)))
  d <- rvtable(x, Val="v1", Prob="p1")

  expect_is(d, "data.table")
  expect_is(d, "rvtable")
  expect_equal(nrow(d), 20)
  expect_true(all(c("Val", "Prob") %in% names(d)))
})

test_that("test error handling", {
  expect_error(rvtable(c(rnorm(100), NA)), "Missing values not permitted.")
  expect_error(rvtable(5), "A single value for `x` with probability=1 is only allowed when discrete=TRUE")
  expect_error(rvtable(1:10, 1:11), "Values and probabilities do not have equal length.")
  expect_error(rvtable(structure(1:10, probability=1:11)), "Values and probabilities do not have equal length.")
  expect_error(rvtable(list(1,2,3)), "`x` is not a data frame or data table.")
  expect_error(rvtable(data.frame(x=1:10, y=1), Val="x", Prob="x"), "`Val` and `Prob` cannot refer to the same column.")
  expect_error(rvtable(data.frame(x=1:10, y=1), Val="x", Prob="z"), "No column called z")
  expect_error(rvtable(data.frame(x=1:10, y=1), Val="z", Prob="y"), "No column called z")
  expect_error(rvtable(data.table(x=rep(1,2), y=1), Val="x", Prob="y"), "Duplicated values in `Val`.")
})
