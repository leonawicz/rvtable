context("sampling")

x <- rvtable(rnorm(1000))
y <- sample_rvtable(x, n=100)
z <- sample_rvtable(y, n=50, resample=TRUE)
d <- rvtable(sample(1:100, 50), discrete=TRUE)
d2 <- sample_rvtable(d, n=100, resample=TRUE)
d3 <- data.frame(X=LETTERS[1:2], Y=factor(c("aaa", "aaa", "bbb", "bbb")),
                 Val=rep(1:100, each=4), Prob=rep(1:100, each=4)) %>%
  dplyr::group_by(X) %>% rvtable
cl <- "rvtable"

test_that("returns correct class object", {
  expect_is(x, cl)
  expect_is(y, cl)
  expect_is(z, cl)
  expect_is(d, cl)
  expect_is(d2, cl)
})

test_that("has correct attributes", {
  att_rv <- function(x) attr(x, "rvtype")
  att_tbl <- function(x) attr(x, "tabletype")
  expect_identical(att_rv(y), "continuous")
  expect_identical(att_rv(z), "continuous")
  expect_identical(att_rv(d), "discrete")
  expect_identical(att_rv(d2), "discrete")
  expect_identical(att_tbl(y), "sample")
  expect_identical(att_tbl(z), "sample")
  expect_identical(att_tbl(d), "distribution")
  expect_identical(att_tbl(d2), "sample")
  expect_identical(att_tbl(d3), "distribution")
})

test_that("has correct RV columns", {
  only_val <- function(x, v, p) v %in% names(x) & !(p %in% names(x))
  expect_true(only_val(y, "x", "y"))
  expect_true(only_val(z, "x", "y"))
  expect_true(only_val(d2, "x", "y"))

  nam <- c("Val", "Prob")
  expect_true(all(nam %in% names(d3) & nam[1]==attr(d3, "valcol") & nam[2]==attr(d3, "probcol")))
})

test_that("test error handling", {
  expect_error(sample_rvtable(5), "`x` must be an rvtable.")
})

test_that("arguments work with grouping variables", {
  n <- 5
  nd3 <- as.integer(n*length(unique(d3$X))*nlevels(d3$Y))
  s <- sample_rvtable(d3, n=n)
  expect_identical(nrow(s), nd3)
  expect_identical(ncol(s), ncol(d3)-1L)
  expect_identical(dplyr::groups(s), dplyr::groups(d3))

  d4 <- d3 %>% dplyr::group_by()
  s <- sample_rvtable(d4, n=n, density.args=list(n=50, adjust=2))
  expect_identical(nrow(s), nd3)
  expect_identical(ncol(s), ncol(d3)-1L)
  expect_identical(dplyr::groups(s), dplyr::groups(d4))

  s2 <- sample_rvtable(s, n=n)
  expect_identical(nrow(s2), nrow(s))
  expect_identical(ncol(s2), ncol(s))
  expect_identical(dplyr::groups(s2), dplyr::groups(s))
})
