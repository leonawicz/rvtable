context("helpers")

x <- rvtable(1:10)

test_that("unrun helpers examples return correctly", {
  expect_true(is_rvtable(x))
  expect_true(!is_sample(x))
  expect_true(is_distribution(x))
  expect_true(!is_discrete(x))
  expect_true(is_continuous(x))
  expect_true(is_density(x))

  expect_is(rvattr(x), "list")
  expect_equal(length(rvattr(x)), 6)
  expect_equal(length(rvattr(x, id=c("rvtype", "tabletype"))), 2)
  expect_equal(length(rvattr(x, all=TRUE)), 9)

  expect_equal(rvtype(x), "continuous")
  expect_equal(tabletype(x), "distribution")
  expect_equal(valcol(x), "x")
  expect_equal(probcol(x), "y")

  expect_identical(get_density_args(x), list(n=512, adjust=1, bw="nrd0", kernel="gaussian"))
  expect_identical(get_sample_args(x), list(n=10000, interp=TRUE, n.interp=100000))
  x <- set_density_args(x, list(n = 1000, adjust = 0.1))
  x <- set_sample_args(x, list(n = 100))
  expect_identical(get_density_args(x), list(n=1000, adjust=0.1, bw="nrd0", kernel="gaussian"))
  expect_identical(get_sample_args(x), list(n=100, interp=TRUE, n.interp=100000))
})
