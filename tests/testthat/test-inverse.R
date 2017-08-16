context("inverse")

suppressMessages(library(dplyr))
set.seed(1)
x <- data.table(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:20, 10) + rnorm(200), Prob=rep(sqrt(1:20), 10)) %>% rvtable
x2 <- filter(x, id2=="low" & id3==1) %>% select(-id2, -id3) %>% rvtable


test_that("inverse_pmf returns correctly", {
  cl <- "rvtable"
  y1 <- inverse_pmf(x, c(5, 8), "id1", sample.args=list(n=100))
  y2 <- inverse_pmf(x2, c(2, 8), "id1", sample.args=list(n=1000))

  expect_is(y1, cl)
  expect_is(y2, cl)
  expect_equal(nrow(y1), 5*2*2)
  expect_equal(nrow(y2), 5)

  x <- rvtable(data.frame(Val=1:10, Prob=1:10), discrete=TRUE)

  rng_err <- "`val.range` must be a length-2 vector giving a valid range."
  expect_error(inverse_pmf(x, c(2, 4)), "`var.new` missing.")
  expect_error(inverse_pmf(x, c(2, 4, 5), "id"), rng_err)
  expect_error(inverse_pmf(x, NULL, "id"), rng_err)
  expect_error(inverse_pmf(x, c(2, 1), "id"), rng_err)
  expect_error(inverse_pmf(x, c(2, 4), "id"), "id not found.")

  x <- rvtable(data.frame(x1=1:10, Prob=1:10, id=rep(LETTERS[1:2], each =5)), Val="x1", discrete=TRUE)
  expect_error(inverse_pmf(x, c(2, 4), "id"), "inverse pmf not currently implemented for discrete rvtables.")

  x <- rvtable(data.frame(x1=1:10, id=rep(LETTERS[1:2], each =5)), Val="x1", force.dist=FALSE)
  expect_error(inverse_pmf(x, c(2, 4), "id"), "`x` must be a distribution-type rvtable.")
})
