context("merging")

x <- data.frame(
 id1=rep(LETTERS[1:5], each=4),
 id2=factor(c("low", "high")),
 id3=rep(1:2, each=2),
 Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20))

x_c <- rvtable(x)
x_d <- rvtable(x, discrete=TRUE)

x_c_merge <- dplyr::group_by(x_c, id2, id3) %>% merge_rvtable()
x_d_merge <- dplyr::group_by(x_d, id3) %>% merge_rvtable()

x_c_margin <- marginalize(x_c, c("id1", "id3"))
x_c_margin_w <- marginalize(x_c, c("id1"), weights=c(1, 1.5, 2, 4, 1))
x_d_margin <- marginalize(x_d, c("id2"))
x_d_margin_w <- marginalize(x_d, c("id1"), weights=c(1, 1.5, 2, 4, 1))

test_that("get_levels returns correctly", {
  cl <- "list"
  expect_is(get_levels(x_c), cl)
  expect_is(get_levels(x_d), cl)
  expect_is(get_levels(x_c, "id2"), cl)
  expect_is(get_levels(x_d, "id3"), cl)
  expect_error(get_levels(x_c, "a"), "`variable` not found")
})

test_that("merge_rvtable returns correct class object", {
  cl <- "rvtable"
  expect_is(merge_rvtable(x_c), cl)
  expect_is(merge_rvtable(x_d), cl)
  expect_is(merge_rvtable(x_c, density.args=list(n=50), sample.args=list(n=100)), cl)
  expect_is(merge_rvtable(x_d, density.args=list(n=50), sample.args=list(n=100)), cl)
  expect_is(x_c_merge, cl)
  expect_is(x_d_merge, cl)
})

test_that("groups work with merge_rvtable", {
  expect_true(nrow(x_c_merge) > nrow(x_d_merge))
  expect_true(nrow(x_d_merge) > nrow(merge_rvtable(x_d)))
  expect_identical(as.character(dplyr::groups(x_c_merge)), c("id2", "id3"))
  expect_identical(as.character(dplyr::groups(x_d_merge)), c("id3"))
})

test_that("marginalize returns correct class object", {
  cl <- "rvtable"
  expect_is(x_c_margin, cl)
  expect_is(x_c_margin_w, cl)
  expect_is(x_d_margin, cl)
  expect_is(x_d_margin_w, cl)
  expect_is(marginalize(x_d, c("id1"), weights=c(1, 1.5, 0, 0, 0)), cl)
})

test_that("groups are ignored by marginalize", {
  y1 <- dplyr::group_by(x_c, id1, id2, id3) %>% marginalize(c("id1", "id3"))
  y2 <- dplyr::group_by(x_d, id2) %>% marginalize(c("id2"))
  expect_identical(dim(x_c_margin), dim(y1))
  expect_identical(attributes(x_c_margin), attributes(y1))
  expect_identical(dim(x_d_margin), dim(y2))
  expect_identical(attributes(x_d_margin), attributes(y2))
})

test_that("marginalize throws correct errors", {
  e <- c("Must specify variable(s) to marginalize over.",
         "May only marginalize over one variable at a time if using level weights.",
         "Marginalizing variable not found",
         "Invalid marginalizaing variable.",
         "Number of weights does not match the number of levels in `margin`.")
  expect_error(marginalize(x_c), e[1], fixed=TRUE)
  expect_error(marginalize(x_d), e[1], fixed=TRUE)
  expect_error(marginalize(x_c, c("id1", "id2"), weights=2), e[2])
  expect_error(marginalize(x_d, c("id1", "id2"), weights=2), e[2])
  expect_error(marginalize(x_c, c("ABC", "DEF"), weights=2), e[2])
  expect_error(marginalize(x_d, "ABC"), e[3])
  expect_error(marginalize(x_c, "Val"), e[4])
  expect_error(marginalize(x_d, "Prob"), e[4])
  expect_error(marginalize(x_c, "id1", weights=c(1, 1.5, 2, 4)), e[5])
  expect_error(marginalize(x_d, "id1", weights=c(1, 1.5, 2, 4, 1, 5)), e[5])
})

suppressMessages(library(dplyr))
cl <- "rvtable"
x <- data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable

test_that("merge_rvtable unrun examples work", {
  expect_is(merge_rvtable(x), cl)
  y <- x %>% group_by(id1) %>% merge_rvtable
  expect_is(y, cl)
  y <- x %>% group_by(id1, id2) %>% merge_rvtable
  expect_is(y, cl)
})

test_that("marginalize unrun examples work", {
  expect_is(marginalize(x, c("id1", "id2")), cl)
  lev <- get_levels(x, "id1")
  expect_is(lev, "list")
  expect_identical(lev, list(id1=LETTERS[1:5]))
  expect_is(marginalize(x, "id1", weights=c(1, 1.5, 2, 4, 1)), cl)
})

test_that("cycle_rvtable unrun examples work", {
  expect_is(cycle_rvtable(x, 2), cl)
  expect_is(x %>% group_by(id1, id2) %>% cycle_rvtable(3, keep="last"), cl)
})

test_that("cycle_rvtable keep arg has consistency", {
  set.seed(1)
  x0 <- cycle_rvtable(x, 3) %>% filter(Cycle==3) %>% rvtable()
  set.seed(1)
  x1 <- cycle_rvtable(x, 3, keep="last")
  expect_identical(x0, x1)
  expect_error(cycle_rvtable(x, keep=1), "keep must be 'all' or 'last'.")
})
