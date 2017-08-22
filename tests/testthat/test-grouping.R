context("grouping")

suppressMessages(library(dplyr))

x <- tbl_df(data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20))) %>% rvtable

x0 <- group_by(x) %>% rvtable
x1 <- group_by(x, id1)  %>% rvtable
x2 <- group_by(x, id1, id2)  %>% rvtable
x3 <- group_by(x, id1, id2, id3)  %>% rvtable

cl <- c("rvtable", "grouped_df", "tbl_df", "tbl", "data.frame")
cl <- list(cl[-2], cl, cl, cl)
lev <- list(id1=LETTERS[1:5], id2=c("high", "low"), id3=1:2)
grps <- purrr::map(c("id1", "id2", "id3"), ~as.name(.x))

xlist <- list(x0, x1, x2, x3)
xlist_sample <- purrr::map(xlist, ~sample_rvtable(.x))
wts <- data.frame(levels=LETTERS[1:5], weights=c(1, 1.5, 2, 4, 1))

suppressWarnings({
xlist_merge1 <- purrr::map(xlist, ~merge_rvtable(.x))
xlist_merge2 <- purrr::map(xlist_sample, ~merge_rvtable(.x))

xlist_margin1 <- purrr::map(xlist, ~marginalize(.x, margin=as.character(groups(.x))))
xlist_margin2 <- purrr::map(xlist_sample, ~marginalize(.x, margin=as.character(groups(.x))))
xlist_margin3 <- purrr::map(xlist, ~marginalize(.x, margin=c("id1", "id2")))
xlist_margin4 <- purrr::map(xlist_sample, ~marginalize(set_weights(.x, "id1", wts), margin="id1"))
})

test_that("grouping ignored/preserved with rvtable", {
  expect_identical(x, x0)
  purrr::walk2(xlist, cl, ~expect_identical(class(.x), .y))
  purrr::walk(xlist, ~expect_identical(get_levels(.x), lev))
  expect_is(groups(x), "NULL")
  expect_identical(groups(x), groups(x0))
  purrr::walk(xlist[-1], ~expect_is(groups(.x), "list"))
  purrr::walk2(xlist[-1], 1:3, ~expect_equal(length(groups(.x)), .y))
  purrr::walk2(xlist[-1], list(grps[1], grps[1:2], grps),  ~expect_identical(groups(.x), .y))
  purrr::walk(xlist[-1], ~expect_identical(dim(.x), dim(x0)))
})

test_that("grouping ignored/preserved with sample_rvtable", {
  purrr::walk2(xlist_sample, cl, ~expect_identical(class(.x), .y))
  purrr::walk(xlist_sample, ~expect_identical(get_levels(.x), lev))
  expect_identical(groups(xlist_sample[[1]]), groups(x0))
  purrr::walk(xlist_sample[-1], ~expect_is(groups(.x), "list"))
  purrr::walk2(xlist_sample[-1], 1:3, ~expect_equal(length(groups(.x)), .y))
  purrr::walk2(xlist_sample[-1], list(grps[1], grps[1:2], grps),  ~expect_identical(groups(.x), .y))
  purrr::walk(xlist_sample[-1], ~expect_identical(dim(.x), dim(xlist_sample[[1]])))
})

test_that("grouping utilized directly and maintained by merge_rvtable", {
  # from distributions vs. from samples
  purrr::walk2(xlist_merge1, xlist_merge2, ~expect_identical(class(.x), class(.y)))
  purrr::walk2(xlist_merge1[1:3], xlist_merge2[1:3], ~expect_identical(dim(.x), dim(.y)))
  expect_identical(ncol(xlist_merge1[[4]]), ncol(xlist_merge2[[4]]))
  purrr::walk2(xlist_merge1, xlist_merge2, ~expect_identical(groups(.x), groups(.y)))

  # merged vs unmerged
  purrr::walk2(xlist_merge1, cl, ~expect_identical(class(.x), .y))
  purrr::walk2(xlist_merge1[-1], list(1, 1:2, 1:3), ~expect_identical(get_levels(.x), lev[.y]))
  expect_identical(groups(xlist_merge1[[1]]), groups(x0))
  purrr::walk(xlist_merge1[-1], ~expect_is(groups(.x), "list"))
  purrr::walk2(xlist_merge1[-1], 1:3, ~expect_equal(length(groups(.x)), .y))
  purrr::walk2(xlist_merge1[-1], list(grps[1], grps[1:2], grps),  ~expect_identical(groups(.x), .y))

  expect_identical(dim(xlist_merge1[[1]]), as.integer(c(512, 2)))
  expect_identical(dim(xlist_merge1[[2]]), as.integer(c(512*5, 2 + 1)))
  expect_identical(dim(xlist_merge1[[3]]), as.integer(c(512*5*2, 2 + 2)))
  expect_identical(ncol(xlist_merge1[[4]]), ncol(xlist[[4]]))
  purrr::walk(xlist_merge2, ~expect_identical(length(groups(.x)), ncol(.x) - 2L))
})

test_that("grouping ignored with marginalize, set to any remaining ID variables", {
  # from distributions vs. from samples
  purrr::walk2(xlist_margin1, xlist_margin2, ~expect_identical(class(.x), class(.y)))
  purrr::walk2(xlist_margin1[2:4], xlist_margin2[2:4], ~expect_identical(dim(.x), dim(.y)))
  expect_identical(ncol(xlist_margin1[[1]]), ncol(xlist_margin2[[1]]))
  purrr::walk2(xlist_margin1, xlist_margin2, ~expect_identical(groups(.x), groups(.y)))

  # marginalized vs unmarginalized
  purrr::walk2(xlist_margin1, rev(cl), ~expect_identical(class(.x), .y))
  purrr::walk2(xlist_margin1[-4], rev(list(3, 2:3, 1:3)), ~expect_identical(get_levels(.x), lev[.y]))
  expect_identical(groups(xlist_margin1[[4]]), groups(x0))
  purrr::walk(xlist_margin1[-4], ~expect_is(groups(.x), "list"))
  purrr::walk2(xlist_margin1[-4], 3:1, ~expect_equal(length(groups(.x)), .y))
  purrr::walk2(xlist_margin1[-4], rev(list(grps[3], grps[2:3], grps)),  ~expect_identical(groups(.x), .y))

  expect_identical(dim(xlist_margin1[[4]]), as.integer(c(512, 2)))
  expect_identical(dim(xlist_margin1[[3]]), as.integer(c(512*2, 2 + 1)))
  expect_identical(dim(xlist_margin1[[2]]), as.integer(c(512*2*2, 2 + 2)))
  expect_identical(dim(xlist_margin1[[1]]), dim(xlist[[1]]))
  purrr::walk(xlist_margin2, ~expect_identical(length(groups(.x)), ncol(.x) - 2L))

  # repeat above using other marginalized rvtables
  purrr::walk(xlist_margin3, ~expect_identical(class(.x), cl[[2]]))
  purrr::walk(xlist_margin3, ~expect_identical(get_levels(.x), lev[3]))
  purrr::walk(xlist_margin3, ~expect_is(groups(.x), "list"))
  purrr::walk(xlist_margin3, ~expect_equal(length(groups(.x)), 1))
  purrr::walk(xlist_margin3,  ~expect_identical(groups(.x), grps[3]))
  purrr::walk(xlist_margin3, ~expect_identical(dim(.x), as.integer(c(512*2, 2 + 1))))
  purrr::walk(xlist_margin3, ~expect_identical(length(groups(.x)), ncol(.x) - 2L))

  # repeat above using other marginalized rvtables
  purrr::walk(xlist_margin4, ~expect_identical(class(.x), cl[[2]]))
  purrr::walk(xlist_margin4, ~expect_identical(get_levels(.x), lev[2:3]))
  purrr::walk(xlist_margin4, ~expect_is(groups(.x), "list"))
  purrr::walk(xlist_margin4, ~expect_equal(length(groups(.x)), 2))
  purrr::walk(xlist_margin4,  ~expect_identical(groups(.x), grps[2:3]))
  purrr::walk(xlist_margin4, ~expect_identical(dim(.x), as.integer(c(512*2*2, 2 + 2))))
  purrr::walk(xlist_margin4, ~expect_identical(length(groups(.x)), ncol(.x) - 2L))
})

set.seed(1)
x <- data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:20, 10) + rnorm(200), Prob=rep(sqrt(1:20), 10)) %>% rvtable

x0 <- group_by(x) %>% rvtable
x1 <- group_by(x, id1)  %>% rvtable
x2 <- group_by(x, id1, id2)  %>% rvtable
x3 <- group_by(x, id1, id2, id3)  %>% rvtable

xlist <- list(x0, x1, x2, x3)
xlist_inverse1 <- purrr::map(xlist, ~inverse_pmf(.x, c(5, 8), "id1", sample.args=list(n=100)))
xlist_inverse2 <- purrr::map(xlist, ~inverse_pmf(.x, c(2, 8), "id2", sample.args=list(n=1000)))
xlist_inverse3 <- purrr::map(xlist, ~inverse_pmf(.x, c(4, 8), "id3", sample.args=list(n=1000)))

test_that("grouping ignored/stripped with inverse_pmf", {
  # comparing different ID variables
  purrr::walk2(xlist_inverse1, xlist_inverse2, ~expect_identical(class(.x), class(.y)))
  purrr::walk2(xlist_inverse1, xlist_inverse2, ~expect_identical(ncol(.x), ncol(.y)))
  purrr::walk2(xlist_inverse1, xlist_inverse2, ~expect_identical(groups(.x), groups(.y)))
  purrr::walk2(xlist_inverse1, xlist_inverse3, ~expect_identical(class(.x), class(.y)))
  purrr::walk2(xlist_inverse1, xlist_inverse3, ~expect_identical(ncol(.x), ncol(.y)))
  purrr::walk2(xlist_inverse1, xlist_inverse3, ~expect_identical(groups(.x), groups(.y)))

  # invrse vs original
  purrr::walk(xlist_inverse1, ~expect_identical(class(.x), cl[[1]]))
  purrr::walk(xlist_inverse1, ~expect_identical(get_levels(.x), lev[1:3]))
  purrr::walk(xlist_inverse1, ~expect_identical(groups(.x), NULL))

  purrr::walk(xlist_inverse1, ~expect_identical(dim(.x), as.integer(c(5*2*2, 2 + 3 - 1))))
  purrr::walk2(xlist_inverse1, xlist, ~expect_identical(ncol(.x), ncol(.y) - 1L))

  purrr::walk(xlist_inverse2, ~expect_true(nrow(.x) <= 5*2*2))
  purrr::walk2(xlist_inverse2, xlist, ~expect_identical(ncol(.x), ncol(.y) - 1L))
  purrr::walk(xlist_inverse3, ~expect_true(nrow(.x) <= 5*2*2))
  purrr::walk2(xlist_inverse3, xlist, ~expect_identical(ncol(.x), ncol(.y) - 1L))
})
