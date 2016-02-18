context("merging")

x <- data.table(
 id1=rep(LETTERS[1:5], each=4),
 id2=factor(c("low", "high")),
 id3=rep(1:2, each=2),
 Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20))

x.c <- rvtable(x)
x.d <- rvtable(x, discrete=TRUE)

x.c.merge <- dplyr::group_by(x.c, id2, id3) %>% merge_rvtable()
x.d.merge <- dplyr::group_by(x.d, id3) %>% merge_rvtable()

x.c.margin <- marginalize(x.c, c("id1", "id3"))
x.c.margin.w <- marginalize(x.c, c("id1"), weights=c(1, 1.5, 2, 4, 1))
x.d.margin <- marginalize(x.d, c("id2"))
x.d.margin.w <- marginalize(x.d, c("id1"), weights=c(1, 1.5, 2, 4, 1))

test_that("get_levels returns correctly", {
  cl <- "list"
  expect_is(get_levels(x.c), cl)
  expect_is(get_levels(x.d), cl)
  expect_is(get_levels(x.c, "id2"), cl)
  expect_is(get_levels(x.d, "id3"), cl)
  expect_error(get_levels(x.c, "a"), "`variable` not found.")
})

test_that("merge_rvtable returns correct class object", {
  cl <- "rvtable"
  expect_is(merge_rvtable(x.c), cl)
  expect_is(merge_rvtable(x.d), cl)
  expect_is(merge_rvtable(x.c, density.args=list(n=50), sample.args=list(n=100)), cl)
  expect_is(merge_rvtable(x.d, density.args=list(n=50), sample.args=list(n=100)), cl)
  expect_is(x.c.merge, cl)
  expect_is(x.d.merge, cl)
})

test_that("groups work with merge_rvtable", {
  expect_true(nrow(x.c.merge) > nrow(x.d.merge))
  expect_true(nrow(x.d.merge) > nrow(merge_rvtable(x.d)))
  expect_identical(as.character(dplyr::groups(x.c.merge)), c("id2", "id3"))
  expect_identical(as.character(dplyr::groups(x.d.merge)), c("id3"))
})

test_that("marginalize returns correct class object", {
  cl <- "rvtable"
  expect_is(x.c.margin, cl)
  expect_is(x.c.margin.w, cl)
  expect_is(x.d.margin, cl)
  expect_is(x.d.margin.w, cl)
  expect_is(marginalize(x.d, c("id1"), weights=c(1, 1.5, 0, 0, 0)), cl)
})

test_that("groups are ignored by marginalize", {
  y1 <- dplyr::group_by(x.c, id1, id2, id3) %>% marginalize(c("id1", "id3"))
  y2 <- dplyr::group_by(x.d, id2) %>% marginalize(c("id2"))
  expect_identical(dim(x.c.margin), dim(y1))
  expect_identical(attributes(x.c.margin), attributes(y1))
  expect_identical(dim(x.d.margin), dim(y2))
  expect_identical(attributes(x.d.margin), attributes(y2))
})

test_that("marginalize throws correct errors", {
  e <- c("Must specify variable(s) to marginalize over.",
         "May only marginalize over one variable at a time if using level weights.",
         "Marginalizing variable not found.",
         "Invalid marginalizaing variable.",
         "Number of weights does not match the number of levels in `margin`.")
  expect_error(marginalize(x.c), e[1], fixed=TRUE)
  expect_error(marginalize(x.d), e[1], fixed=TRUE)
  expect_error(marginalize(x.c, c("id1", "id2"), weights=2), e[2])
  expect_error(marginalize(x.d, c("id1", "id2"), weights=2), e[2])
  expect_error(marginalize(x.c, c("ABC", "DEF"), weights=2), e[2])
  expect_error(marginalize(x.d, "ABC"), e[3])
  expect_error(marginalize(x.c, "Val"), e[4])
  expect_error(marginalize(x.d, "Prob"), e[4])
  expect_error(marginalize(x.c, "id1", weights=c(1, 1.5, 2, 4)), e[5])
  expect_error(marginalize(x.d, "id1", weights=c(1, 1.5, 2, 4, 1, 5)), e[5])
})
