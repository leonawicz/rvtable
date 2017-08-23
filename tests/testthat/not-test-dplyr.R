context("dplyr")

x <- data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable

x0 <- group_by(x)
x1 <- group_by(x, id1)
x2 <- group_by(x, id1, id2)
x3 <- group_by(x, id1, id2, id3)
xlist <- list(x0, x1, x2, x3)

cl <- c("rvtable", "grouped_df", "tbl_df", "tbl", "data.frame")
cl <- list(cl[-2], cl, cl, cl)
lev <- list(id1=LETTERS[1:5], id2=c("high", "low"), id3=1:2)
grps <- purrr::map(c("id1", "id2", "id3"), ~as.name(.x))

test_that("rvtable class and attributes preserved", {
  clg <- c("grouped_df", "tbl_df", "tbl", "data.frame")

  purrr::walk2(xlist, cl, ~expect_identical(class(.x), .y))
  expect_identical(class(ungroup(x2)), cl[[1]])
  expect_identical(class(group_by(x2)), cl[[1]])
  expect_identical(class(group_by(x2, id1, id2, id3)), cl[[4]])
  expect_identical(class(filter(x3, id1=="A")), cl[[4]])
  expect_identical(class(slice(x3, 1:8)), cl[[4]])
  expect_identical(class(select(x0, -id2)), cl[[1]])
  expect_identical(class(arrange(x3, id1)), cl[[4]])

  expect_identical(class(mutate(x3, id4=1)), cl[[4]])
  xnew <- mutate(x3, id4=1:10)
  expect_identical(length(get_weights(xnew)), 4L)
  expect_identical(names(get_weights(xnew)), paste0("id", 1:4))
  expect_identical(nrow(get_weights(xnew)[[4]]), 10L)
  expect_identical(length(get_levels(xnew)), 4L)
  expect_identical(length(idcols(xnew)), 4L)
  expect_identical(idcols(xnew)[[4]], "id4")
  expect_identical(dim(xnew), dim(x3) + c(0L, 1L))

  expect_identical(class(summarise(x3, Sum=sum(Val))), clg)

  expect_identical(class(distinct(x0)), cl[[1]])
})
