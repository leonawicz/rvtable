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

#marginalize(x, c("id1", "id2"))
#get_levels(x, "id1")
#marginalize(x, "id1", weights=c(1, 1.5, 2, 4, 1))

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
