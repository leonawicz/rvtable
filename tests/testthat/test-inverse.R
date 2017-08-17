context("inverse")

suppressMessages(library(dplyr))

sum_to_one <- function(x, grp){
  purrr::map2(
    x, grp,
    ~abs((group_by_(.x, .dots=.y) %>% summarise(Sum=sum(Prob1)))$Sum - 1) < 1e-15) %>%
    unlist %>% all
}

set.seed(1)
x <- data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val1=rep(1:20, 10) + rnorm(200), Prob1=rep(sqrt(1:20), 10)) %>%
  rvtable(Val="Val1", Prob="Prob1")

cl <- "rvtable"
y1 <- inverse_pmf(x, c(5, 8), "id1", sample.args=list(n=100))
y2 <- inverse_pmf(x, c(5, 8), "id2", sample.args=list(n=1000))
y3 <- inverse_pmf(x, c(5, 8), "id3", sample.args=list(n=1000))
ylist <- list(y1, y2, y3)
n <- c(nlevels(x$id1), nlevels(x$id2), length(unique(x$id3)))

test_that("inverse_pmf on continuous RV returns correctly", {
  purrr::walk(ylist, ~expect_is(.x, cl))
  purrr::walk2(ylist, c(20, 8, 8), ~expect_equal(nrow(.x), .y))
  purrr::walk2(ylist, c("id1", "id2", "id3"), ~expect_identical(attr(.x, "valcol"), .y))
  purrr::walk(ylist, ~expect_identical(attr(.x, "probcol"), "Prob1"))

  suppressWarnings(
    expect_warning(
      y <- inverse_pmf(x, c(50, 80), "id3", sample.args=list(n=1000)),
      warning(paste0("'", "id3",
        "' has probability zero over the given value range of the primary random variable."))
    )
  )
  expect_equal(nrow(y), 0)
  expect_identical(y$Prob1, numeric())
  purrr::walk(ylist, ~expect_identical(class(.x$id1), class(y$id1)))
  purrr::walk(ylist, ~expect_identical(class(.x$id2), class(y$id2)))
  purrr::walk(ylist, ~expect_identical(class(.x$id3), class(y$id3)))
  purrr::walk(ylist, ~expect_identical(class(.x$Prob1), class(y$Prob1)))

  expect_true(sum_to_one(ylist, list(c("id2", "id3"), c("id1", "id3"), c("id1", "id2"))))
})

set.seed(1)
x <- data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val1=1:40, Prob1=sqrt(1:40)) %>%
  rvtable(Val="Val1", Prob="Prob1", discrete=TRUE)

y1 <- inverse_pmf(x, c(15, 25), "id1", sample.args=list(n=100))
y2 <- inverse_pmf(x, c(5, 12), "id2", sample.args=list(n=10000))
y3 <- inverse_pmf(x, c(27, 35), "id3", sample.args=list(n=1000))
ylist <- list(y1, y2, y3)

x2 <- data.frame(
  id1=c("A", "B"),
  Val=c(1, 1, 2, 2),
  Prob=c(0.25, 0.5, 0.75, 0.5)) %>% rvtable(discrete=TRUE)
y1 <- inverse_pmf(x2, 1, "id1")
y2 <- inverse_pmf(x2, 2, "id1")
y3 <- inverse_pmf(x2, 1:2, "id1")
ylist <- c(ylist, list(y1, y2, y3))

test_that("inverse_pmf on discrete RV returns correctly", {
  purrr::walk(ylist, ~expect_is(.x, cl))
  purrr::walk2(ylist, c(20, 8, 12, rep(2, 3)), ~expect_equal(nrow(.x), .y))
  purrr::walk2(ylist, c("id1", "id2", "id3", rep("id1", 3)), ~expect_identical(attr(.x, "valcol"), .y))
  purrr::walk2(ylist, rep(c("Prob1", "Prob"), each=3), ~expect_identical(attr(.x, "probcol"), .y))

  suppressWarnings(
    expect_warning(
      y <- inverse_pmf(x, c(50, 80), "id3", sample.args=list(n=1000)),
      warning(paste0("'", "id3",
                     "' has probability zero over the given value range of the primary random variable."))
    )
  )
  expect_equal(nrow(y), 0)
  expect_identical(y$Prob1, numeric())
  purrr::walk(ylist, ~expect_identical(class(.x$id1), class(y$id1)))
  purrr::walk(ylist[1:3], ~expect_identical(class(.x$id2), class(y$id2)))
  purrr::walk(ylist[1:3], ~expect_identical(class(.x$id3), class(y$id3)))
  purrr::walk(ylist[1:3], ~expect_identical(class(.x$Prob1), class(y$Prob1)))
  purrr::walk(ylist[4:6], ~expect_identical(class(.x$Prob), class(y$Prob1)))

  expect_true(sum_to_one(ylist[1:3], list(c("id2", "id3"), c("id1", "id3"), c("id1", "id2"))))
  expect_true(all(purrr::map(ylist[4:6], ~sum(.x$Prob)) == 1))
})

set.seed(1)
x3 <- rvtable(data.frame(x1=1:10, id=rep(LETTERS[1:2], each =5)), Val="x1",
              discrete=TRUE, force.dist=FALSE)
x4 <- rvtable(data.frame(x1=1:10, id=rep(LETTERS[1:2], each =5)), Val="x1",
              density.args=list(n=10000, adjust=0.01), force.dist=FALSE)
ylist2 <- list(
  inverse_pmf(x3, c(2, 4), "id"),
  inverse_pmf(x3, c(2, 8), "id"),
  inverse_pmf(x4, c(2, 4), "id"),
  inverse_pmf(x4, c(2, 8), "id"))

test_that("inverse_pmf returns consistent, correct values", {
  purrr::walk(ylist2, ~expect_is(.x, cl))
  purrr::walk2(ylist2, rep(2, 4), ~expect_equal(nrow(.x), .y))
  purrr::walk2(ylist2, rep("id", 4), ~expect_identical(attr(.x, "valcol"), .y))
  purrr::walk2(ylist2, rep("Prob", 4), ~expect_identical(attr(.x, "probcol"), .y))

  expect_true(abs(ylist2[[1]]$Prob[1] - ylist2[[3]]$Prob[1]) < 0.01)
  expect_true(abs(ylist2[[1]]$Prob[2] - ylist2[[3]]$Prob[2]) < 0.01)
  expect_true(abs(ylist2[[2]]$Prob[1] - ylist2[[4]]$Prob[1]) < 0.02)
  expect_true(abs(ylist2[[2]]$Prob[2] - ylist2[[4]]$Prob[2]) < 0.02)
  expect_true(all(purrr::map(ylist2, ~sum(.x$Prob)) == 1))
})

test_that("inverse_pmf handles errors correctly", {
  x <- rvtable(data.frame(Val=1:10, Prob=1:10), discrete=TRUE)

  values_err <- "discrete `values` must be a single value or valid range."
  expect_error(inverse_pmf(x, c(2, 4)), "`id` missing.")
  expect_error(inverse_pmf(x, c(2, 4, 5), "id"), values_err)
  expect_error(inverse_pmf(x, NULL, "id"), values_err)
  expect_error(inverse_pmf(x, c(2, 1), "id"), values_err)
  expect_error(inverse_pmf(x, c(2, 2), "id"), "id not found.")
  expect_error(inverse_pmf(x, c(2, 4), "id"), "id not found.")

  x <- rvtable(data.frame(Val=1:10, Prob=1:10))

  values_err <- "continuous `values` must be a valid range."
  expect_error(inverse_pmf(x, c(2, 4)), "`id` missing.")
  expect_error(inverse_pmf(x, c(2, 4, 5), "id"), values_err)
  expect_error(inverse_pmf(x, NULL, "id"), values_err)
  expect_error(inverse_pmf(x, c(2, 1), "id"), values_err)
  expect_error(inverse_pmf(x, c(2, 2), "id"), values_err)
  expect_error(inverse_pmf(x, c(2, 4), "id"), "id not found.")
})
