context("weights")

x <- rvtable(data.frame(
  id1=rep(LETTERS[1:5], each=4),
  id2=factor(c("low", "high")),
  id3=rep(1:2, each=2),
  Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)))

test_that("get/set weights and marginalize/merge correct", {
  w <- get_weights(x)
  expect_is(w, "list")
  expect_equal(length(w), 3)
  expect_is(w[[1]], "tbl_df")
  expect_equal(nrow(w[[1]]), 5)

  expect_equal(length(get_weights(x, "id1")), 1)
  expect_equal(length(get_weights(x, c("id1", "id2"))), 2)

  x <- set_weights(x, "id1", data.frame(levels=LETTERS[1:5], weights=1:5))
  expect_identical(get_weights(x, "id1")[[1]]$weights, 1:5)

  wts1 <- data.frame(levels=LETTERS[1:5], weights=c(1, 1.5, 2, 4, 1))
  wts2 <- data.frame(levels=c("low", "high"), weights=c(1, 2))
  wts3 <- data.frame(levels=c(1, 2), weights=3:4)
  ids <- c("id1", "id2", "id3")
  x <- set_weights(x, ids, list(wts1, wts2, wts3))

  xwts <- list(wts1, wts2, wts3)
  names(xwts) <- ids
  expect_identical(get_weights(x), xwts)

  xm1 <- marginalize(x, "id1")
  xm2 <- marginalize(x, "id2")
  xm32 <- marginalize(x, c("id3", "id2"))
  xmlist <- list(xm1, xm2, xm32)

  purrr::walk2(xmlist, c(4, 4, 3), ~expect_equal(ncol(.x), .y))
  purrr::walk2(xmlist, list(c("id2", "id3"), c("id1", "id3"), "id1"), ~expect_true(all(.y %in% names(.x))))

  expect_identical(tbl_df(wts1), get_weights(xm32, "id1")[[1]])

  suppressWarnings(
    expect_warning(xm <- merge_rvtable(x), warning(
      "Ungrouped ID variable levels have unequal weights. Consider `marginalize` instead of `merge_rvtable`."
      )
    )
  )
  expect_is(xm, "rvtable")
  expect_identical(dim(xm), c(512L, 2L))
  expect_true(all(c("Val", "Prob") %in% names(xm)) & !any(ids %in% names(xm)))
})

test_that("get/set weights errors", {
  xvp <- rvtable(data.frame(Val=1:10, Prob=1:10))
  expect_warning(
    set_weights(xvp, id="id1", weights=data.frame(levels=1:2, weights=1:2)),
    "`x` has no ID variables. Weights not set."
  )

  expect_error(
    set_weights(x, id="id1", weights=data.frame(levels=1:2, weights=1:2)),
    "`weights` data frame has wrong number of rows."
  )
  expect_error(
    set_weights(x, id="id1", weights=data.frame(levels=1:5, weights=1:5)),
    "Invalid levels in `weights`."
  )
  expect_error(
    set_weights(x, id=c("id1", "id2"), weights=data.frame(levels=1:5, weights=1:5)),
    "`weights` must be a list of data frames when `id` is a vector."
  )
  expect_error(
    set_weights(x, id=c("id1", "id2"), weights=list(NULL, NULL, data.frame(levels=1:5, weights=1:5))),
    "`id` and `weights` must have equal length."
  )
  expect_error(
    set_weights(x, id="id2", weights=list(NULL, NULL)),
    "`id` and `weights` must have equal length."
  )
  expect_error(
    set_weights(x, id="id2", weights=list(NULL, NULL)),
    "`id` and `weights` must have equal length."
  )
  expect_error(
    set_weights(x, id="id2", weights=5),
    "`weights` must be NULL, a data frame, or list of data frames."
  )

  expect_error(
    set_weights(x, id="id2", weights=list(5)),
    "`weights` list elements must be a data frame if not NULL."
  )
  expect_error(
    set_weights(x, id="id2", weights=list(data.frame(x=1))),
    "`weights` data frame must have two columns: `levels` and `weights`."
  )
  expect_error(
    set_weights(x, id="id2", weights=list(data.frame(x=1, y=2))),
    "`weights` data frame must have two columns: `levels` and `weights`."
  )
  expect_error(
    set_weights(x, id="id2", weights=list(x=data.frame(x=1, y=2))),
    "All `weights` names must be `x` ID column names."
  )
  expect_error(
    set_weights(x, id=c("id1", "id2", "id3"),
                weights=list(
                  id3=data.frame(),
                  id2=data.frame(levels=c("low", "high"), weights=1:2))),
    "`id` and `weights` must have equal length."
  )

  expect_identical(set_weights(x, id="id2", weights=NULL), set_weights(x, id="id2", weights=list()))
  expect_identical(
    set_weights(x, id="id2", weights=list()), set_weights(x, id="id2", weights=list(NULL))
  )

  x <- data.frame(
    id1=rep(LETTERS[1:5], each=4),
    id2=factor(c("low", "high")),
    id3=rep(1:2, each=2),
    Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20))

  expect_error(
    rvtable(x, weights=list(1)),
    "`weights` list must be a named list in `rvtable`."
  )
  expect_error(
    rvtable(x, weights=list(a=1, b=2, c=3)),
    "`id` must contain only valid ID variables in `x`."
  )
  expect_error(
    rvtable(x, weights=list(id1=2)),
    "`weights` list elements must be a data frame if not NULL."
  )
  expect_error(
    rvtable(x, weights=list(id1=data.frame(1))),
    "`weights` data frame must have two columns: `levels` and `weights`."
  )
  expect_error(
    rvtable(x, weights=list(id1=data.frame(levels=1, weights=1))),
    "`weights` data frame has wrong number of rows."
  )
  expect_error(
    rvtable(x, weights=list(id1=data.frame(levels=1:5, weights=1:5))),
    "Invalid levels in `weights`."
  )
  expect_error(
    rvtable(x, weights=list(id1=data.frame(levels=LETTERS[1:5], weights=c(-1, 2:5)))),
    "Weights must be non-negative."
  )
  expect_error(
    rvtable(x, weights=list(id1=data.frame(levels=LETTERS[1:5], weights=LETTERS[1:5]))),
    "Weights must be numeric."
  )

  set.seed(1)
  x1 <- rvtable(x, weights=list(id1=data.frame(levels=LETTERS[1:5], weights=c(1:5))))
  set.seed(1)
  x2 <- rvtable(x) %>% set_weights(id="id1", list(id1=data.frame(levels=LETTERS[1:5], weights=c(1:5))))
  expect_identical(x1, x2)
})
