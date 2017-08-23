#' Preserving rvtable class and attributes with dplyr
#'
#' Methods for the rvtable class are provided for a number of common generic \code{dplyr} methods.
#'
#' Functions in the \code{dplyr} package tend to strip custom classes and attributes from data frames.
#' These rvtable methods preserve the \code{rvtable} class as the primary class and preserve all rvtable-specific attributes.
#'
#' @param .data rvtable.
#' @param ... additional arguments.
#' @name rvdplyr
#'
#' @return an rvtable.
NULL

.replace_rvatts <- function(x, a){
  anames <- names(a)
  xattnames <- names(attributes(x))
  for(i in seq_along(a)){
    if(!anames[i] %in% xattnames) attr(x, anames[i]) <- a[[i]]
  }
  .lost_rv_class_check(x)
}

#' @rdname rvdplyr
#' @export
arrange.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::arrange(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
filter.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::filter(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
slice.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::slice(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
mutate.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::mutate(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
summarise.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::summarise(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
summarize.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::summarise(.data, ...), a)
}

#' @rdname rvdplyr
#' @export
distinct.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::distinct(.data, ...), a)
}
