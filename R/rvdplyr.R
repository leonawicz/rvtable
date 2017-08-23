#' Preserving rvtable class and attributes with dplyr
#'
#' Methods for the \code{rvtable} class are provided for a number of common generic \code{dplyr} methods.
#'
#' Functions in the \code{dplyr} package tend to strip custom classes and attributes from data frames.
#' These rvtable methods preserve the \code{rvtable} class as the primary class and preserve all rvtable-specific attributes.
#'
#' Note that some operations inherently destroy an rvtable. For example, the resulting data frame cannot be an rvtable
#' after using \code{select} to drop the values column.
#' Constructing an rvtable generally suggests that most other operations that manipulate the data frame have been completed;
#' there should be little reason to do anything other than possibly use \code{group_by}, \code{ungroup} or \code{arrange}
#' and still have use for the result as an rvtable specifically.
#' \code{filter}, \code{slice}, \code{select}, \code{mutate}, \code{summarise} and \code{distinct} should be used with caution.
#' Methods will make attempts to explicitly coerce output to a tibble instead of preserving the rvtable class and associated attributes
#' when a change in the data frame that ruins the meaningfulness of an rvtable is detected.
#'
#' For example, if a dplyr operation removes the values column, the result will be a simple tibble, not an rvtable.
#' Similarly, if the original rvtable was in distribution form, but a dplyr function strips the probabilities column,
#' the result is left as a tibble and the \code{rvtable} class and associated attributes are not preserved.
#' If all observations for an ID variable are removed with \code{filter} or \code{slice},
#' the attribute entries for that ID variable in  \code{coltypes$idcols} and \code{weights} are removed.
#' Nevertheless, despite modest efforts to protect against misuse, it remains incredibly easy to create bogus rvtables
#' by applying various \code{dplyr} operations indiscriminantly.
#' These \code{rvtable} methods are provided for convenience only.
#' It is recommended to only construct rvtables from data frames that clearly conform to rvtable requirements and expectations.
#' If hacking at an rvtable with dplyr functions, consideration should be given to whether the result is still meaningful as an rvtable.
#'
#' The \code{rvtable} package exports its own generic methods for the above functions.
#' Therefore, if strict \code{dplyr} methods for tibbles is preferred over \code{rvtable} class methods when working with \code{rvtable} objects,
#' just load \code{library(dplyr)} after \code{library(rvtable)} to override the \code{rvtable} versions.
#'
#' @param .data rvtable.
#' @param ... additional arguments.
#' @name rvdplyr
#'
#' @return an rvtable.
NULL

.replace_rvatts <- function(x, a){
  anames <- names(a)
  id <- names(x)
  v <- a$coltypes$values
  p <- a$coltypes$probs
  ids <- a$coltypes$ids
  allcols <- c(ids, v, p)

  if(!v %in% id) return(x) # check val/prob/id columns, weights
  if(!is.null(p) && !p %in% id) return(x)
  if(!is.null(ids)){
    ids_idx <- which(ids %in% id)
    a$coltypes$ids <- if(length(ids_idx)) ids[ids_idx] else NULL
    a$weights <- if(length(ids_idx)) a$weights[ids_idx] else list(x=1)[0]
  }
  newcols <- which(!id %in% allcols) # check for new columns/add ids/weights
  if(length(newcols)){
    for(i in id[newcols]){
      a$coltypes$ids <- c(a$coltypes$ids, i)
      tmp <- x[[i]]
      tmp <- if(is.factor(tmp)) levels(tmp) else unique(tmp)
      a$weights[[i]] <- tibble::data_frame(levels=tmp, weights=1)
    }
  }

  xattnames <- names(attributes(x))
  for(i in seq_along(a)){
    if(!anames[i] %in% xattnames) attr(x, anames[i]) <- a[[i]]
  }
  .lost_rv_class_check(x)
}

#' @rdname rvdplyr
#' @export
arrange <- function (.data, ...) {
  UseMethod("arrange", .data)
}

#' @rdname rvdplyr
#' @export
filter <- function (.data, ...) {
  UseMethod("filter", .data)
}

#' @rdname rvdplyr
#' @export
slice <- function (.data, ...) {
  UseMethod("slice", .data)
}

#' @rdname rvdplyr
#' @export
select <- function (.data, ...) {
  UseMethod("select", .data)
}

#' @rdname rvdplyr
#' @export
mutate <- function (.data, ...) {
  UseMethod("mutate", .data)
}

#' @rdname rvdplyr
#' @export
summarise <- function (.data, ...) {
  UseMethod("summarise", .data)
}

#' @rdname rvdplyr
#' @export
summarize <- function (.data, ...) {
  UseMethod("summarise", .data)
}

#' @rdname rvdplyr
#' @export
distinct <- function (.data, ..., .keep_all = FALSE) {
  UseMethod("distinct", .data)
}

#' @rdname rvdplyr
#' @export
group_by <- function (.data, ..., add = FALSE) {
  UseMethod("group_by", .data)
}

#' @rdname rvdplyr
#' @export
ungroup <- function (x, ..., .keep_all = FALSE) {
  UseMethod("ungroup", x)
}

#' @export
arrange.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::arrange(.data, ...), a)
}

#' @export
filter.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::filter(.data, ...), a)
}

#' @export
slice.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::slice(.data, ...), a)
}

#' @export
select.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::select(.data, ...), a)
}

#' @export
mutate.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::mutate(.data, ...), a)
}

#' @export
summarise.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::summarise(.data, ...), a)
}

#' @export
summarize.rvtable <- function(.data, ...) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::summarise(.data, ...), a)
}

#' @export
distinct.rvtable <- function(.data, ..., .keep_all = FALSE) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::distinct(.data, ..., .keep_all = .keep_all), a)
}

#' @export
group_by.rvtable <- function(.data, ..., add = FALSE) {
  a <- rvattr(.data)
  class(.data) <- class(.data)[-1]
  .replace_rvatts(dplyr::group_by(.data, ..., add = add), a)
}

#' @export
ungroup.rvtable <- function(x, ...) {
  a <- rvattr(x)
  class(x) <- class(x)[-1]
  .replace_rvatts(dplyr::ungroup(x, ...), a)
}
