#' rvtable helpers and attributes
#'
#' Helper functions for the rvtable class.
#'
#' These helper functions provide information about `rvtable` class objects and their attributes.
#' They get and in some cases set rvtable attributes as well as perform various logical checks.
#' \code{rvatts} returns a named list of attributes of \code{x}. \code{is*} functions return \code{TRUE} or \code{FALSE}.
#' \code{rvtype}, \code{tabletype}, \code{valcol} and \code{probcol} return individual attributes of \code{x} (character).
#'
#' \code{get*} functions for density and sample argument lists return the lists from the attributes of \code{x}
#' and the corresponding \code{set*} functions return \code{x} with the new \code{denisty.args} and \code{sample.args}.
#' Setting density or sample argument lists is usually done when passing arguments to a function, e.g., \code{rvtable}
#' or \code{sample_rvtable}. However, it can sometimes be useful to set these directly or update them at a desired step
#' in a rvtable processing pipeline.
#'
#' @param x rvtable.
#' @param id character, rvtable attribute(s). If not provided, all available rvtable attributes.
#' @param all logical, ignore `id` and return all attributes,
#' including those not specific to the `rvtable` class. Defaults to \code{FALSE}.
#' @name helpers
#'
#' @return information about various rvtable attributes. See details.
#'
#' @examples
#' x <- rvtable(1:10)
#'
#' is_rvtable(x)
#' is_sample(x)
#' is_distribution(x)
#' is_discrete(x)
#' is_continuous(x)
#' is_density(x)
#'
#' rvatts(x)
#' rvatts(x, id=c("rvtype", "tabletype"))
#' rvatts(x, all = TRUE)
#'
#' rvtype(x)
#' tabletype(x)
#' valcol(x)
#' probcol(x)
#'
#' get_density_args(x)
#' get_sample_args(x)
#' x <- set_density_args(x, list(n = 1000, adjust = 0.1))
#' x <- set_sample_args(x, list(n = 100))
#' get_density_args(x)
#' get_sample_args(x)
NULL

#' @export
#' @rdname helpers
rvatts <- function(x, id, all=FALSE){
  .rv_class_check(x)
  atts <- .rvtable_attribute_names()
  smpl <- tabletype(x) == "sample"
  if(smpl) atts <- atts[atts != "probcol"]
  if(missing(id)) id <- atts
  if(smpl & any(id == "probcol"))
    stop("'probcol' is not an attribute of sample-type rvtables.")
  if(any(!id %in% atts)) stop("Invalid attribute name(s) in `id`.")
  x <- attributes(x)
  if(all) return(x)
  x[names(x) %in% id]
}

#' @export
#' @rdname helpers
rvtype <- function(x){
  .rv_class_check(x)
  attr(x, "rvtype")
}

#' @export
#' @rdname helpers
tabletype <- function(x){
  .rv_class_check(x)
  attr(x, "tabletype")
}

#' @export
#' @rdname helpers
is_rvtable <- function(x) "rvtable" %in% class(x) & .has_rv_attributes(x)

#' @export
#' @rdname helpers
is_sample <- function(x) tabletype(x) == "sample"

#' @export
#' @rdname helpers
is_distribution <- function(x) tabletype(x) == "distribution"

#' @export
#' @rdname helpers
is_discrete <- function(x) rvtype(x) == "discrete"

#' @export
#' @rdname helpers
is_continuous <- function(x) rvtype(x) == "continuous"

#' @export
#' @rdname helpers
is_density <- function(x) rvtype(x) == "continuous" & tabletype(x) == "distribution"

#' @export
#' @rdname helpers
valcol <- function(x){
  .rv_class_check(x)
  attr(x, "valcol")
}

#' @export
#' @rdname helpers
probcol <- function(x){
  .rv_class_check(x)
  attr(x, "probcol")
}

#' @export
#' @rdname helpers
idcols <- function(x){
  .rv_class_check(x)
  names(get_weights(x))
}

#' @export
#' @rdname helpers
get_density_args <- function(x){
  .rv_class_check(x)
  attr(x, "density.args")
}

#' @rdname helpers
get_sample_args <- function(x){
  .rv_class_check(x)
  attr(x, "sample.args")
}

#' @export
#' @rdname helpers
set_density_args <- function(x, density.args){
  .rv_class_check(x)
  attr(x, "density.args") <- density.args
  x
}

#' @export
#' @rdname helpers
set_sample_args <- function(x, sample.args){
  .rv_class_check(x)
  attr(x, "sample.args") <- sample.args
  x
}

.rvtable_attribute_names <- function(prob=TRUE){
  x <- c("rvtype", "tabletype", "valcol", "probcol", "weights", "density.args", "sample.args")
  if(prob) x else x[-4]
}

.has_rv_attributes <- function(x)
  all(.rvtable_attribute_names(prob=FALSE) %in% names(attributes(x)))

.lost_rv_class_check <- function(x){
  if(.has_rv_attributes(x) & !(is_rvtable(x)))
    class(x) <- unique(c("rvtable", class(x)))
  x
}

.add_rvtable_class <- function(x, Val, Prob, discrete, distr, weights=list(), density.args=list(), sample.args=list()){
  if(!distr & !is.null(Prob)) stop("Expected `Prob` to be NULL if tabletype is 'sample'.")
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- ifelse(discrete, "discrete", "continuous")
  attr(x, "tabletype") <- ifelse(distr, "distribution", "sample")
  attr(x, "valcol") <- Val
  attr(x, "probcol") <- Prob
  attr(x, "weights") <- .set_all_weights(x, weights, Prob, Val)
  attr(x, "density.args") <- density.args
  attr(x, "sample.args") <- sample.args
  x
}

#' Stop Error Helper Function
#'
#' Return an error if \code{x} is not an rvtable.
#'
#' This helper function is used inside other functions to interrupt when \code{x} is not an rvtable.
#'
#' @param x an R object.
#'
#' @return
#'
#' @examples
#' f <- function(x) .rv_class_check(x)
#' f(1)
.rv_class_check <- function(x) if(!is_rvtable(x)) stop("`x` must be an rvtable.") else TRUE
