#' rvtable helpers and attributes
#'
#' Helper functions for the rvtable class.
#'
#' These helper functions provide information about \code{rvtable} class objects and their attributes.
#' They get and in some cases set rvtable attributes as well as perform various logical checks.
#' \code{rvattr} returns a named list of attributes of \code{x}. \code{is*} functions return \code{TRUE} or \code{FALSE}.
#' \code{rvtype}, \code{tabletype}, \code{valcol} and \code{probcol} return individual attributes of \code{x} (character).
#'
#' \code{get*} functions for density and sample argument lists return the lists from the attributes of \code{x}
#' and the corresponding \code{set*} functions return \code{x} with the new \code{denisty.args} and \code{sample.args} attributes.
#' Setting density or sample argument lists is usually done when passing arguments to a function, e.g., \code{rvtable}
#' or \code{sample_rvtable}. However, it can sometimes be useful to set these directly or update them at a desired step
#' in a rvtable processing pipeline. Note that when defaults are not explicitly provided (i.e., leaving as \code{list()} or \code{NULL}),
#' the atrributes of an rvtable will still display all sampling default arguments and the most important \code{density}
#' default arguments for clarity.
#'
#' @param x rvtable.
#' @param id character, rvtable attribute(s). If not provided, all available rvtable attributes.
#' @param all logical, ignore `id` and return all attributes,
#' including those not specific to the `rvtable` class. Defaults to \code{FALSE}.
#' @param density.args set rvtable \code{density} arguments. See details.
#' @param sample.args set rvtable sampling arguments. See details.
#' @name helpers
#'
#' @return information about various rvtable attributes. See details.
#'
#' @examples
#' \dontrun{
#' x <- rvtable(1:10)
#'
#' is_rvtable(x)
#' is_sample(x)
#' is_distribution(x)
#' is_discrete(x)
#' is_continuous(x)
#' is_density(x)
#'
#' rvattr(x)
#' rvattr(x, id=c("rvtype", "tabletype"))
#' rvattr(x, all = TRUE)
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
#' }
NULL

#' @export
#' @rdname helpers
rvattr <- function(x, id, all=FALSE){
  .rv_class_check(x)
  atts <- .rvtable_attribute_names()
  if(missing(id)) id <- atts
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
coltypes <- function(x){
  .rv_class_check(x)
  attr(x, "coltypes")
}

#' @export
#' @rdname helpers
valcol <- function(x){
  .rv_class_check(x)
  attr(x, "coltypes")$values
}

#' @export
#' @rdname helpers
probcol <- function(x){
  .rv_class_check(x)
  attr(x, "coltypes")$probs
}

#' @export
#' @rdname helpers
idcols <- function(x){
  .rv_class_check(x)
  attr(x, "coltypes")$ids
}

#' @export
#' @rdname helpers
get_density_args <- function(x){
  .rv_class_check(x)
  attr(x, "density.args")
}

#' @export
#' @rdname helpers
get_sample_args <- function(x){
  .rv_class_check(x)
  attr(x, "sample.args")
}

#' @export
#' @rdname helpers
set_density_args <- function(x, density.args){
  .rv_class_check(x)
  attr(x, "density.args") <- .update_default_args(density.args, "density")
  x
}

#' @export
#' @rdname helpers
set_sample_args <- function(x, sample.args){
  .rv_class_check(x)
  attr(x, "sample.args") <- .update_default_args(sample.args, "sample")
  x
}

.rvtable_attribute_names <- function(drop=NULL){
  x <- c("rvtype", "tabletype", "coltypes", "weights", "density.args", "sample.args")
  if(is.null(drop)) x else x[!x %in% drop]
}

.has_rv_attributes <- function(x, ...)
  all(.rvtable_attribute_names(...) %in% names(attributes(x)))

.lost_rv_class_check <- function(x){
  if(.has_rv_attributes(x) && (!(is_rvtable(x)) || class(x)[1] != "rvtable"))
    class(x) <- unique(c("rvtable", class(x)))
  x
}

.update_default_args <- function(user_args, type, drop_nulls=TRUE){
  if(!type %in% c("density", "sample")) stop("`type` must be 'density' or 'sample'.")
  if(type == "density"){
    x <- list(n=512, adjust=1, from=NULL, to=NULL, bw="nrd0", kernel="gaussian")
    if(drop_nulls) x <- x[!names(x) %in% c("from", "to")]
  }
  if(type == "sample")
    x <- formals(sample_rvtable)[c("n", "interp", "n.interp", "decimals")]
  for(i in names(x)) if(!i %in% names(user_args)) user_args[[i]] <- x[[i]]
  user_args
}

.add_rvtable_class <- function(x, Val, Prob, discrete, distr, weights=list(),
                               density.args=list(), sample.args=list(),
                               not_ids=c("Val", "Prob", "weights_")){
  density.args <- .update_default_args(density.args, "density")
  sample.args <- .update_default_args(sample.args, "sample")
  if(!distr & !is.null(Prob))
    stop("Expected `Prob` to be NULL if tabletype is 'sample'.")
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- ifelse(discrete, "discrete", "continuous")
  attr(x, "tabletype") <- ifelse(distr, "distribution", "sample")
  attr(x, "density.args") <- density.args
  attr(x, "sample.args") <- sample.args
  id <- names(x)[!names(x) %in% c(Val, Prob, not_ids)]
  if(!length(id)) id <- NULL
  attr(x, "coltypes") <- list(values=Val, probs=Prob, ids=id)
  weights <- if(is.null(id)) list(x=1)[0] else .set_all_weights(x, weights, Val, Prob)
  attr(x, "weights") <- weights
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
