globalVariables(c(".", "Val", "n", "numer", "denom"))

.has_rv_attributes <- function(x){
  !is.null(attr(x, "rvtype")) &
    !is.null(attr(x, "tabletype")) &
    !is.null(attr(x, "valcol")) &
    #!is.null(attr(x, "probcol")) &
    !is.null(attr(x, "density.args")) &
    !is.null(attr(x, "sample.args"))
}

.lost_rv_class_check <- function(x){
  if(.has_rv_attributes(x) & !("rvtable" %in% class(x))) class(x) <- unique(c("rvtable", class(x)))
  x
}

.add_rvtable_class <- function(x, Val, Prob, discrete, distr, density.args=list(), sample.args=list()){
  if(!distr & !is.null(Prob)) stop("Expected `Prob` to be NULL if tabletype is 'sample'.")
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- ifelse(discrete, "discrete", "continuous")
  attr(x, "tabletype") <- ifelse(distr, "distribution", "sample")
  attr(x, "valcol") <- Val
  attr(x, "probcol") <- Prob
  attr(x, "density.args") <- density.args
  attr(x, "sample.args") <- sample.args
  x
}

#' Check For rvtable Class
#'
#' Check if an object has class rvtable.
#'
#' @param x an R object.
#'
#' @return \code{TRUE} if \code{x} is an rvtable class object, otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' is.rvtable("a")
#' is.rvtable(rvtable(1:10))
is.rvtable <- function(x){
  has_class <- "rvtable" %in% class(x)
  if(has_class & .has_rv_attributes(x)) TRUE else FALSE
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
.rv_class_check <- function(x) if(!is.rvtable(x)) stop("`x` must be an rvtable.") else TRUE

#' Random Variable Table
#'
#' Class constructor for \code{rvtable} objects.
#'
#' These are long format data tables containing a Val and Prob column describing the distribution of a random variable.
#' Any other columns are ID columns and should be categorical variables. The random variable described by Val and Prob may be discrete or continuous.
#' When discrete, probabilities are true probabilities.
#' When continuous, Val and Prob are based on \code{x} and \code{y} output from \code{density} and describe a distribution curve,
#' and therefore values in Prob may be greater than one and may not sum to one.
#' Val is typically numeric but may be character when discrete such as when an rvtable object is returned from \code{inverse_pmf}.
#'
#' For data frame inputs, Val and Prob here refer generally to whatever columns in an rvtable are specified by \code{Val} and \code{Prob}.
#' In \code{rvtable}, if the \code{Val} argument is not supplied, Val is assumed to be \code{Val="Val"} and \code{rvtable}
#' will search the names of a data frame for this column, throwing an error if it is not found, like with any other value of \code{Val}.
#' When \code{Prob} is missing, however, this is analogous to when \code{x} is numeric and
#' \code{y} and \code{x} probability attributes are both NULL: the data in the \code{Val} column are assumed to be a direct sample
#' from a distribution rather than a vector of values that describes a distribution in conjunction with a \code{Prob} column.
#' When \code{x} is numeric, a supplied \code{Val} will substitute for rvtable names \code{x} and \code{y} in the output, respectively.
#'
#' All rvtable objects are in one of two forms: distribution-type or sample-type.
#' This primary constructor constructs distribution- or sample-type rvtable objects,
#' with the corresponding attribute \code{tabletype="distribution"} or \code{tabletype="sample"}.
#' Sampling on an rvtable can generate a sample-type rvtable, with the attribute \code{tabletype="sample"}.
#' Other operations like merging or marginalizing distributions typically yield rvtables in distribution form.
#' This is the common form and rvtables are usually kept in this form until a final step in a processing chain
#' where samples are needed.
#'
#' Every rvtable object also has a variable type attribute, \code{rvtype}, which is either "discrete" or "continuous".
#' Other attributes assigned during rvtable construction include \code{valcol} and \code{probcol}, the names of the Val and Prob columns,
#' and a \code{density.args} attribute that lists any most recent arguments passed to \code{density} in the process
#' of making the rvtable.
#'
#' If an rvtable is already of class \code{rvtable}, the \code{rvtable} function simply returns
#' the rvtable as is; any other arguments passed to \code{rvtable} are ignored and
#' neither the table nor its attributes are updated or altered in any way.
#'
#' @param x a numeric vector, data frame, or data table.
#' @param y an optional vector of probabilities associated with \code{x} when \code{x} is a numeric vector with no similar probabilities vector attribute.
#' @param Val the column name of \code{x} referring to random variable values when \code{x} is a data frame or data table.
#' @param Prob the column name of \code{x} referring to random variable values when \code{x} is a data frame or data table.
#' @param discrete whether the random variable is discrete.
#' @param density.args optional arguments passed to \code{density}.
#' @param force.dist logical, force distribution-type rvtable output if \code{Prob} is missing, i.e., \code{Val} is assumed to be a sample.
#' Defaults to \code{TRUE}.
#'
#' @return an object of class \code{rvtable}.
#' @export
#'
#' @examples
#' # basic samples from continuous and discrete RVs
#' x <- rnorm(100)
#' rvtable(x)
#' rvtable(x, density.args=list(n=50, adjust=2))
#' rvtable(x, discrete=TRUE) # incorrect: x is a continuous RV
#' x <- sample(1:10, size=30, replace=TRUE, prob=sqrt(10:1))
#' rvtable(x, discrete=TRUE) # discrete=T only needed if ambiguous
#' x <- 1:5
#' probs <- c(0.1, 0.2, 0.3, 0.15, 0.25)
#' rvtable(x, y=probs) # discrete inferred from y
#' attr(x, "probabilities") <- probs
#' rvtable(x) # discrete inferred from attributes
#'
#' # an existing data frame or data table
#' x <- data.frame(Val=1:10, Prob=0.1)
#' rvtable(x)
#' library(data.table)
#' x <- data.table(id=rep(LETTERS[1:2], each=10), v1=rep(1:10, 2), p1=c(rep(0.1, 10), sqrt(1:10)))
#' rvtable(x, Val="v1", Prob="p1")
#' @importFrom magrittr %>%
#' @import data.table
#' @importFrom stats approx density
rvtable <- function(x, y=NULL, Val, Prob, discrete=FALSE, density.args=list(), force.dist=TRUE){
  if(missing(x)) stop("`x` is missing.")
  if(is.rvtable(x)) return(x)
  vpmissing <- c(missing(Val), missing(Prob))
  if(vpmissing[1]) Val <- "Val"
  if(vpmissing[2]) Prob <- "Prob"
  if(Val==Prob) stop("`Val` and `Prob` cannot refer to the same column.")
  distr <- !vpmissing[2] | force.dist
  if(!distr) Prob <- NULL
  if(is.numeric(x))
    return(.rvtable_numeric(x, y, Val, Prob, discrete, density.args, force.dist, vpmissing))
  #if(!any(class(x) %in% c("data.table", "data.frame"))) stop("`x` is not a data frame or data table.")
  if(!any(class(x) %in% "data.frame")) stop("`x` is not a data frame.")
  .rvtable_df(x, Val, Prob, discrete, density.args, force.dist, vpmissing)
}

.rvtable_df <- function(x, Val, Prob, discrete, density.args, force.dist, vpmissing){
  distr <- !vpmissing[2] | force.dist
  forced <- vpmissing[2] & force.dist
  grp <- dplyr::groups(x)
  #if(length(class(x))==1 && class(x)!="data.table") x <- data.table::data.table(x)
  if("data.table" %in% class(x)) x <- data.frame(x)
  if(!"tbl_df" %in% class(x)) x <- dplyr::tbl_df(x)
  id <- names(x)
  if(!(Val %in% id)) stop(paste("No column called", Val))
  if(distr && !(Prob %in% id)) stop(paste("No column called", Prob))
  stopifnot((is.numeric(x[[Val]]) || discrete) && !any(is.na(x[[Val]])))
  if(distr){
    stopifnot(is.numeric(x[[Prob]]) && !any(is.na(x[[Prob]])))
    stopifnot(min(x[[Prob]]) >= 0)
  }
  dots <- lapply(id[!(id %in% c(Val, Prob))], as.symbol)
  if(distr){
    if(forced && !Prob %in% id){
      x <- dplyr::group_by_(x, .dots=dots) %>%
        .add_rvtable_class(Val, Prob, discrete, distr, density.args, list()) %>%
        .rvtable_makedist()
    }
    tmp <- (
      dplyr::group_by_(x, .dots=dots) %>%
        dplyr::summarise_(Duplicated=lazyeval::interp(~any(duplicated(var)), var=as.name(Val)))
      )$Duplicated
    if(any(tmp)) stop(paste0("Duplicated values in `", Val, "`."))
  }
  dplyr::group_by_(x, .dots=grp) %>%
    .add_rvtable_class(Val, Prob, discrete, distr, density.args, list())
}

.rvtable_numeric <- function(x, y, Val, Prob, discrete, density.args, force.dist, vpmissing){
  if(any(is.na(x))) stop("Missing values not permitted.")
  if(length(x)==1 && !discrete)
    stop("A single value for `x` with probability=1 is only allowed when discrete=TRUE")
  if(is.null(y)) y <- attr(x, "prob")
  distr <- TRUE
  if(force.dist & is.null(y)){
    if(discrete){
      x <- table(x)
      y <- as.numeric(x/sum(x))
      x <- as.numeric(names(x))
    } else {
      x <- do.call(density, args=c(list(x=x), density.args))
      y <- x$y
      x <- x$x
    }
  } else if(is.null(y)){
    distr <- FALSE
  }

  if(distr && length(x) != length(y))
    stop("Values and probabilities do not have equal length.")
  #x <- if(distr) data.table(x=x, y=y) else data.table(x=x)
  x <- if(distr) data.frame(x=x, y=y) else data.frame(x=x)
  x <- dplyr::tbl_df(x)
  if(vpmissing[1]) Val <- "x"
  if(distr){
    if(vpmissing[2]) Prob <- "y"
    if(any(!vpmissing)) names(x) <- c(Val, Prob)
  } else {
    if(!vpmissing[1]) names(x) <- Val
  }
  dplyr::ungroup(x) %>%
    .add_rvtable_class(Val, Prob, discrete, distr, density.args, list())
}
