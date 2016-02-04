#' Random Variable Table
#'
#' Class constructor for \code{rvtable} objects.
#'
#' These are long format data tables containing a Val and Prob column describing the distribution of a random variable.
#' Any other columns are ID columns and should be categorical variables. The random variable described by Val and Prob may be discrete or continuous.
#' When discrete, probabilities are true probabilities.
#' When continuous, Val and Prob are based on \code{x} and \code{y} output from \code{density} and describe a distribution curve,
#' and therefore values in Prob may be greater than one and may not sum to one.
#' All rvtable objects are either distribution-based or sample-based.
#' This primary constructor only constructs distribution-based rvtable objects, with the attribute \code{tabletype="distribution"}.
#' Sampling on an rvtable can generate a sample-based rvtable, with the attribute \code{tabletype="sample"}.
#' Every rvtable object also has a variable type attribute, \code{rvtype}, which is either "discrete" or "continuous".
#'
#' @param x a numeric vector, data frame, or data table.
#' @param y an optional vector of probabilities associated with \code{x} when \code{x} is a numeric vector with no similar probabilities vector attribute.
#' @param Val the column name of \code{x} referring to random variable values when \code{x} is a data frame or data table.
#' @param Prob the column name of \code{x} referring to random variable values when \code{x} is a data frame or data table.
#' @param discrete whether the random variable is discrete.
#' @param density.args optional arguments passed to \code{density}.
#'
#' @return an S3 object of class \code{rvtable}
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
rvtable <- function(x, y=NULL, Val="Val", Prob="Prob", discrete=FALSE, density.args=list()){
  if(missing(x)) stop("`x` is missing.")
  if("rvtable" %in% class(x)) return(x)
  if(is.numeric(x)){
    if(any(is.na(x))) stop("Missing values not permitted.")
    if(length(x)==1 && !discrete) stop("A single value for `x` with probability=1 is only allowed when discrete=TRUE")
    if(is.null(y)) y <- attr(x, "prob")
    if(is.null(y)){
      if(discrete){
        x <- table(x)
        y <- as.numeric(x/length(x))
        x <- as.numeric(names(x))
      } else {
        x <- do.call(density, args=c(list(x=x), density.args))
        y <- x$y
        x <- x$x
      }
    }
    if(length(x) != length(y)) stop("Values and probabilities do not have equal length.")
    x <- data.table(Val=x, Prob=y)
  }
  if(!any(class(x) %in% c("data.table", "data.frame"))) stop("`x` is not a data frame or data table.")
  if(length(class(x))==1 && class(x)=="data.frame") x <- data.table(x)
  if(Val==Prob) stop("`Val` and `Prob` cannot refer to the same column.")
  id <- names(x)
  if(!(Val %in% id) && !("Val" %in% id)) stop(paste("No column called", Val))
  if(!(Prob %in% id) && !("Prob" %in% id)) stop(paste("No column called", Prob))
  if(Val %in% id && Val != "Val") names(x)[id==Val] <- "Val"
  if(Prob %in% id && Prob != "Prob") names(x)[id==Prob] <- "Prob"
  stopifnot(is.numeric(x$Val) && is.numeric(x$Prob))
  stopifnot(!any(is.na(x$Val)) && !any(is.na(x$Prob)))
  stopifnot(min(x$Prob) >= 0)
  #stopifnot(max(x$Prob) <= 1)
  dots <- lapply(id[!(id %in% c("Val", "Prob"))], as.symbol)
  tmp <- (dplyr::group_by_(x, .dots=dots) %>% dplyr::summarise_(Duplicated=~any(duplicated(Val))))$Duplicated
  if(any(tmp)) stop("Duplicated values in `Val`.")
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- ifelse(discrete, "discrete", "continuous")
  attr(x, "tabletype") <- "distribution"
  x
}
