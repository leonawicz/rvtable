#' rvtable helpers
#'
#' @param x rvtable.
#' @param id character, rvtable attribute(s). If not provided, all available rvtable attributes.
#' @param all logical, ignore `id` and return all attributes,
#' including those not specific to the `rvtable` class. Defaults to \code{FALSE}.
#'
#' @return a named list of attributes.
#' @export
#'
#' @examples
#' x <- rvtable(1:10)
#' rvatts(x)
#' rvatts(x, id=c("rvtype", "tabletype"))
#' rvatts(x, all=TRUE)
rvatts <- function(x, id, all=FALSE){
  .rv_class_check(x)
  atts <- .rvtable_attribute_names()
  if(is.null(attr(x, "probcol"))) atts <- atts[atts != "probcol"]
  if(missing(id)) id <- atts
  if(any(!id %in% atts)) stop("Invalid attribute name(s) in `id`.")
  print(id)
  if(any(id == "probcol")) stop("'probcol' is not an attribute of sample-type rvtables.")
  x <- attributes(x)
  if(all) return(x)
  x[names(x) %in% id]
}
# rvtype()
# tabletype()
# is_sample()
# is_distribution()
# is_continuous()
# is_discrete()
# is_density()
# valcol()
# probcol()
# density_args()
# sample_args()

.rvtable_attribute_names <- function()
  c("rvtype", "tabletype", "valcol", "probcol", "density.args", "sample.args")
