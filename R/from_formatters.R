# ## Changelog
# nocov start
# styler: off

setGeneric("obj_label", function(obj) standardGeneric("obj_label"))

#' The new label
#' @param value character(1). The new label
#' @export
setGeneric("obj_label<-", function(obj, value) standardGeneric("obj_label<-"))

#' @exportMethod obj_label
setMethod("obj_label", "ANY", function(obj) attr(obj, "label"))

#' @exportMethod obj_label<-
setMethod(
  "obj_label<-", "ANY",
  function(obj, value) {
    attr(obj, "label") <- value
    obj
  }
)

with_label <- function (x, label)
{
  obj_label(x) <- label
  x
}

# nocov end
# styler: on
