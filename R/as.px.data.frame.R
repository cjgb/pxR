#################################################################
#
# File:         as.px.data.frame
# Purpose:      Converts an data.frame to a px object
#
# Created:      20141209
# Authors:      fvf
#
# Modifications:
#               [141209] fvf
#               20150701 cjgb: transform the df to array 
#                              and use previous, existing function
#################################################################


as.px.data.frame <- function(x, skeleton.px = NULL, list.keys = NULL,
                                value.column = NULL, ...)

{
  value.column <- ifelse(is.null(value.column), "value", value.column)
  if (! value.column %in% colnames(x))
    stop("Error: Column ", value.column, "not found in ", deparse(substitute(x)))
  
  dim.names <- setdiff(colnames(x), value.column)
  x.array   <- acast(x, as.list(dim.names))
  names(dimnames(x.array)) <- dim.names
  
  as.px(x.array, skeleton.px = skeleton.px, list.keys = list.keys, ...) 
}



