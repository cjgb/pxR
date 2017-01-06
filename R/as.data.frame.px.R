#################################################################
# 
# File:         as.data.frame.px.R 
# Purpose:      extracts a df from a px object
#
# Created:      20110801
# Authors:      cjgb, opl, fvf
#
# Modifications: 
#    20120323, cjgb: added error check: variables and codes should have the same length
#    20120402, cjgb: warnings can be either errors or warnings depending on paranoia level
#    20120402, cjgb: adapted to the new px object format (where DATA is already a df)
#    20141222. fvf:  bug in "wide" direction
#################################################################

as.data.frame.px <- function( x, ..., use.codes = FALSE, warnings.as.errors = TRUE, direction = "long"){

  dat <- x$DATA$value  # already a data frame
  
  ## maybe we need to change values to codes
  if (is.logical(use.codes) && use.codes)
    use.codes <- names(x$CODES)
  
  if (! is.logical(use.codes))
    for( var.name in intersect( use.codes, intersect(colnames(dat), names(x$CODES) ) ) )
        dat[[var.name]] <- mapvalues(dat[[var.name]], 
                                     from = x$VALUES[[var.name]], 
                                     to   = x$CODES[[var.name]])
     
  ## do we need to reshape?
  if (direction == "wide")
    # fvf.20121222: The order of variables rows and pivots-columns was inverted
    # dcast(dat, list(x$HEADING$value, x$STUB$value))
    dcast(dat, list(x$STUB$value,x$HEADING$value))
  else
    dat
}

