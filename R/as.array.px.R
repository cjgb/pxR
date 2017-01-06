#################################################################
# 
# File:         as.array.px.R
# Purpose:      Converts a px object to an array
#
# Created:      20110801
# Authors:      fvf
#
# Modifications: opl, fvf (20130618)
#               cjgb, 20130813: adapted to the new internal structure
#
#################################################################

as.array.px <- function(x, use.codes = FALSE,... ){
  
  # extract the df component of the px object and
  # use code already in place in as.data.frame 
  # for codes
  df <- as.data.frame(x, use.codes = use.codes)
  
  # use all cols but "value" as dimensions in the output array
  dim.names <- setdiff(colnames(df), "value")
  res       <- acast(df, as.list( dim.names ))
  names( dimnames(res) ) <- dim.names
  res
}


