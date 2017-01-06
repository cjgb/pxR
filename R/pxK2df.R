#################################################################
# 
# File:         pxK2df.R
# Purpose:      non-exported function.
#               For use in "as.array.px" and "as.data.frame.px" 
#		            Extracted px$DATA into a data.frame 
#                
# Created:      20130611
# Authors:      fvf
#
# Modifications: 
#
#################################################################
                                        
pxK2df <- function (px, use.codes = FALSE)  {
  names(px$KEYS) -> names.keys
  no.keys <- names(px$VALUES)[-match(names(px$KEYS),names(px$VALUES))]
  if (use.codes) {
     for (i in names.keys) {
       if (i %in% names(px$CODES)) levels(px$DATA$value[[i]])<-px$CODES[[i]]
     }
     for (i in no.keys) {
      if (i %in% names(px$CODES)) levels(px$DATA$datakeys[[i]])<-px$CODES[[i]]
     }
  }
  ne   <- dim(px$DATA$datakeys)[1]
  ni   <- length(px$DATA$value$datanum)
  ndim <- length(px$DATA$value)-1
  for (ii in 1:ni) {
    tmp <-list()
    for (i in names.keys) {
      tmp  <- c (tmp , list(rep(px$DATA$value[[i]][ii],ne)))
    }
    names(tmp)<- names.keys
    tmp <- as.data.frame(c(tmp,
                           px$DATA$datakeys))
    tmp <- cbind(tmp,dat=px$DATA$value$datanum[[ii]])
    if (ii==1) {
      df.data <- tmp } else { df.data <- rbind(df.data,tmp) }
  }
  return (df.data)
}
