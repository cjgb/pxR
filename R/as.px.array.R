#################################################################
#
# File:         as.px.array
# Purpose:      Converts an array to a  px object
#
# Created:      20110801
# Authors:      fvf, cjgb, opl
#
# Modifications:
#                 [130624] fvf: Check consistency between CODES and VALUES
#                 20150701 cjgb: eliminate check for array: it is an array because of dispatching  
#
#################################################################


as.px.array  <- function ( x, skeleton.px = NULL, list.keys = NULL, ...  ){

  ## auxiliary functions
  
  mkl1 <- function(key, value){
    zz <- list(list(value = value))
    names(zz) <- key
    zz
  }
  
  ## default skeleton
  
  if (is.null(opx <- skeleton.px))
    opx <- c( mkl1('CHARSET'      , 'ANSI'),
              mkl1('MATRIX'       , 'file000'),
              mkl1('AXIS-VERSION' , '2000'),
              mkl1('SUBJECT-CODE' , 'xx'),
              mkl1('SUBJECT-AREA' , 'unknown'),
              mkl1('CONTENTS'     , 'unknown'),
              mkl1('UNITS'        , 'unknown'),
              mkl1('TITLE'        , 'Title unknown'),
              mkl1('DECIMALS'     , 0),
              mkl1('CREATION-DATE', format(Sys.time(), "%Y%m%d %H:%M:%S")),
              mkl1('LAST-UPDATED' , format(Sys.time(), "%Y%m%d %H:%M:%S"))  
              )

  ## Add key-value pair if list.keys<>NULL
  if (! is.null(list.keys)) 
    for (key in names(list.keys))
      opx[key] <- list(list(value = list.keys[[key]]))
      
  # delete STUB, HEADING, VALUES and DATA if present
  opx['STUB'] <- opx['HEADING'] <- opx['VALUES'] <- opx['DATA'] <- NULL
  
  dd <- length(dim(x))
  if (dd > 1) {
    # First dim to 'header', the rest in reverse order
    opx[["STUB"]]    <- list(value = rev(names(dimnames(x))[2:dd]))
    opx[["HEADING"]] <- list(value = names(dimnames(x))[1]) 
  } 
  else 
    opx[["STUB"]] <- list(value = rev(names(dimnames(x))))  # Only one dim
  
  opx$VALUES      <- dimnames(x)
  opx$DATA$value  <- as.data.frame(ftable(x))
  colnames(opx$DATA$value) <- c(names(dimnames(x)), "value")
  
  #  Delete skeleton.px$CODES: unused or inconsistent
  
  if (! is.null(opx$CODES)){
    
    new.codes <- opx$CODES
    opx$CODES <- list()
    
    new.codes <- new.codes [names(new.codes) %in% names(opx$VALUES)]
    
    new.codes <- new.codes[
      sapply(new.codes, length) ==
        sapply(opx$VALUES[names(new.codes)] ,length)
      ]
    
    opx$CODES <- new.codes
  }
  
  class( opx ) <- "px"
  
  opx
}

# ### example
# oo  <- read.px(  system.file( "extdata", "example.px", package = "pxR"))
# aa  <- as.array(oo)
# as.px(aa)->px.aa1
# as.px(aa,skeleton.px=oo )->px.aa2
# as.array(px.aa1)->aa1

# ### export data checks
# sum(as.array(aa)-as.array(px.aa1))
# sum(as.array(aa)-as.array(px.aa2))

# ### Checks writing for missing data
# oo  <- read.px(  system.file( "extdata", "example2.px", package = "pxR"))
# aa  <-  as.array(oo)
# aa[sample(1:length(aa),5)]<-NA
# write.px(as.px.array(aa),file='tmpzxc01.px')

# ### append and modify keys
# write.px(as.px(aa,skeleton.px=oo),file='tmpzxc02.px')
# write.px(as.px(aa,
#              list.keys= list(MATRIX='xxx', CONTENTS='new data',
#                              NEWKEY='an other key',
#                              UNITS='people', TITLE='My Title')
#                      ),file='tmpzxc03.px')
#
# file.remove('tmpzxc01.px','tmpzxc02.px','tmpzxc03.px')
