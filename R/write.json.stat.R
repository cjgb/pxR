#################################################################
# 
# File:         write.json.stat.R
# Purpose:      Write an object of class 'px' a file with json-stat format
#               See http://json-stat.org/
#
# Created:      20130813
# Authors:      cjgb
#
# Modifications: 
#
#################################################################


write.json.stat <- function ( obj.px, filename ){
  
  if ( ! inherits( obj.px, "px" ) )
    stop("Error: object needs to have class 'px'" )
  
  aa <- as.array(obj.px)

  js.dim.id <- names(dimnames(aa))
  
  js.dim.size <- sapply(dimnames(aa), length)
  names(js.dim.size) <- NULL
  
  js.dim.ids <- lapply( dimnames(aa), 
                 function(x){
                   list( category = list( index = x ) )
                   
                 }
  )
  
  js.value <- as.numeric(aa)
  
  js.updated <- as.character(Sys.time())
  js.source  <- obj.px$SOURCE$value
  js.label   <- obj.px$TITLE$value
  
  
  js <- list( label     = js.label,
              source    = js.source,
              updated   = js.updated,
              value     = js.value,
              dimension = c( list( id   = js.dim.id,
                                   size = js.dim.size ),
                             
                             js.dim.ids 
                             
              )
  )
  
  js <- list(js)
  names(js) <- obj.px$MATRIX$value
  
  
  # encoding must be UTF-8 for json files
  con <- file( description = filename, open = "w", encoding = "UTF-8" )
  on.exit( close(con) )
  
  cat(toJSON(js), file = con)
  
}  

