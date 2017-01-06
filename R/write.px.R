#################################################################
# 
# File:         write.px.R
# Purpose:      Write an object of class 'px' to a PC-Axis file 
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#               fvf (20130618)
#               cjgb, 20130811: added on.exit hook to close the open connection
#               cjgb, 20130811: fixed encoding issues (testing)
#               cjgb, 20130813: we do not generate files using KEYS (i.e., if present,
#                         the KEYS part needs to be ignored)
#               cjgb, 20131117: some users want to specify heading & stub precisely
#                         in order to control the shape of the resulting matrix
#               fvf,  20141222: Allows use of "KEYS" for the export of sparse matrices.
#                               Use CODES values, if any, to identify rows.
#                               Fixed a bug related to iconv
#################################################################


write.px <- function ( obj.px, filename, heading = NULL, stub = NULL,
                       keys = NULL , write.na = FALSE, write.zero = FALSE ,
                       fileEncoding = "ISO-8859-1" )
{
  
  if ( ! inherits( obj.px, "px" ) )
    stop("Error: object needs to have class 'px'")
  
  ## auxiliary functions ## 
  unquote <- function(x)
    gsub( '^"|"$', "", x)           # removes " at beginning/end of string
  
  requote <- function(x)            # adds dquotes (") to character vectors
    paste( '"', unquote(x), '"', sep = "")
    
  wf <- function(...){
    cadena <- paste(..., sep = "")
    #cadena <- iconv(cadena, to = my.encoding)
    cat(cadena, file = con, sep = "") 
  }
  
  ## end - auxiliary functions
  
  # modify px object providing sensible default values
  obj.px[['LAST.UPDATED']]$value <- format(Sys.time(), "%Y%m%d %H:%M:%S")
  obj.px[['CHARSET']]$value      <- ifelse(fileEncoding == "ISO-8859-1", 'ANSI', fileEncoding)
  obj.px$INFO$value              <- "File generated using R and package pxR (http://pxr.r-forge.r-project.org/)"

  # Using KEYS Allowed: as parameter or object property

  if (!is.null(keys)) {
    obj.px$KEYS <- NULL  ## Redefine KEYS si existen
                         ## CODES si hay, si no VALUES
    
    if ( ! all(keys %in% names(obj.px$VALUES)))    {
        c('Error: Some keys are not in VALUES')
    }
    
    kk <- lapply(keys,function(e) { 'VALUES' })
    names(kk) <- keys    
    for (i in keys[keys %in% names(obj.px$COD)]) {
      kk[[i]]<- 'CODES'
      levels(obj.px$DATA[[1]][,i]) <- obj.px$CODES[[i]]
      ## Change by defaut ("levels"): VALUES by CODES: 
    }    
    obj.px$KEYS <- kk
  }
  
  # CREATION-DATE:
  
  if  ( is.null(obj.px$'CREATION-DATE')  | is.null(obj.px$'CREATION.DATE') ) {
    obj.px$'CREATION-DATE'$value <- format(Sys.time(),'%Y%m%d %H:%m')
  }
  
  
  # obj.px names may have changed (and got - changed into . by R)
  # we need to revert that  
  names(obj.px) <- gsub("\\.", "-", names(obj.px))
  
  # we want to write the output file keywords in the 'right' order
 
  order.kw <- c("CHARSET", "AXIS-VERSION", "CODEPAGE", "LANGUAGE",
                "LANGUAGES", "CREATION-DATE", "NEXT-UPDATE", "PX-SERVER",
                "DIRECTORY-PATH", "UPDATE-FREQUENCY", "TABLEID", "SYNONYMS",
                "DEFAULT-GRAPH", "DECIMALS", "SHOWDECIMALS", "ROUNDING",
                "MATRIX", "AGGREGALLOWED", "AUTOPEN", "SUBJECT-CODE",
                "SUBJECT-AREA", "CONFIDENTIAL", "COPYRIGHT", "DESCRIPTION",
                "TITLE", "DESCRIPTIONDEFAULT", "CONTENTS", "UNITS", "STUB",
                "HEADING", "CONTVARIABLE", "VALUES", "TIMEVAL", "CODES",
                "DOUBLECOLUMN", "PRESTEXT", "DOMAIN", "VARIABLE-TYPE",
                "HIERARCHIES", "HIERARCHYLEVELS", "HIERARCHYLEVELSOPEN",
                "HIERARCHYNAMES", "MAP", "PARTITIONED", "ELIMINATION", "PR",
                "ECISION", "LAST-UPDATED", "STOCKFA", "CFPRICES", "DAYADJ",
                "SEASADJ",  "CONTACT", "REFPERIOD", "BASEPERIOD",
                "DATABASE", "SOURCE", "SURVEY", "LINK", "INFOFILE",
                "FIRST-PUBLISHED", "META-ID", "OFFICIAL-STATISTICS", "INFO",
                "NOTEX", "NOTE", "VALUENOTEX", "VALUENOTE", "CELLNOTEX",
                "CELLNOTE", "DATASYMBOL1", "DATASYMBOL2", "DATASYM", "BOL3",
                "DATASYMBOL4", "DATASYMBOL5", "DATASYMBOL6", "DATASYMBOLSUM",
                "DATASYMBOLNIL", "DATANOTECELL", "DATANOTESUM", "DATANOTE",
                "KEYS", "ATTRIBUTE-ID", "ATTRIBUTE-TEXT", "ATTRIBUTES",
                "PRECISION","DATA")
 
  order.px  <- charmatch(names(obj.px), order.kw, nomatch=999)   # keyword no in list to end
  new.order <- setdiff( names(obj.px)[order(order.px)], "DATA" ) # all but "DATA"
  
  if(! is.null(heading)){
    if(is.null(stub))
      stop("If heading is specified, you need also specify the stub parameter.")
    
    if(! setequal(c(heading, stub), c(obj.px$HEADING$value, obj.px$STUB$value)) )
      stop("Specified heading and stub parameters differ from those in the px object")
    
    obj.px$HEADING$value <- heading
    obj.px$STUB$value    <- stub
  }

  # If there is a "KEYS" key, then 
  # change HEADING and STUB to fit KEYS

  if (! is.null(obj.px$KEYS)) {
        
    keys    <- names(obj.px$KEYS)
    values  <- names(obj.px$VALUES)    
    no.keys <- values[! (values %in% keys) ]
    
    obj.px$STUB$value     <- keys
    obj.px$HEADING$value  <- no.keys
  }
 
  ## open the connection and close automatically on exit
  con <- file( description = filename, open = "w", encoding = ifelse(fileEncoding == "ISO-8859-1", "latin1", fileEncoding))
  on.exit(close(con))
  
  ## metadata part
  for (key in new.order ) {

    if (length(obj.px[[key]]) == 0)
      next				# this fixes a bug where length(CODES) == 0    
    
    # these are exceptions: no quotes
    # e.g.: 'DECIMALS=0;'
     ## ELIMINATION("~~~~")=YES is a diferente exceptions fvf: (20141222)
    
    if (key %in% c('DECIMALS', 'SHOWDECIMALS', 
                   'COPYRIGHT', 'DESCRIPTIONDEFAULT', 'DAYADJ', 'SEASADJ')){
      wf( key, "=")
      wf( unquote(obj.px[[key]]$value) )
      wf(';\n')
      next
    }
    
    # most metadata entries are like this: 
    # 'KEY="a","b";'
    if ( names(obj.px[[key]])[1] == 'value' ) {  
      wf( key, "=")
      wf( paste( requote(obj.px[[key]]$value), collapse = ',') )
      wf(';\n')
      next
    } 
    
    # meta with second name; there can be more than one
    for (subkey in names(obj.px[[key]])){
      wf( key, '("', subkey, '")=' )
      
      # ELIMINATION is here: fvf (20141222)
      # for some keys, it uses quotes; for others (YES/NO), it does not.
      # KEYS is another exception: the argument is not quoted
            
      if ( (key =='ELIMINATION' && obj.px[[key]][[subkey]] %in% c('YES','NO')) || key == 'KEYS' )
          wf(obj.px[[key]][[subkey]]) 
        else
          wf(paste(requote(obj.px[[key]][[subkey]]), collapse = ',')) 

      wf ( ';\n' )              
    }     
  }
 
  # DATA part:  KEYS are allowed 20141222 -------------------------------
  
  wf('DATA=\n')
  
  if (!is.null(obj.px$KEYS)) {
    
     keys   <- names(obj.px$KEYS)
     values <- names(obj.px$VALUES)
     
     fm <- formula( paste(  
              paste(keys, collapse = '+'),'~', 
              paste(values[!values %in% keys], collapse = '+'), sep=''))    
    
     # levels KEYS to CODES or VALUES:
     for (i in keys) {
       if (obj.px$KEYS[[i]]=='CODES') {
         levels(obj.px$DATA[[1]][,i]) <- obj.px$CODES[[i]] 
       } else 
         levels(obj.px$DATA[[1]][,i]) <- obj.px$VALUES[[i]]        
     }
          
     res <- dcast(obj.px$DATA[[1]], fm, sum)    # keys first, then data in columns
     with.data <- rep(TRUE, nrow(res))          # rows to keep
     
     no.keys <- names(res)[!names(res) %in% keys]   # columns with no keys
     
     data.no.keys <- as.matrix(res[,no.keys])
     
     if (!write.na)
       with.data <- with.data & ! apply(data.no.keys, 1, function(x) all(is.na(x)))
     if (!write.zero)
       with.data <- with.data & ! apply(data.no.keys, 1, function(x) all(x == 0))
 
     res <- res[with.data, ]   
     
     zz <- res[, keys[1]] 

     for (i in keys[-1]) {
       zz <- paste(zz, res[,i],sep='","')
     }
     
     zz <- paste('"',zz,'",',sep='')
     
     
     for (i in names(res)[!names(res) %in% keys] ) {
        column.num <- formatC(res[,i],
                        format = 'f',
                        digits = as.numeric(obj.px$DECIMALS$value),
                        drop0trailing = T, flag = '-')
        column.num <- gsub("NA", '".."', column.num)        
        zz <- paste(zz, column.num, sep=' ')
        
     }
    
     write(zz, file = con, ncolumns = 1, append = T )
     
  } else {
      zz <- formatC(as.array(obj.px),
                  format = 'f',
                  digits = as.numeric(obj.px$DECIMALS$value),
                  drop0trailing = T, flag = '-')
      #zz <- str_trim(zz) 
      zz <- gsub("NA", '".."', zz)  
      zz <- aperm(zz, c(rev(obj.px$HEADING$value), rev(obj.px$STUB$value)))
      write(zz, file = con, ncolumns = sum( dim(zz)[1:length(obj.px$HEADING$value)] ), append = T )
  }
  
  wf(";\n")
  
  invisible(NULL)
}  

