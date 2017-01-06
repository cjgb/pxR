#################################################################
# 
# File:         read.pxini.R
# Purpose:      non-exported function for reading .ini files associated
#		with .shp files for depicting maps
#
# Created:      20111210
# Authors:      etm, op, cjgb
#
# Modifications: 
#
#################################################################


read.pxini <- function(filename, shpPath = 'shp',  encoding = "latin1", ...){

    if( ! grepl(".ini$", filename) ) 
        filename <- paste(filename, ".ini", sep = "")

    filename <- file.path( shpPath, filename )      # Platform independent path construction

    a <- scan(filename, what = "character", sep = "\n",
                quiet = TRUE, fileEncoding = encoding)

    foo <- function( pattern, strings ){
        tmp <- strings[grep(pattern, strings, ignore.case = T)]
        gsub(pattern, "", tmp)
    }

    filesshp  <- foo("^[0-9]*=", a)
    filesshp  <- gsub(".*\\\\", "", filesshp)		# Maybe not too platform independent
    filesshp  <- file.path(shpPath, filesshp)

    # Note: the code assumes that maps and keyfields are listed in the same order in the .ini file

    list( 
            nummaps   = as.numeric(foo("NumMaps=", a)),
            keyfields = foo("KeyField=", a),
            filesshp  = filesshp
    )
}


