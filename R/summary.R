#################################################################
# 
# File:         summary.R
# Purpose:      creates a summary of a px object
#
# Created:      20110630
# Authors:      cjgb
#
# Modifications: 
#
#################################################################

summary.px <- function( object, ... ){
    cat( "\nSummary of metadata:\n" )
    str( object[ which( names( object ) != "DATA" ) ] )

    cat( "\nSummary of data:\n" )
    str( as.data.frame( object ) )
}

