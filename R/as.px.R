#################################################################
# 
# File:         as.px.R
# Purpose:      Generic function for creating px objects from different R data structures
#
# Created:      20110801
# Authors:      cjgb
#
# Modifications: 
#
#################################################################

as.px <- function ( x, ... ){
    UseMethod( "as.px", x )
}

