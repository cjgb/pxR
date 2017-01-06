# PC-Axis and the pxR package

[![Build Status](https://travis-ci.org/cjgb/pxR.svg?branch=master)](https://travis-ci.org/cjgb/pxR)

[PC-Axis](http://www.scb.se/Pages/StandardNoLeftMeny____314045.aspx) is a software family consisting of a number of programs for the Windows and Internet environment used to present statistical information. It is used by national and international institutions to publish statistical data.

Programs in the PC-Axis family use a particular data file format (see the full [PX-Axis data format description](http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf) or this other [technical document](http://tilastokeskus.fi/tup/pcaxis/tiedostomuoto2006_laaja_en.pdf)). The pxR package provides a set of functions for reading and writing PC-Axis files. This will facilitate the analysis of statistical data to the R community.

## Reading PC-Axis files into R

Function `read.px` reads a PC-Axis file from a given location and returns an object of class `px` containing all the data and metadata in the original PC-Axis file.

The single most important piece of infomation within a `px`object is the data matrix, which can be extracted into a R data.frame using function `as.data.frame`.
For instance,

```
my.px.object <- read.px( "/path/to/pc-axis/file" )
my.px.data   <-  as.data.frame( my.px.object ) 
```

will create the data.frame `my.px.data` with the data in the corresponding PC-Axis file.
