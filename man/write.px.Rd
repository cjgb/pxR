\name{write.px}
\alias{write.px}
\title{
Write a PC-Axis file
}
\description{
  
  This function writes an object of class \code{px} to a PC-Axis file 

}
\usage{
write.px(obj.px, filename, heading = NULL, stub = NULL,
         keys = NULL , write.na = FALSE, write.zero = FALSE,
         fileEncoding = "ISO-8859-1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj.px}{The name of a object of class \code{px}}
  \item{filename}{The name of the PC-Axis file to create}
  \item{heading}{An optional character vector with the names of variables in the HEADING part of the output file}
  \item{stub}{An optional character vector with the names of the variables in the STUB part of the output file}
  \item{keys}{An optional character vector indicating the variables used as keys}
  \item{write.na}{Whether to write rows with NA values (if keys are used)}
  \item{write.zero}{Whetehr to write rows with 0 values (if keys are used)}  
  \item{fileEncoding}{A character string describing the encoding to use in px file (see \code{\link{iconv}} for details)}
}

\details{
  The function can write either regular files or files with KEYS.
  
  For regular files, by default, the output file will have a single variable in the HEADING part of the file (columns in the matrix). It 
  is possible to override the default by providing the \code{heading} and \code{stub} parameters. These are optional, non-overlapping, 
  exhaustive character vectors of names of variables in the output matrix. 
  
  It is possible to write files with KEYS which could help reduce the final file size for large and sparse datasets. In such case, it 
  is possible to indicate whether to write rows with all values equal to 0 or NA or not (via arguments \code{write.zero} and ·\code{write.na}).
}

\references{
\url{http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf}
}
\author{
Francisco J. Viciana Fernández, Oscar Perpiñan Lamigueiro, Carlos J. Gil Bellosta
}


\seealso{
  \code{\link{read.px}},  
  \code{\link{as.data.frame.px}},
  \code{\link{as.array.px}}
  \code{\link{iconv}}
}

\examples{
opx1 <- read.px(system.file( "extdata", "example.px", package = "pxR"))  
\dontrun{
	write.px(opx1, file = "opx.px")
	write.px(opx1, file = "opx.px", 
                   heading = c("sexo", "edad"),
                   stub = "municipios")
        write.px(opx1, filename  = "opx.px", 
                 keys = c("municipios","edad")  )
}
}

\keyword{ manip }
