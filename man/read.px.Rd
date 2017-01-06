\name{read.px}
\alias{read.px}
\title{
Reads a PC-Axis file
}
\description{
This function reads a PC-AXIS file (a text file with certain format) and creates an object of the class px.
}
\usage{
read.px(filename, encoding = NULL, 
        na.strings = c('"."', '".."', '"..."', '"...."', '":"'))
}

\arguments{
  \item{filename}{ The name of the PC-Axis file to read }
  \item{encoding}{A character string describing the current encoding; see the Details section}
  \item{na.strings}{A character to be interpreted as missing value in
    the \code{DATA} field of the PC-AXIS file. The chapter 4 of the
    second reference provides details about the dot codes used in the
    PC-AXIS format.}
}
\details{
This function reads data files in the PC-Axis format.
The format is described in the two documents in the reference section.

According to them, null values can be encoded using ".", "..", "...", or "....". 

Also, the documentation is not very specific concerning the actual encoding to be expected in
PC-Axis files.
It is however quite safe to assume that they will be encoded using some "latin1" variant.
The alternative value for the \code{encoding} argument would be "utf-8".
}
\value{
An object of the class \code{px}, which is essentially a list comprising the
fields of a PC-AXIS file (see references for details).
}
\references{
  \url{http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf}
  \url{http://tilastokeskus.fi/tup/pcaxis/tiedostomuoto2006_laaja_en.pdf}
}
\author{
Carlos J. Gil Bellosta, Oscar Perpiñan Lamigueiro, Francisco J. Viciana Fernández
}
% \note{
%  ~~further notes~~
% }
\seealso{
  \code{\link{as.data.frame.px}},
  \code{\link{as.array.px}}
  \code{\link{iconv}}
}
\examples{

my.px.object <- read.px(system.file("extdata", "example.px",
                        package = "pxR") )
my.px.data   <-  as.data.frame( my.px.object ) 

}

\keyword{ manip }
