\name{as.array.px}
\alias{as.array.px}
\title{Extraction of data from px objects into arrays }
\description{
This function extracts the data component from a px object as a \code{array}.
}

\usage{
\S3method{as.array}{px}(x, use.codes = FALSE, ...)
}
\arguments{
  \item{x}{A \code{px} object.}
  \item{use.codes}{A logical value to indicate whether to use CODES
    instead of VALUES as  names of the levels: TRUE to use  CODES,
    defaut FALSE.
    Or a vector with the names of the variables
    that use CODES to identify levels  }
  \item{...}{Additional arguments. Not used yet.}
}
\value{An array}
\references{
http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf
}
\seealso{
  \code{\link{read.px}},
  \code{\link{as.data.frame.px}}
}
\examples{
my.px     <- read.px( system.file( "extdata", "example.px", package = "pxR"))
my.array  <- as.array( my.px )
my.array2  <- as.array( my.px ,use.codes=TRUE)
my.array3  <- as.array( my.px ,use.codes=c('municipios'))
}
\author{Francisco J. Viciana}
\keyword{manip}

