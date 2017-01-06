\name{summary.px}
\alias{summary.px}
\alias{summary}

\title{
summary.px
}
\description{
Compactly display the internal structure of a px object.
}
\usage{
\method{summary}{px}( object, ... )
}
\arguments{
  \item{object}{ a px object }
  \item{...}{ Additional arguments, currently not used }
}
% \details{
% TBA
% }
\value{
A verbose description of data and metadata within the px object.
}

\references{
http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf
}

\author{
Carlos J. Gil Bellosta
}
% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.px}}
  \code{\link{as.data.frame.px}}
}
\examples{
my.px.object <- read.px( system.file( "extdata", "example.px", package = "pxR") )
summary( my.px.object )
}
\keyword{ manip }
