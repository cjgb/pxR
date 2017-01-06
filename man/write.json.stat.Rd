\name{write.json.stat}
\alias{write.json.stat}
\title{
Write a px object in JSON-stat format.
}
\description{
  This function writes a object of class \code{px} to a file conforming the JSON-stat standard, a simple lightweight JSON dissemination format best suited for data visualization, mobile apps, or open data initiatives.
}
\usage{
write.json.stat(obj.px, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj.px}{the name of a objest of class \code{px}}
  \item{filename}{the name of the json-stat file to be created (suggested extension, .json)}
}

\references{
  \url{http://json-stat.org/}
}
\author{
  Carlos J. Gil Bellosta
}

\seealso{
  \code{\link{write.px}}
}
\examples{
\dontrun{
  opx1 <- read.px(system.file( "extdata", "example.px", package = "pxR"))  
  write.json.stat(opx1, file = "opx.px")
}
}

\keyword{ manip }
