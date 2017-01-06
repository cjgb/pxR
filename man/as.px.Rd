\name{as.px}
\alias{as.px}
\alias{as.px.array}
\alias{as.px.data.frame}

\title{ Convert an array or a data.frame into an object of class px}

\description{
  This function converts an array object into a \code{px} object. It uses the
  \code{dimnames} of the array to fill the \code{VALUES} list of the
  \code{px} object. It can also converts an data.frame object into a
  \code{px} object. Optionally it uses an object of type \code{px} as the
  skeleton of the new object. } 


\usage{
  as.px(x,... )
  \S3method{as.px}{array}(x, skeleton.px = NULL, list.keys = NULL, ...)
  \S3method{as.px}{data.frame}(x, skeleton.px = NULL,
                             list.keys = NULL, value.column = NULL,  ...)
}

\arguments{
  \item{x}{An array or \code{data.frame} with the data required to build the \code{px} object}
  \item{skeleton.px}{A \code{px} object with metadata that will be used in the new \code{px} object}
  \item{list.keys}{A optional list of metadata pairs}
  \item{value.column}{Optional name to numeric value column in data.frame; defaults to "value"}    
  \item{...}{ Other arguments that can be passed to the function }
}
\details{
  
  A \code{px} object is the internal representation in R of a PC-Axis
  file. As such, it contains both data and metadata.

  The data to populate the \code{px} object comes from \code{x}, the
  first argument of the function. If \code{x} is an array, it needs
  to have 'named \code{dimnames}', i.e., its \code{dimnames} attribute
  needs to be a list with named entries.

  \code{x} can also be a molten \code{data.frame}, i.e., each row contains a single 
  value and the remaining columns indicate the levels of the variables it refers to.
  The ts \code{names} will be used in STUB and HEDING keyword of px objects. The first 
  column of \code{x} will be used as 'HEADING', and the rest, in reverse, order as 'STUB'. 
  
  There are two compatible methods to provide metadata information. The
  first one is via \code{skeleton.px}, a \code{px} object whose metadata
  (title, etc.) is to be recycled in the new object. In addition to
  that, \code{list.keys} can be used to pass metadata in key-value pairs
  as shown in the examples below.

  Metadata provided in \code{list.keys} has priority over metadata
  coming from \code{skeleton.px}. Also, note that the metadata items
  used to name the variables and their labels are neither extracted from
  \code{skeleton.px} nor \code{px} but from the \code{dimnames} attribute 
  of \code{x}.

  Note that the keywords 'HEADING', 'VALUES' and 'DATA', if present in \code{skeleton.px} or
  \code{list.keys}, are ignored. Inconsistent 'CODES' in \code{skeleton.px} will also be
  ignored.

  Few checks are made to guarantee that metadata keys conform to the PC-Axis standard.
} 
\value{ a \code{px} object }

\author{Francisco J. Viciana, Carlos J. Gil Bellosta,}


\seealso{
  \code{\link{write.px}},
  \code{\link{as.array.px}}
}
\examples{

my.px.object  <- read.px(system.file("extdata", "example.px", package = "pxR"))
my.data       <- as.array(my.px.object)
my.px.object2 <- as.px(my.data)
my.px.object3 <- as.px(my.data, skeleton.px = my.px.object) 
my.px.object4 <- as.px(my.data, list.keys = list(MATRIX = "xxx", CONTENTS = "new data",
                             NEWKEY = "another key", UNITS = "people", TITLE = "My Title"))
                             
my.px.df      <- as.data.frame(my.px.object)
my.px.object5 <- as.px(my.px.df)
}

\keyword{ manip }

