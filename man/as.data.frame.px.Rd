\name{as.data.frame.px}
\alias{as.data.frame.px}
\alias{as.data.frame}

\title{
Extraction of data from px into data.frame objects
}
\description{
This function extracts the data component from a px object as a \code{data.frame}.
}
\usage{
\S3method{as.data.frame}{px}( x, ..., use.codes = FALSE,
                 warnings.as.errors = TRUE, direction = 'long')
}
\arguments{
  \item{x}{ a px object }
  \item{use.codes}{ If true, this parameter replaces the names (or
    levels) of the variables by their codes, if available;
    alternatively, it can be a character vector identifying those
    variables for which codes will replace the levels of the
    variables. See details section. }
  \item{warnings.as.errors}{ If true, the function will fail in case any issues are found; otherwise, it will generate warnings. }
  \item{direction}{character string, either ‘"wide"’ to reshape to wide format,
          or ‘"long"’ to reshape to long format (default).}
  \item{...}{ Additional arguments, currently not used }
}
\details{
    The PC-Axis provides two alternative mechanisms for naming variables: their values or their codes, usually less verbose.
    The the \code{use.codes} parameter can be used to select those variables for which the CODES attribute in the PC-Axis file for naming the levels of the variables.
    It should be noted that there may not be CODES for all variables; in such cases, the VALUES are used instead.

    The function will operate in 'paranoid mode' unless \code{warnings.as.errors} is set to \code{FALSE}. 
    In such mode, the function will fail if any issues are found in the data.
    If \code{warnings.as.errors} is set to \code{FALSE}, the function will do its best to solve some known issues in some PC-Axis files.
}
\value{
A data.frame object.
}
\references{
http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf
}
\author{
Carlos J. Gil Bellosta, Oscar Perpiñán Lamigueiro, Francisco J. Viciana
Fernández, Emilio Torres
}
%\note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.px}},
  \code{\link{as.array.px}}
  }

\examples{
my.px.object <- read.px(system.file( "extdata", "example.px",
                                              package = "pxR"))
my.px.data   <- as.data.frame(my.px.object)
}
\keyword{ manip }
