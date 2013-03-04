\name{file.name.from.url}
\alias{file.name.from.url}
\title{Extract the file name from some URL}
\usage{
  file.name.from.url(URL)
}
\arguments{
  \item{URL}{Some url to a file.}
}
\value{
  The name of the file in the URL
}
\description{
  Gets a character of link to some file, and returns the
  name of the file in this link.
}
\details{
  The install.packages.zip must use this function, since it
  is crucial that the name of the file into which the
  ZIPPED package is downloaded to the computer, will have
  the same name as the file which is online.
}
\examples{
file.name.from.url("http://cran.r-project.org/bin/windows/base/R-2.15.3-win.exe") # returns: "R-2.15.3-win.exe"
}
\seealso{
  \code{\link{install.URL}},
  \code{\link{install.packages.zip}}
}

