\name{turn.version.to.number}
\alias{turn.version.to.number}
\title{Turns version to number (for a vector of values)}
\usage{
  turn.version.to.number(version_with_dots)
}
\arguments{
  \item{version_with_dots}{A character vector - of the
  version of R (for example 2.15.2)}
}
\value{
  A vector of "numbers" representing the versions (for
  example: 2015002).  The names of the vector is the
  original version character.
}
\description{
  Turns version to number (for a vector of values)
}
\examples{
\dontrun{
turn.version.to.number(c("2.15.2", "2.15.2"))
}
}

