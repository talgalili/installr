\name{check.integer}
\alias{check.integer}
\title{Check if a number is integer}
\source{
  \link{http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer}
}
\usage{
  check.integer(N)
}
\arguments{
  \item{N}{A number (if a vector is supplied only the first
  element is checked - without warning)}
}
\value{
  TRUE/FALSE on whether a number is integer or not.
}
\description{
  Returns TRUE/FALSE on whether a number is integer or not.
}
\details{
  Surprising as it may be, R doesn't come with a handy
  function to check if the number is integer. This function
  does just this.
}
\examples{
check.integer <- installr:::check.integer
check.integer(4) # TRUE
check.integer(3243) #TRUE
check.integer(3243.34) #FALSE
check.integer("sdfds") #FALSE
check.integer(1e4) #TRUE
check.integer(1e6) #TRUE
check.integer(1e600) #FALSE - the function is having a hardtime with Inf...
rm(check.integer)
}
\author{
  VitoshKa
}

