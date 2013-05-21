\name{require2}
\alias{require2}
\title{Loading Packages (and Installing them if they are missing)}
\usage{
  require2(package, ask = TRUE, ...)
}
\arguments{
  \item{package}{A character of the name of a package.}

  \item{ask}{Should the user be asked to install the
  require packaged, in case it is missing? (default is
  TRUE)}

  \item{...}{not used}
}
\value{
  returns (invisibly) a logical indicating whether the
  required package is available.
}
\description{
  require2 load add-on packages by passing it to
  \link{require}.  However, if the package is not available
  on the system, it will first install it (through
  \link{install.packages}), and only then try to load it
  again.
}
\examples{
\dontrun{
a= require2("devtools")
a
a= require2("geonames")
a
}
}

