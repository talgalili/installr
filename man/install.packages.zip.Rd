\name{install.packages.zip}
\alias{install.packages.zip}
\title{Downloads and installs a ZIP R package Binary (for Windows) from a URL}
\usage{
  install.packages.zip(zip_URL)
}
\arguments{
  \item{zip_URL}{a link to a ZIP R package Binary.}
}
\value{
  Invisible NULL
}
\description{
  Gets a character with a link to an R package Binary,
  downloads it, and installs it.
}
\details{
  To my knowledge, there is currently three ways to install
  packages on R: 1. To get the package through a repository
  (such as CRAN or RForge) through install.packages. 2. To
  manually download a ZIP file locally to the computer, and
  use install.packages on it. 3. To get the package from
  github, by using devtools (but this will require you to
  first install RTools, and not everyone wishes to do it
  for just some package). This function aims to combine
  option 1 and 2, by automatically downloading the ZIP file
  locally and then running install.packages on it. After
  being downloaded and installed, the binary is erased from
  the computer.
}
\examples{
\dontrun{
install.packages.zip("http://cran.r-project.org/bin/windows/contrib/r-release/devtools_1.1.zip")
}
}
\seealso{
  \code{\link{install.packages}},
  \code{\link[R.utils]{installPackages}}
}

