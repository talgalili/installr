\name{install.URL}
\alias{install.URL}
\title{Downloads and runs a .exe installer file for some software from a URL}
\usage{
  install.URL(exe_URL, keep_install_file = F)
}
\arguments{
  \item{exe_URL}{A character with a link to an installer
  file (with the .exe file extension)}

  \item{keep_install_file}{If TRUE - the installer file
  will not be erased after it is downloaded and run.}
}
\value{
  Nothing.
}
\description{
  Gets a character with a link to an installer file,
  downloads it, runs it, and then erases it.
}
\details{
  This function is used by many functions in the installr
  package. The .exe file is downloaded into a temporary
  directory, where it is erased after installation has
  started (by default - though this can be changed)
}
\examples{
# install.URL()
}
\author{
  GERGELY DAROCZI, Tal Galili
}

