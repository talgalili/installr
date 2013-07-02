\name{uninstall.R}
\alias{uninstall.R}
\alias{uninstall.r}
\title{Uninstall an R version}
\usage{
  uninstall.R(r_version, use_GUI = TRUE)
}
\arguments{
  \item{r_version}{a character vector for R versions to
  uninstall (the format is of the style: "2.15.3"). default
  is empty - resulting in a prompt massage asking the user
  what to do.}

  \item{use_GUI}{If asking the user which R version to
  uninstall, should the GUI be used? (default is TRUE)}
}
\value{
  the output of \link{shell} running the uninstaller
}
\description{
  Choose an R version to uninstall via a menubar. By
  default, the function allows the user to pick an R
  version to uninstall from a list. Also, the function can
  be called with using "r_version", where multiple R
  versions can be supplied and all will be uninstalled.
}
\examples{
\dontrun{
uninstall.R() # choose an R version to uninstall
uninstall.R("2.15.3") # will uninstall R 2.15.3
uninstall.R(c("2.15.3", "2.14.0")) # will uninstall two R versions (if both exists)
uninstall.R("10.10.0") # would pop up the menu options (until R 10.10.0 will be released :D )
}
}
\seealso{
  \link{install.R}, \link{updateR}, \link{shell}
}

