\name{install.Rtools}
\alias{install.Rtools}
\title{Downloads and installs Rtools}
\source{
  Some parts of the code are taken from the devtools, see
  \url{https://github.com/hadley/devtools/blob/master/R/rtools.r}
}
\usage{
  install.Rtools(choose_version = FALSE, check = TRUE,
    use_GUI = TRUE,
    page_with_download_url = "http://cran.r-project.org/bin/windows/Rtools/",
    ...)
}
\arguments{
  \item{choose_version}{if TRUE, allows the user to choose
  which version of RTools to install.  Useful if you wish
  to install the devel version of RTools, or if you are
  running on an old version of R which requires an old
  version of R.}

  \item{check}{checks if we need to install Rtools or not.
  Relies on the "find_rtools" function in the {devtools}
  package.}

  \item{use_GUI}{Should a GUI be used when asking the user
  questions? (defaults to TRUE)}

  \item{page_with_download_url}{the URL of the RTools
  download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  invisible(TRUE/FALSE) - was the installation successful
  or not.
}
\description{
  Allows the user to choose, downloads and install - the
  latest version of Rtools for Windows.  By default, the
  function searches if RTools is installed, if not, it
  checks if it knows which version to isntall for the
  current R version, and if not - it asks the user to
  choose which Rtools version to install.
}
\details{
  RTools is a collection of software for building packages
  for R under Microsoft Windows, or for building R itself
  (version 1.9.0 or later). The original collection was put
  together by Prof. Brian Ripley; it is currently being
  maintained by Duncan Murdoch.
}
\examples{
\dontrun{
install.Rtools() # installs the latest version of RTools (if one is needed)
install.Rtools(TRUE) # if one is needed - asks the user to choose the latest version of RTools to install
install.Rtools(TRUE, FALSE) # asks the user to choose the latest version of RTools to install (regardless if one is needed)
}
}
\references{
  RTools homepage (for other resources and documentation):
  \url{http://cran.r-project.org/bin/windows/Rtools/}
}

