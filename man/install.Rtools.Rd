\name{install.Rtools}
\alias{install.Rtools}
\title{Downloads and installs Rtools}
\usage{
  install.Rtools(choose_version = FALSE,
    latest_Frozen = FALSE,
    page_with_download_url = "http://cran.r-project.org/bin/windows/Rtools/",
    ...)
}
\arguments{
  \item{choose_version}{if TRUE, allows the user to choose
  which version of RTools to install.  Useful if you wish
  to install the devel version of RTools, or if you are
  running on an old version of R which requires an old
  version of R.}

  \item{latest_Frozen}{if FALSE (and choose_version is
  FALSE) the function installs the latest devel version of
  RTools (good for people using the devel version of R).
  If TRUE (default), the latest frozen version of RTools is
  installed.}

  \item{page_with_download_url}{the URL of the RTools
  download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to choose, downloads and install - the
  latest version of Rtools for Windows.
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
install.Rtools() # installs the latest frozen version of RTools
install.Rtools(F, F) # installs the latest devel version of RTools
install.Rtools(T) # choose your version
}
}
\references{
  RTools homepage (for other resources and documentation):
  \url{http://cran.r-project.org/bin/windows/Rtools/}
}

