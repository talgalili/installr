\name{install.SWFTools}
\alias{install.SWFTools}
\title{Downloads and installs SWFTools for windows}
\usage{
  install.SWFTools(page_with_download_url = "http://swftools.org/download.html",
    ...)
}
\arguments{
  \item{page_with_download_url}{the URL of the SWFTools
  download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to downloads and install the latest
  version of SWFTools for Windows.
}
\details{
  SWFTools is a collection of utilities for working with
  Adobe Flash files (SWF files). The tool collection
  includes programs for reading SWF files, combining them,
  and creating them from other content (like images, sound
  files, videos or sourcecode). SWFTools is released under
  the GPL. This function downloads current releases and NOT
  the Development Snapshots. This function is useful for
  saveSWF() in the {animation} package.
}
\examples{
\dontrun{
install.SWFTools() # installs the latest version of SWFTools
}
}
\references{
  \itemize{ \item SWFTools homepage:
  \url{http://swftools.org/} }
}

