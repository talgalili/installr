\name{install.MikTeX}
\alias{install.MikTeX}
\title{Downloads and installs MikTeX for windows}
\usage{
  install.MikTeX(version,
    page_with_download_url = "http://miktex.org/download")
}
\arguments{
  \item{page_with_download_url}{the URL of the MikTeX
  download page.}
}
\value{
  Nothing.
}
\description{
  Allows the user to downloads and install the latest
  version of MikTeX for Windows.
}
\details{
  MiKTeX is a typesetting system for Microsoft Windows that
  is developed by Christian Schenk. It consists of an
  implementation of TeX and a set of related programs.
  MiKTeX provides the tools necessary to prepare documents
  using the TeX/LaTeX markup language, as well a simple tex
  editor (TeXworks).

  MiKTeX is essential for using Sweave, knitr, and creating
  Vignette for R packages.
}
\examples{
\dontrun{
install.MikTeX() # installs the latest version of git
}
}
\references{
  MikTeX homepage: \link{http://miktex.org/} MikTeX
  download page: \link{http://miktex.org/download}
}

