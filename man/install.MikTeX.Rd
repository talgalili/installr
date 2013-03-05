\name{install.MikTeX}
\alias{install.MikTeX}
\title{Downloads and installs MikTeX for windows}
\usage{
  install.MikTeX(version,
    page_with_download_url = "http://miktex.org/download")
}
\arguments{
  \item{version}{gets the values 32 and 64. Deciding if we
  should install version 32bit or 64bit. If missing
  (default) the user is prompted for a decisioin.}

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
  MikTeX homepage: \url{http://miktex.org/} MikTeX download
  page: \url{http://miktex.org/download}
}

