\name{install.LyX}
\alias{install.lyx}
\alias{install.LyX}
\title{Downloads and installs LyX for windows}
\usage{
  install.LyX(page_with_download_url = "http://www.lyx.org/Download",
    new_installation, ...)
}
\arguments{
  \item{page_with_download_url}{the URL of the LyX download
  page.}

  \item{new_installation}{boolean. TRUE means we should
  make a new installation of LyX. FALSE means to update an
  existing installation.  Missing - prompts the user to
  decide.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to downloads and install the latest
  version of LyX for Windows.
}
\details{
  LyX is an advanced open source document processor running
  on Linux/Unix, Windows, and Mac OS X. It is called a
  "document processor", because unlike standard word
  processors, LyX encourages an approach to writing based
  on the structure of your documents, not their
  appearance.#' LyX lets you concentrate on writing,
  leaving details of visual layout to the software. LyX
  automates formatting according to predefined rule sets,
  yielding consistency throughout even the most complex
  documents. LyX produces high quality, professional output
  - using LaTeX, an open source, industrial strength
  typesetting engine, in the background.
}
\examples{
\dontrun{
install.LyX() # installs the latest version of git
}
}
\references{
  \itemize{ \item LyX homepage: \url{http://www.lyx.org/} }
}

