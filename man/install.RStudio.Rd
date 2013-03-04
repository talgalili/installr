\name{install.RStudio}
\alias{install.RStudio}
\title{Downloads and installs RStudio for windows}
\usage{
  install.RStudio(page_with_download_url = "http://www.rstudio.com/ide/download/desktop")
}
\arguments{
  \item{page_with_download_url}{the URL of the RStudio
  download page.}
}
\value{
  Nothing.
}
\description{
  Allows the user to downloads and install the latest
  version of RStudio for Windows.
}
\details{
  RStudio is a free and open source integrated development
  environment (IDE) for R, a programming language for
  statistical computing and graphics.
}
\examples{
install.RStudio() # installs the latest version of git
}
\references{
  \itemize{ \item RStudio homepage:
  \url{http://www.rstudio.com//} \item devtools::source_url
  \link{http://rgm3.lab.nig.ac.jp/RGM/r_function?p=devtools&f=source_url}
  }
}

