\name{install.R}
\alias{install.R}
\title{Downloads and installs the latest R version}
\usage{
  install.R(page_with_download_url = "http://cran.rstudio.com/bin/windows/base/")
}
\arguments{
  \item{page_with_download_url}{URL from which the latest
  stable version of R can be downloaded from.}
}
\value{
  TRUE/FALSE - was the installation of R successful or not.
}
\description{
  Fetches the latest (not development!) R version
}
\details{
  If you are not sure if you need to update R or not, you
  are It is better to use updateR for updating R, since it
  includes more options. But in case you wish to only
  install R, with no other steps taken (for example, taking
  care of your old packages), then you can use install.R()
}
\examples{
\dontrun{
install.R()
}
}

