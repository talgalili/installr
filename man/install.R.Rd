\name{install.R}
\alias{install.R}
\title{Downloads and installs the latest R version}
\usage{

  install.R(page_with_download_url = "http://cran.rstudio.com/bin/windows/base/",
  pat = "R-[0-9.]+-win.exe", to_checkMD5sums = TRUE, ...)
}
\arguments{
  \item{page_with_download_url}{URL from which the latest
  stable version of R can be downloaded from.}

  \item{pat}{the pattern of R .exe file to download}

  \item{to_checkMD5sums}{Should we check that the new R
  installation has the files we expect it to (by checking
  the MD5 sums)? default is TRUE.  It assumes that the R
  which was isntalled is the latest R version.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
TRUE/FALSE - was the installation of R successful or not.
}
\description{
Fetches the latest (not development!) R version
}
\details{
If you are not sure if you need to update R or not, It is
better to use updateR for updating R, since it includes
more options. But in case you wish to only install R, with
no other steps taken (for example, taking care of your old
packages), then you can use install.R()

See the \link{install.Rdevel} function for installing the
latest R-devel version.
}
\examples{
\dontrun{
install.R()
}
}
\references{
\url{http://cran.rstudio.com/bin/windows/base/}
}
\seealso{
\link{uninstall.R}, \link{install.Rdevel}, \link{updateR},
\link{system}
}

