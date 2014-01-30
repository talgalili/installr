\name{install.GitHub}
\alias{install.GitHub}
\alias{install.github}
\title{Downloads and installs GitHub for windows}
\usage{
  install.GitHub(URL = "http://github-windows.s3.amazonaws.com/GitHubSetup.exe",
    ...)
}
\arguments{
  \item{URL}{the URL of the GitHub download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to downloads and install the latest
  version of GitHub for Windows.
}
\details{
  "The easiest way to use Git on Windows." (at least so
  they say...)
}
\examples{
\dontrun{
install.GitHub() # installs the latest version of GitHub for windows
}
}
\references{
  \itemize{ \item GitHub homepage:
  \url{https://github.com/} \item GitHub for windows
  download page: \url{http://windows.github.com/} }
}

