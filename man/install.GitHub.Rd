\name{install.GitHub}
\alias{install.GitHub}
\title{Downloads and installs GitHub for windows}
\usage{
  install.GitHub(URL = "http://github-windows.s3.amazonaws.com/GitHubSetup.exe")
}
\arguments{
  \item{URL}{the URL of the GitHub download page.}
}
\value{
  Nothing.
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
install.GitHub() # installs the latest version of git
}
\references{
  \itemize{ \item GitHub homepage:
  \url{https://github.com/} \item GitHub for windows
  download page: \url{http://windows.github.com/} }
}

