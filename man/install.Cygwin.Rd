\name{install.Cygwin}
\alias{install.Cygwin}
\alias{install.cygwin}
\title{Downloads and installs Cygwin for windows}
\usage{
install.Cygwin(URL = "http://cygwin.com/setup.exe", ...)
}
\arguments{
  \item{URL}{the URL of the Cygwin setup.exe file.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
TRUE/FALSE - was the installation successful or not.
}
\description{
Allows the user to downloads and install the latest version
of Cygwin for Windows.
}
\details{
Cygwin is a collection of tools which provide a Linux look
and feel environment for Windows.
}
\examples{
\dontrun{
install.Cygwin() # installs the latest version of Cygwin
}
}
\references{
\itemize{ \item Cygwin homepage: \url{http://cygwin.com/} }
}

