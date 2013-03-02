\name{update.R}
\alias{update.R}
\title{Checks for the latest R version, and if there is a newer version of R - downloads and installs it.}
\usage{
  update.R(ask = T, notify_user = T, browse_news)
}
\arguments{
  \item{ask}{should the user be asked if to download R or
  not (default is TRUE).  If not, and the latest version of
  R is newer than what is currently installed - then R
  would be installed without asking the user for
  permission.  Of course the installation part itself (the
  running of the .exe file) is dependent on the user.}

  \item{notify_user}{if to tell the user what version he
  has and what is the latest version (default is TRUE)}

  \item{browse_news}{if TRUE (and if there is a newer
  version of R) - it opens the browser to the NEWS of the
  latest version of R, for the user to read through}
}
\value{
  a TRUE/FALSE value on whether or not R was updated.
}
\description{
  Checks for the latest R version, and if there is a newer
  version of R - downloads and installs it.
}
\examples{
update.R()
}

