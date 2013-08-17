\name{rm_installr_from_startup}
\alias{rm_installr_from_startup}
\title{Have the installr package NOT load on startup}
\usage{
  rm_installr_from_startup(...)
}
\arguments{
  \item{...}{not used.}
}
\value{
  invisible(NULL)
}
\description{
  Have the installr package NOT load on startup
}
\examples{
\dontrun{
load_installr_on_startup()
rm_installr_from_startup()
}
}
\references{
  \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
  \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
  \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
  \url{http://www.statmethods.net/interface/customizing.html}
}

