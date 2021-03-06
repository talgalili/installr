% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/startup.R
\name{rm_installr_from_startup}
\alias{rm_installr_from_startup}
\title{Remove installr from startup}
\usage{
rm_installr_from_startup(...)
}
\arguments{
\item{...}{not used. (but good for future backward compatibility)}
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
\url{https://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
\url{https://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
\url{http://www.noamross.net/archives/2012-11-02-rprofile/}
\url{https://www.statmethods.net/interface/customizing.html}
}
