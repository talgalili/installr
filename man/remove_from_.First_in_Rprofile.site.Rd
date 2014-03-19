\name{remove_from_.First_in_Rprofile.site}
\alias{remove_from_.First_in_Rprofile.site}
\title{Remove a code line from Rprofile.site .First}
\usage{
  remove_from_.First_in_Rprofile.site(code, fixed = TRUE,
    ...)
}
\arguments{
  \item{code}{A character scalar with code to add at the
  beginning of the .First function in Rprofile.site}

  \item{fixed}{passed to \link{grep}}

  \item{...}{passed to \link{grep}}
}
\value{
  logical. Did we remove that line or not (in case it was
  not there)
}
\description{
  Goes through Rprofile.site text, finds a line of code -
  and removes it.
}
\examples{
\dontrun{
is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
add_to_.First_in_Rprofile.site("suppressMessages(require(installr))")
is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # TRUE
remove_from_.First_in_Rprofile.site("suppressMessages(require(installr))")
is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
# this would still leave .First
}
}
\references{
  \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
  \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
  \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
  \url{http://www.statmethods.net/interface/customizing.html}
}

