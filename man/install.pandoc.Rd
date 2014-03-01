\name{install.pandoc}
\alias{install.pandoc}
\title{Downloads and installs pandoc}
\source{
\url{http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command}
}
\usage{
install.pandoc(URL = "http://code.google.com/p/pandoc/downloads/list",
  use_regex = TRUE, to_restart, ...)
}
\arguments{
  \item{URL}{a link to the list of download links of
  pandoc}

  \item{use_regex}{(default TRUE) - defunced (kept for
  legacy purposes).}

  \item{to_restart}{boolean. Should the computer be
  restarted after pandoc is installed? (if missing then the
  user is prompted for a decision)}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
TRUE/FALSE - was the installation successful or not.
}
\description{
Downloads and installs the latest version of pandoc for
Windows.
}
\details{
pandoc is a free open source software for converting
documents from many filetypes to many filetypes.  For
details, see \url{http://johnmacfarlane.net/pandoc/}.

Credit: the code in this function is based on GERGELY
DAROCZIs coding in his answer on the Q&A forum
StackOverflow, and also G. Grothendieck for the non-XML
addition to the function. I thank them both!
}
\examples{
\dontrun{
install.pandoc()
}
}
\author{
GERGELY DAROCZI, G. Grothendieck, Tal Galili
}

