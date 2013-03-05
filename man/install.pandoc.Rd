\name{install.pandoc}
\alias{install.pandoc}
\title{Downloads and installs pandoc}
\source{
  \url{http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command}
}
\usage{
  install.pandoc(page_with_download_url = "http://code.google.com/p/pandoc/downloads/list",
    use_regex = T)
}
\arguments{
  \item{page_with_download_url}{a link to the list of
  download links of pandoc}

  \item{use_regex}{(default TRUE) should the regex method
  be used to extract exe links, or should the XML package
  be used.}
}
\value{
  Nothing.
}
\description{
  Downloads and installs the latest version of pandoc for
  Windows.
}
\details{
  pandoc is a free open source software for converting
  documents from many filetypes to many filetypes.  For
  details, see \link{http://johnmacfarlane.net/pandoc/}.

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

