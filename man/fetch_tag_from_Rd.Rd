\name{fetch_tag_from_Rd}
\alias{fetch_tag_from_Rd}
\title{Access tag elements from R's Rd file}
\source{
\url{http://stackoverflow.com/questions/17909081/access-elements-from-rs-rd-file}
}
\usage{
fetch_tag_from_Rd(package, tag = "\\\\author", ...)
}
\arguments{
  \item{package}{a character string of the package we are
  interested in.}

  \item{tag}{a character vector of tag(s) to get from a
  package's Rd files.}

  \item{...}{not in use.}
}
\value{
a character vector with the tag's contant, and the name of
the Rd source of the function the tag came from.
}
\description{
A function to extract elements from R's help file.

It is useful, for example, for going through a package and
discover who are its authors (useful for me to help me give
proper credit in the DESCRIPTION file).
}
\examples{
\dontrun{
fetch_tag_from_Rd("installr", "\\\\author")
fetch_tag_from_Rd("knitr", "\\\\author")
fetch_tag_from_Rd("lubridate", "\\\\author")

fetch_tag_from_Rd("installr", "\\\\source")

# get all the authors for this package
unique(unname(fetch_tag_from_Rd("installr", "\\\\author")))

fetch_tag_from_Rd("installr", "\\\\author")
package_authors("installr")

}
}
\author{
Thomas J. Leeper <thosjleeper
}
\seealso{
\link{package_authors}
}

