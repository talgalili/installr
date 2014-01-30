\name{package_authors}
\alias{package_authors}
\title{Access (and clean) author elements from R's Rd file}
\usage{
package_authors(package, to_strsplit = TRUE, split = c(",|and"),
  to_table = FALSE, ...)
}
\arguments{
  \item{package}{a character string of the package we are
  interested in.}

  \item{to_strsplit}{logical (TRUE). Should the authors
  strings be split (in cases of a "and" or a comma ",")?}

  \item{split}{a character scalar to be passed to
  \link{strsplit} split paramter. default is c(",|and)}

  \item{to_table}{logical (FALSE). Should the authors
  strings be listed in a table - showing a count of how
  many .Rd files they were listed in? If not - a unique
  list is produced.}

  \item{...}{not used.}
}
\value{
a character vector with a package authors (as extracted
from the author tag in the .Rd files)
}
\description{
Access (and clean) author elements from R's Rd file
}
\details{
List authors for a package from its "author" tag elements
from its Rd files. The function also seperate lists of
authors, and cleans the output a bit (from spaces at the
beginning of the strings).
}
\examples{
\dontrun{

# before:
fetch_tag_from_Rd("installr", "\\\\author")
# after:
package_authors("installr")
sort(package_authors("installr")) # sorted name list...


## From the top R packages list:
## http://www.r-statistics.com/2013/06/top-100-r-packages-for-2013-jan-may/
package_authors("plyr")
package_authors("digest")
package_authors("ggplot2")
package_authors("colorspace")
package_authors("stringr") # empty string.

package_authors("knitr")
package_authors("MASS")
package_authors("rpart")
package_authors("Rcpp")


}
}
\references{
Useful for updating your DESCRIPTION file:

\url{http://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file}
}
\seealso{
\link{fetch_tag_from_Rd}
}

