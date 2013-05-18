\name{checkMD5sums2}
\alias{checkMD5sums2}
\title{Check and Create MD5 Checksum Files}
\usage{
  checkMD5sums2(package, dir, md5file, omit_files)
}
\arguments{
  \item{package}{the name of an installed package}

  \item{dir}{the path to the top-level directory of an
  installed package.}

  \item{md5file}{the exact path of the md5file to compare
  the dir with}

  \item{omit_files}{a character vector with the files or
  file diractories to not include in the checksums}
}
\value{
  checkMD5sums returns a logical, NA if there is no 'MD5'
  file to be checked.
}
\description{
  checkMD5sums checks the files against a file 'MD5'.  This
  extends the default checkMD5sums from package tools by
  adding a new parameter "md5file"
}
\examples{
\dontrun{
checkMD5sums2(dir=R.home()) # doesn't work for R 3.0.0 or R 3.0.1
checkMD5sums2(dir=R.home(), omit_files = c("etc/Rconsole", "etc/Rprofile.site")) # will work!
}
}
\seealso{
  \link[tools]{checkMD5sums}
}

