\name{install.GraphicsMagick}
\alias{install.graphicsmagick}
\alias{install.GraphicsMagick}
\title{Downloads and installs GraphicsMagick for windows}
\usage{
  install.GraphicsMagick(URL = "http://sourceforge.net/projects/graphicsmagick/",
    ...)
}
\arguments{
  \item{URL}{the URL of the ImageMagick download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to downloads and install the latest
  version of GraphicsMagick for Windows.
}
\details{
  GraphicsMagick is the swiss army knife of image
  processing. Comprised of 282K physical lines (according
  to David A. Wheeler's SLOCCount) of source code in the
  base package (or 964K including 3rd party libraries) it
  provides a robust and efficient collection of tools and
  libraries which support reading, writing, and
  manipulating an image in over 88 major formats including
  important formats like DPX, GIF, JPEG, JPEG-2000, PNG,
  PDF, PNM, and TIFF. This function downloads Win32 dynamic
  at 16 bits-per-pixel.
}
\examples{
\dontrun{
install.GraphicsMagick() # installs the latest version of GraphicsMagick
}
}
\references{
  \itemize{ \item GraphicsMagick homepage:
  \url{http://www.graphicsmagick.org/} }
}

