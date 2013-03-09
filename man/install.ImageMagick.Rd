\name{install.ImageMagick}
\alias{install.ImageMagick}
\title{Downloads and installs ImageMagick for windows}
\usage{
  install.ImageMagick(page_with_download_url = "http://www.imagemagick.org/script/binary-releases.php")
}
\arguments{
  \item{page_with_download_url}{the URL of the ImageMagick
  download page.}
}
\value{
  TRUE/FALSE - was the installation successful or not.
}
\description{
  Allows the user to downloads and install the latest
  version of ImageMagick for Windows.
}
\details{
  ImageMagick® is a software suite to create, edit,
  compose, or convert bitmap images. It can read and write
  images in a variety of formats (over 100) including DPX,
  EXR, GIF, JPEG, JPEG-2000, PDF, PhotoCD, PNG, Postscript,
  SVG, and TIFF. Use ImageMagick to resize, flip, mirror,
  rotate, distort, shear and transform images, adjust image
  colors, apply various special effects, or draw text,
  lines, polygons, ellipses and Bézier curves. This
  function downloads Win32 dynamic at 16 bits-per-pixel.
}
\examples{
\dontrun{
install.ImageMagick() # installs the latest version of git
}
}
\references{
  \itemize{ \item ImageMagick homepage:
  \url{http://www.imagemagick.org/script/index.php} }
}

