% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install.7zip}
\alias{install.7zip}
\title{Downloads and installs 7-Zip for windows}
\usage{
install.7zip(
  page_with_download_url = "https://www.7-zip.org/download.html",
  ...
)
}
\arguments{
\item{page_with_download_url}{the URL of the 7-Zip download page.}

\item{...}{extra parameters to pass to \link{install.URL}}
}
\value{
TRUE/FALSE - was the installation successful or not.
}
\description{
Allows the user to downloads and install the latest version of 7-Zip for Windows.
}
\details{
7-Zip is open source software. Most of the source code is under the GNU LGPL license. The unRAR code is under a mixed license: GNU LGPL + unRAR restrictions. Check license information here: 7-Zip license.
You can use 7-Zip on any computer, including a computer in a commercial organization. You don't need to register or pay for 7-Zip.
*The main features of 7-Zip
*High compression ratio in 7z format with LZMA and LZMA2 compression
*Supported formats:
**Packing / unpacking: 7z, XZ, BZIP2, GZIP, TAR, ZIP and WIM
**Unpacking only: ARJ, CAB, CHM, CPIO, CramFS, DEB, DMG, FAT, HFS, ISO, LZH, LZMA, MBR, MSI, NSIS, NTFS, RAR, RPM, SquashFS, UDF, VHD, WIM, XAR and Z.
For ZIP and GZIP formats, 7-Zip provides a compression ratio that is 2-10 % better than the ratio provided by PKZip and WinZip
*Strong AES-256 encryption in 7z and ZIP formats
*Self-extracting capability for 7z format
*Integration with Windows Shell
*Powerful File Manager
*Powerful command line version
*Plugin for FAR Manager
*Localizations for 79 languages
}
\examples{
\dontrun{
install.7zip() # installs the latest version of 7-Zip
}
}
\references{
\itemize{
\item 7-zip homepage: \url{https://www.7-zip.org/}
}
}
