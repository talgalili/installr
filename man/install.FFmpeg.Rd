\name{install.FFmpeg}
\alias{install.FFmpeg}
\title{Unzips a file using 7z
#}
\usage{
  install.FFmpeg(page_with_download_url = "http://ffmpeg.zeranoe.com/builds/",
    ...)
}
\arguments{
  \item{page_with_download_url}{the URL of the FFmpeg
  download page. #}

  \item{...}{NOT used #}

  \item{page_with_download_url}{the URL of the FFmpeg
  download page.}

  \item{...}{extra parameters to pass to
  \link{install.URL}}
}
\value{
  the shell output of 7z #

  NULL
}
\description{
  Allows the user to downloads the latest version of FFmpeg
  for Windows. IMPORTANT NOTE: The user (YOU) are
  responsible for unpacking the 7zip file into the relevant
  directory.  All that this function does is to download
  the 7zip file and "run" it.
}
\details{
  #

  FFmpeg is a complete, cross-platform solution to record,
  convert and stream audio and video. It includes
  libavcodec - the leading audio/video codec library. See
  the documentation for a complete feature list and the
  Changelog for recent changes. This function downloads
  current releases and NOT the Development Snapshots. This
  function is useful for saveVideo() in the {animation}
  package.
}
\examples{
#\dontrun{
#
#}
}
\references{
  #\url{http://stackoverflow.com/questions/14122732/unzip-files-7-zip-via-cmd-command}
  #

  \itemize{ \item FFmpeg homepage: \url{http://FFmpeg.org/}
  }
}

