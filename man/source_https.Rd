\name{source_https}
\alias{source_https}
\title{Read R Code from a File in an https URL}
\usage{
  source_https(URL, ..., remove_r_file = T)
}
\arguments{
  \item{URL}{the URL of the .r file to download and
  source.}

  \item{...}{parameters to pass to \link{source}}

  \item{remove_r_file}{if to remove the .r file after it
  was sourced.}
}
\value{
  Nothing.
}
\description{
  source_https causes R to accept its input from a File in
  an https URL. Input is read and parsed from that file
  until the end of the file is reached, then the parsed
  expressions are evaluated sequentially in the chosen
  environment.
}
\details{
  "The easiest way to use Git on Windows." (at least so
  they say...)
}
\examples{
source_https("https://raw.github.com/talgalili/installr/master/R/install.r")
}
\references{
  Other solutions to the source_https problem: \itemize{
  \item Using RCurl:
  \url{http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/}
  \item devtools::source_url
  \link{http://rgm3.lab.nig.ac.jp/RGM/r_function?p=devtools&f=source_url}
  \item A erlevant (OLD) discussion:
  http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
  }
}
\seealso{
  \link{source}
}

