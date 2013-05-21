\name{freegeoip}
\alias{freegeoip}
\title{Geolocate IP addresses in R}
\source{
  \url{http://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/}.
  \url{http://freegeoip.net/json/}
}
\usage{
  freegeoip(ip,
    format = ifelse(length(ip) == 1, "list", "dataframe"),
    ...)
}
\arguments{
  \item{ip}{the output of getCRANmirrors.  Defaults to
  using all of the mirrors.}

  \item{format}{format of the output. Either "list"
  (default) or "data.frame"}

  \item{...}{not in use}
}
\value{
  a list or data.frame with details on your geo location
  based on the freegeoip.net service.
}
\description{
  This R function uses the free freegeoip.net geocoding
  service to resolve an IP address (or a vector of them)
  into country, region, city, zip, latitude, longitude,
  area and metro codes.

  The function require rjson.
}
\examples{
\dontrun{
freegeoip()
}
}
\author{
  Heuristic Andrew (see source for details)
}

