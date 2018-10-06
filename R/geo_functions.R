# Copyright (C) Tal Galili
#
# This file is part of installr.
#
# installr is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# installr is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#








# Escaping "@" in Roxygen2 Style Documentation
# http://stackoverflow.com/questions/8809004/escaping-in-roxygen2-style-documentation


#' @title Measures the speed of downloading from different CRAN mirrors
#' @export
#' @description Estimates the speed of each CRAN mirror by measuring the time it takes to download the NEWS file.
#' 
#' @author Barry Rowlingson <b.rowlingson@@lancaster.ac.uk>
#' 
#' @param ms - the output of getCRANmirrors.  Defaults to using all of the mirrors.
#' @param ... not in use
#' 
#' 
#' @details
#' It works by downloading the latest NEWS file (288 Kbytes at the moment, so not huge) 
#' from each of the mirror sites in the CRAN mirrors list. 
#' If you want to test it on a subset then call getCRANmirrors yourself and subset it somehow.
#' 
#' It runs on the full CRAN list and while designing this package I've yet to find a 
#' timeout or error so I'm not sure what will happen if download.file
#' fails. It returns a data frame like you get from getCRANmirrors but
#' with an extra 't' column giving the elapsed time to get the NEWS file.
#' 
#' CAVEATS: if your network has any local caching then these results
#' will be wrong, since your computer will probably be getting the
#' locally cached NEWS file and not the one on the server. Especially if
#' you run it twice. Oh, I should have put cacheOK=FALSE in the
#' download.file - but even that might get overruled somewhere. Also,
#' sites may have good days and bad days, good minutes and bad minutes,
#' your network may be congested on a short-term basis, etc etc.
#' 
#' There may also be a difference in reliability, which would not so easily be measured by an individual user.
#' 
#' Later that year, Barry also wrote Cranography. See: \url{http://www.maths.lancs.ac.uk/~rowlings/R/Cranography/}.
#' 
#' @return a data.frame with details on mirror sites and the time it took to download their NEWS file.
#' 
#' 
#' @seealso \link{freegeoip}, \link{myip}, \link{cranometer}
#' 
#' @examples
#' \dontrun{
#' # this can take some time
#' x <- cranometer() 
#' 
#' time_order <- order(x$t)
#' 
#' # a quick overview of the fastest mirrors
#' head(x[time_order,c(1:4, 9)], 20)
#' 
#' # a dotchart of the fastest mirrors
#' with(x[rev(time_order),],
#'  dotchart(t, labels =Name,
#'  cex = .5, xlab = "Timing of CRAN mirror")
#'  )
#'
#'# tail(geonames_df)
#'# tail(x)
#'require(plyr)
#'ss <- !(x$Name == "0-Cloud")
#'gvis_df <- ddply(x[ss,], .(CountryCode), function(xx) {
#'   ss <- which.min(xx$t) 
#'   if(length(ss) == 0) ss <- 1
#'   data.frame(time = xx$t[ss], name = xx$Name[ss] )
#'})
#'gvis_df <- gvis_df[!is.na(gvis_df$time), ]
#'
#'require2("googleVis")
#'Geo<-gvisGeoMap(gvis_df,
#'               locationvar = "CountryCode",
#'                numvar="time",
#'                hovervar = "name",
#'                options=list(
#'                             colors='[0xA5EF63, 
#'                              0xFFB581, 0xFF8747]')
#'               )
#'# Display chart
#'plot(Geo) 
#' }
cranometer <- function(ms = getCRANmirrors(all = FALSE, local.only = FALSE),...){
   dest = tempfile()
   
   nms = dim(ms)[1]
   ms$t = rep(NA,nms)
   for(i in 1:nms){
      m = ms[i,]
      url = paste(m$URL,"/src/base/NEWS",sep="")
      t = try(system.time(download.file(url,dest),gcFirst=TRUE))
      if(file.exists(dest)){
         file.remove(dest)
         ms$t[i]=t['elapsed']
      }else{
         ms$t[i]=NA
      }
   }
   
   ms$t <- as.numeric(ms$t)
   
   return(ms)
}

## ----
# # a geomap of mirrors around the world, and how fast each one is
# require2("geonames")
# this can take some time
# find the geo-locations for all of the CRAN mirrors based on http://www.geonames.org/export/geonames-search.html
# geonames_df <- NULL
# for(i in 1:nrow(x)) {
#    tmp_geo <- with(x[i,], GNsearch(name=City, country= CountryCode))[1,]
#    # if we have no columns, it probably means that GNsearch couldn't find that city name, in which case, we shorten the name of the city and search again.
#    if(ncol(tmp_geo) == 0) {
#       tmp_city <- paste(tail(strsplit(x[i,"City"], " ")[[1]], -1), collapse = " ")
#       tmp_geo <- with(x[i,], GNsearch(name=tmp_city, country= CountryCode))[1,]      
#    }
#    if(ncol(tmp_geo) == 0) tmp_geo <- NA # if we still can't find anything, we should turn this to NA so that we would still add a row (though an empy one) to the data.frame
#       
#    geonames_df <- rbind(geonames_df,tmp_geo)            
# }
# LatLong <- with(geonames_df, paste(lat, ":", lng, sep = ""))
# gvis_df <- data.frame(LatLong, time = x$t, name = x$Name)







#' @title Geolocate IP addresses in R
#' @export
#' @description 
#' This R function uses the free freegeoip.net geocoding service to resolve an IP address (or a vector of them) into country, region, city, zip, latitude, longitude, area and metro codes.
#' 
#' The function require rjson.
#' 
#' @author Heuristic Andrew (see source for details)
#' 
#' @param ip a character vector of ips (default is the output from \link{myip})
#' @param format format of the output. Either "list" (default) or "data.frame" 
#' @param ... not in use
#' 
#' @return a list or data.frame with details on your geo location based on the freegeoip.net service.
#' 
#' @source  \url{http://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/}.
#' 
#' @seealso \link{freegeoip}, \link{myip}, \link{cranometer}
#' @examples
#' \dontrun{
#' freegeoip()
#' 
#' ## http://www.students.ncl.ac.uk/keith.newman/r/maps-in-r
#' # install.packages("maps")
#' # install.packages("mapdata")
#' library(maps)
#' library(mapdata)    # Contains the hi-resolution points that mark out the countries.
#' map('worldHires')
#' require(installr)
#' myip_details <- freegeoip(myip())
#' my_lati <- myip_details$latitude
#' my_long <- myip_details$longitude
#' points(my_lati,my_long,col=2,pch=18, cex = 1)
#' # lines(c(my_lati,0) ,c(my_long, 50), col = 2)#' 
#' }
freegeoip <- function(ip = myip(), format = ifelse(length(ip)==1,'list','dataframe'),...)
{
   stop("The website of this service has moved to https://ipstack.com/. If you want to update this function to work with it, please send a pull request on https://github.com/talgalili/installr")
   if (1 == length(ip))
   {
      # a single IP address
      require2("rjson")
      url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
      ret <- rjson::fromJSON(readLines(url, warn=FALSE))
      if (format == 'dataframe')
         ret <- data.frame(t(unlist(ret)))
      return(ret)
   } else {
      ret <- data.frame()
      for (i in 1:length(ip))
      {
         r <- freegeoip(ip[i], format="dataframe")
         ret <- rbind(ret, r)
      }
      return(ret)
   }
}





#' @title What is my IP
#' @export
#' @description 
#' Retrieving your public IP via \url{https://api.ipify.org}.
#' (old solution used: http://api.exip.org/
#' based on http://stackoverflow.com/questions/3097589/getting-my-public-ip-via-api)
#' @param ... not in use
#' @return your current ip (character string)
#' @source  \url{https://api.ipify.org}
#' @seealso \link{freegeoip}, \link{myip}, \link{cranometer}
#' 
#' @examples
#' \dontrun{
#' myip() # "37.132.25.15"
#' }
myip <- function(...) {
   readLines("https://api.ipify.org", warn = FALSE)
}




