#' @title Downloads and installs the latest R version
#' @description Fetches the latest (not development!) R version
#' @details
#' If you are not sure if you need to update R or not, you are 
#' It is better to use update.R for updating R, since it includes more options.
#' But in case you wish to only install R, with no other steps taken (for example, taking care of your old packages), then you can use install.R()
#' @param page_with_download_url URL from which the latest stable version of R can be downloaded from.
#' @return Nothing.
#' @export
#' @examples
#' install.R() 
install.R <- function(page_with_download_url = "http://cran.rstudio.com/bin/windows/base/") {
   # I'm using the rsudio cran since it redirects to other servers wourld wide.
   # here there is a question on how to do it with the different mirrors. (maybe to add it as an option?)
   # this might be a good time for the "find the best mirror" function.   
   page   <- readLines(page_with_download_url, warn = FALSE)
   pat <- "R-[0-9.]+-win.exe"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   exe_filename   <- regmatches(target_line, m) 
   URL <- paste(page_with_download_url, exe_filename, sep = '')
   
   install.URL(URL)    
   
   # str(R.version )
   # R.version$major    
   # R.version$minor 
   # R.version$version.string
}

turn.version.to.number <- function(version_with_dots) {
   # version_with_dots is a character of the form xx.xx.xx
   version_with_dots_numbers <- as.numeric(strsplit(version_with_dots, "\\.")[[1]]   )
   version_with_dots_major <- version_with_dots_numbers[1]
   version_with_dots_minor1 <- version_with_dots_numbers[2]
   version_with_dots_minor2 <- version_with_dots_numbers[3]
   
   version_with_dots_long <- version_with_dots_major*1000000 + 
      version_with_dots_minor1*1000+ 
      version_with_dots_minor2
   
   version_with_dots_long
}
# turn.version.to.number("2.15.11") # ---> 2015011

# number_to_dots = 2015011
turn.number.version <- function(number_to_dots) {
   # I'm not really using this...
   number_to_dots_1 <- trunc(number_to_dots/1000000)
   number_to_dots <- number_to_dots - number_to_dots_1*1000000
   number_to_dots_2 <- trunc(number_to_dots/1000)
   number_to_dots_3 <- number_to_dots - number_to_dots_2*1000
   paste(number_to_dots_1, number_to_dots_2, number_to_dots_3, sep =".")   
}
# turn.number.version(2015011) # "2.15.11"


check.for.updates.R <- function(notify_user = T, 
                                page_with_download_url = "http://cran.rstudio.com/bin/windows/base/") {
   page   <- readLines(page_with_download_url, warn = FALSE)
   pat <- "R-[0-9.]+-win"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   latest_R_version  <- regmatches(target_line, m) 
   latest_R_version  <- gsub(pattern="R-|-win" ,"", latest_R_version) # remove junk text
   
   current_R_version <- paste(R.version$major, R.version$minor, sep=".")
   
   # Turn the version character into a number
   latest_R_version_long <- turn.version.to.number(latest_R_version)   
   current_R_version_long <- turn.version.to.number(current_R_version)   
   
   there_is_a_newer_version <- current_R_version_long < latest_R_version_long # TRUE = there IS a need to update (since the latest version is higher then what we currently have)
   
   if(notify_user) {
      if(there_is_a_newer_version) {
         cat("There is a newer version of R for you to download!\n")
         cat("You are using R version: ", current_R_version, "\n")
         cat("And the latest R version is: ", latest_R_version, "\n")
      } else {# we are not using the latest R version - now what...
         # tell it to the user:
         cat("No need to update - you are using the latest R version: ", R.version$version.string   , "\n")
      }         
   }
   
   there_is_a_newer_version
}



##' Checks for the latest R version, and if there is a newer version of R - downloads and installs it.
##'
##' 
##' @param ask should the user be asked if to download R or not (default is TRUE).  If not, and the latest version of R is newer than what is currently installed - then R would be installed without asking the user for permission.  Of course the installation part itself (the running of the .exe file) is dependent on the user.
##' @param notify_user if to tell the user what version he has and what is the latest version (default is TRUE)
##' @param browse_news if TRUE (and if there is a newer version of R) - it opens the browser to the NEWS of the latest version of R, for the user to read through
##' @return a TRUE/FALSE value on whether or not R was updated.
##' @export
##' @examples
##' update.R()
##' 
update.R <- function(ask = T, notify_user=T, browse_news) {
   # this function checks if we have the latest version of R
   # IF not - it notifies the user - and leaves.
   # If there is a new version - it offers the user to download and install it.   
   
   there_is_a_newer_version_of_R <- check.for.updates.R(notify_user)
   
   if(!there_is_a_newer_version_of_R) return(F) # if we have the latest version - we might as well stop now...
   
   # else - there_is_a_newer_version_of_R==T
   # should we ask?
   if(ask) {      
      
      # since there is a newer version - do you want to see the latest NEWS?
      if(missing(browse_news)) {
         to_read_news <- readline("Do you wish to see the NEWS regarding this new version of R? (y/n): ")
         browse_news <- ifelse(tolower(to_read_news) == "y", T, F)
      }      
      if(browse_news) browseURL("http://stat.ethz.ch/R-manual/R-patched/NEWS")
      
      to_update <- readline("Do you wish to install the latest version of R? (y/n): ")
      if(tolower(to_update) != "y") return(F)
   }
   
   # if we got this far, the user wants to install the latest version of R (and his current version is old)
   cat("Installing the newest version of R, pleaes wait...\n")
   install.R()
   return(TRUE)
}
