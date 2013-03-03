#' @title Checks if the running OS is windows
#' @description Returns TRUE/FALSE if the R session is running on Windows or not.
#' @details
#' This function is run when the 'installr' package is first loaded in order to check if the current running OS is Windows.
#' If you are running a different OS, then the installr package (at its current form) does not have much to offer you.
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the R session is running on Windows or not. 
#' @export
#' @examples
#' is.windows() # returns TRUE on my machine.
#' 
is.windows <- function(...) unname(Sys.info()["sysname"] == "Windows")
# inspiration: http://dennislwm-star.blogspot.sg/2012/11/r-is-almost-platform-independent.html


.onLoad <- function(libname, pkgname){
   if(!is.windows()) warning("The 'installr' package was designed for installing software on Windows. \nIt appears that you are NOT running R on the Windows OS - hence it is not clear if the package has any useful functions to offer you at this point (I'm sorry...).")
   # Thanks for Romain: http://stackoverflow.com/questions/4369334/first-lib-idiom-in-r-packages
}






# let's create a new package called installR  (a play on words installer) - for doing installing softwares on windows without leaving R.

file.name.from.url <- function(URL) tail(strsplit(URL,   "/")[[1]],1)

install.packages.zip <- function(zip_URL) {
   # zip_URL is the URL for the package_name.zip file
   zip_filename <- file.path(tempdir(), file.name.from.url(zip_URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(zip_URL, destfile=zip_filename, mode = 'wb')   
   install.packages(pkgs= zip_filename, repos=NULL)   
   unlink(zip_filename)
}
# a simple example of use:
# install.packages.zip(zip_URL="http://cran.r-project.org/bin/windows/contrib/r-release/TeachingSampling_2.0.1.zip")

# install.URL(URL)
install.URL <- function(exe_URL, remove_install_file = T) {
   # source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
   # input: a url of an .exe file to install
   # output: it runs the .exe file (for installing something)   
   exe_filename <- file.path(tempdir(), file.name.from.url(exe_URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(exe_URL, destfile=exe_filename, mode = 'wb')     
   system(exe_filename)
   if(remove_install_file) unlink(exe_filename)
   invisible()
}


install.pandoc <- function(
   page_with_download_url = 'http://code.google.com/p/pandoc/downloads/list',
   use_regex = T
) {
   # source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
   # published on: http://www.r-statistics.com/2013/02/installing-pandoc-from-r-on-windows/
   
   
   if(use_regex) {
      page     <- readLines(page_with_download_url, warn = FALSE)
      pat <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+-setup.exe"; 
      target_line <- grep(pat, page, value = TRUE); 
      m <- regexpr(pat, target_line); 
      URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
      URL      <- paste('http', URL, sep = ':')
   } else { # use XML
      if(!require(XML)) stop("You need to install the {XML} package in order to use this function.")
      page     <- readLines(page_with_download_url, warn = FALSE)
      pagetree <- htmlTreeParse(page, error=function(...){}, useInternalNodes = TRUE, encoding='UTF-8')
      URL      <- xpathSApply(pagetree, '//tr[2]//td[1]//a ', xmlAttrs)[1]
      URL      <- paste('http', URL, sep = ':')
   }
   
   install.URL(URL)
}


check.integer <- function(N){
   # source: http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
   # author: VitoshKa
   
   # notice that the function "is.integer" is used by based R for checking the object if it is of type integer.
   
   !length(grep("[^[:digit:]]", format(N, scientific = FALSE)))
}
# is.integer(3)

ask.user.for.a.row <- function(TABLE, header_text = "Possible versions to download (choose one)") {
   # http://stackoverflow.com/questions/5974967/what-is-the-correct-way-to-ask-for-user-input-in-an-r-program
   # based on code by Joris Meys
   
   correct_input <- F
   nrow_TABLE <- nrow(TABLE)
   
   while(!correct_input){# n is the row number from the user
      cat("=============",header_text,"====================\n")      
      print(TABLE)
      ROW_id <- readline("Please review the table of versions from above, \n  and enter the row number of the file-version you'd like to install: ")
      ROW_id <- as.numeric(ROW_id)
      correct_input <- 
         !is.na(ROW_id) && # don't check other condition if this is not met.
         check.integer(ROW_id) & # is integer AND
         ROW_id >= 1 & # make sure our ROW_id is within range (between 1 and the number of rows in the table)
         ROW_id <= nrow_TABLE
      if(!correct_input) cat("Wrong input: Please enter a valid number (integer, between 1 to the number of rows) \n  for the row number for the file you'd like to install\n")
      # if(is.na(n)){break}  # breaks when hit enter
   }
   
   ROW_id
}

# ask.user.for.a.row(TABLE)


install.Rtools <- function(choose_version = F,
                           latest_Frozen = T,
                           page_with_download_url = 'http://cran.r-project.org/bin/windows/Rtools/'
) {
   # choose_version==T allows the user to choose which version of Rtools he wishes to install
   # latest_Frozen==T means we get the latest Rtools version which is Frozen (when writing this function it is Rtools215.exe)
   # latest_Frozen==F means we get the latest Rtools version which is not Frozen (when writing this function it is Rtools30.exe)
   
   
   if(!require(XML)) stop("You need to install the {XML} package in order to use this function.")
   TABLE <- readHTMLTable(page_with_download_url, header=T,stringsAsFactors=F)[[1]]
   # example: http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package
   
   
   if(choose_version) {
      cat("Please remember you are using: ", R.version$version.string , "\n")
      ROW_id <- ask.user.for.a.row(TABLE)
      exe_filename <- TABLE[ROW_id,"Download"] # the version the user asked for
   } else { # if we choose the version for the user, we rely on latest_Frozen
      if(latest_Frozen) {
         ss <- TABLE[,"Frozen?"] == "Yes"
         exe_filename <- TABLE[ss,"Download"][1] # the latest Frozen filename
      } else { # choose the latest un-frozen (e.g: bleeding edge)
         ss <- TABLE[,"Frozen?"] == "No"
         exe_filename <- TABLE[ss,"Download"] # the latest un-Frozen filename
      }
   }
   
   
   URL <- paste(page_with_download_url, exe_filename, sep = '')
   
   install.URL(URL)   
}

# install.Rtools(T)



install.git <- function(page_with_download_url="http://git-scm.com/download/win") {
   # "http://git-scm.com/download/win"
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # https://msysgit.googlecode.com/files/Git-1.8.1.2-preview20130201.exe
   pat <- "//msysgit.googlecode.com/files/Git-[0-9.]+-preview[0-9.]+.exe"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('https', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL)   
}



install.MikTeX  <- function(version, page_with_download_url="http://miktex.org/download") {
   if(missing(version)) {
      version <- ifelse(ask.user.for.a.row(data.frame(version = c(32, 64)), "Which version of MiKTeX do you want?") == 1,
                        32, 64)
   } else { if(!(version %in% c(32,64))) stop("'version' MUST be either 32 or 64") }
   
   # get download URL:
   if(version == 32) {
      page     <- readLines(page_with_download_url, warn = FALSE)
      #"http://mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-2.9.4757.exe
      # "http://mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-2.9.4757-x64.exe"
      pat <- "//mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-[0-9.]+.exe"; 
      target_line <- grep(pat, page, value = TRUE); 
      m <- regexpr(pat, target_line); 
      URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
      URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   } else { # else -> version == 64
      page     <- readLines(page_with_download_url, warn = FALSE)
      #"http://mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-2.9.4757.exe
      # "http://mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-2.9.4757-x64.exe"
      pat <- "//mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-[0-9.]+-x64.exe"; 
      target_line <- grep(pat, page, value = TRUE); 
      m <- regexpr(pat, target_line); 
      URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
      URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one      
   }
   # install.
   install.URL(URL)   
}

# install.MikTeX()   

# 
# installR <- function(what) {
# # matches a character with what to install...
# 
# }
# 



install.RStudio  <- function(page_with_download_url="http://www.rstudio.com/ide/download/desktop") {    
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://download1.rstudio.org/RStudio-0.97.318.exe#
   pat <- "//download1.rstudio.org/RStudio-[0-9.]+.exe"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL)   
}

# install.RStudio()

install.GitHub <- function(URL = "http://github-windows.s3.amazonaws.com/GitHubSetup.exe") {
   # https://help.github.com/articles/set-up-git
   install.URL(URL)
}





# I can see if I am running on windows using this: 
# http://dennislwm-star.blogspot.sg/2012/11/r-is-almost-platform-independent.html




source_https <- function(URL, remove_install_file = T) {
   # this is an alternative to this code: http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
   # but one which does not require RCurl
   r_filename <- file.path(tempdir(), file.name.from.url(URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(URL, destfile=exe_filename, mode = 'wb')     
   source(r_filename)
   if(remove_install_file) unlink(r_filename)
   invisible(T)
}
# source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R")

# 
# # being able to source from github
# source("http://www.r-statistics.com/wp-content/uploads/2012/01/source_https.r.txt")
# source_https("source url for install.packages.zip")
# install.packages.zip("URL/installR.zip")
# # actually use functions...
# 
# 
