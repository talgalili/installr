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
	   target.line <- grep(pat, page, value = TRUE); 
	   m <- regexpr(pat, target.line); 
	   URL      <- regmatches(target.line, m) # (The http still needs to be prepended.
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


install.RStudio <- function() {
# http://www.rstudio.com/ide/download/desktop

}


check.integer <- function(N){
   # source: http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
   # author: VitoshKa
   
   # notice that the function "is.integer" is used by based R for checking the object if it is of type integer.
   
   !length(grep("[^[:digit:]]", format(N, scientific = FALSE)))
}
# is.integer(3)

ask.user.for.a.row <- function(TABLE) {
   # http://stackoverflow.com/questions/5974967/what-is-the-correct-way-to-ask-for-user-input-in-an-r-program
   # based on code by Joris Meys
   
   correct_input <- F
   nrow_TABLE <- nrow(TABLE)
   
   while(!correct_input){# n is the row number from the user
      cat("=============Possible versions to download (choose one)====================\n")      
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



install.git <- function() {
# http://git-scm.com/download/win

}

install.R <- function() {
# http://cran.case.edu/bin/windows/base/
# here there is a question on how to do it with the different mirrors. (maybe to add it as an option?)
# this might be a good time for the "find the best mirror" function.

# str(R.version )
# R.version$major    
# R.version$minor 
}


install.MikTeX <- function() { 

}
   


installR <- function(what) {
# matches a character with what to install...

}










# I can see if I am running on windows using this: 
# http://dennislwm-star.blogspot.sg/2012/11/r-is-almost-platform-independent.html

RegIsWindowsBln <- function() {
  retBln <- (Sys.info()["sysname"] == "Windows")
  names(retBln) <- NULL
  retBln
}
RegIsLinuxBln <- function() {
  retBln <- (Sys.info()["sysname"] == "Linux")
  names(retBln) <- NULL
  retBln
}


RegGetHomeDir <- function() {
  retDir <- NULL
  if( RegIsLinuxBln() )
    retDir <- paste0("/home/",Sys.info()["user"],"/")
  if( RegIsWindowsBln() )
    retDir <- paste0("C:/Users/",Sys.info()["user"],"/")
  retDir
}

RegIsEmailBln <- function( emailChr ) {  
  patStr <- "^([a-zA-Z0-9]+[a-zA-Z0-9._%-]*@(?:[a-zA-Z0-9-])+(\\.+[a-zA-Z]{2,4}){1,2})$"
  grepl(patStr, emailChr)
}

