# let's create a new package called installR  (a play on words installer) - for doing installing softwares on windows without leaving R.

install.URL <- function(URL) {
	# source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
	# input: a url of an .exe file to install
	# output: it runs the .exe file (for installing something)
	file_exe <- tempfile(fileext = '.exe')
	download.file(URL, file_exe, mode = 'wb')
	system(file_exe)
	unlink(file_exe)
}


install.pandoc <- function() {
	# source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
	# published on: http://www.r-statistics.com/?p=60914
	
	if(require(XML)) {
		page     <- readLines('http://code.google.com/p/pandoc/downloads/list', warn = FALSE)
		pagetree <- htmlTreeParse(page, error=function(...){}, useInternalNodes = TRUE, encoding='UTF-8')
		URL      <- xpathSApply(pagetree, '//tr[2]//td[1]//a ', xmlAttrs)[1]
		URL      <- paste('http', URL, sep = ':')
	} else {
		page     <- readLines('http://code.google.com/p/pandoc/downloads/list', warn = FALSE)
		pat <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+-setup.exe"; 
		target.line <- grep(pat, page, value = TRUE); 
		m <- regexpr(pat, target.line); 
		URL      <- regmatches(target.line, m) # (The http still needs to be prepended.
		URL      <- paste('http', URL, sep = ':')
	}

	install.URL(URL)
}


install.RStudio <- function() {
# http://www.rstudio.com/ide/download/desktop

}

install.Rtools <- function() {
# http://cran.r-project.org/bin/windows/Rtools/

}

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
