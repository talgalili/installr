


#' @title Extract the file name from some URL
#' @description Gets a character of link to some file, and returns the name of the file in this link.
#' @details
#' The install.packages.zip must use this function, since it is crucial that the name of the file into which the ZIPPED package is downloaded to the computer, will have the same name as the file which is online.
#' @param URL Some url to a file.
#' @return The name of the file in the URL
#' @export
#' @seealso \code{\link{install.URL}}, \code{\link{install.packages.zip}}
#' @examples
#' \dontrun{
#' url <- "http://cran.r-project.org/bin/windows/base/R-2.15.3-win.exe"
#' file.name.from.url(url) # returns: "R-2.15.3-win.exe"
#' }
file.name.from.url <- function(URL) tail(strsplit(URL,   "/")[[1]],1)




#' @title Downloads and installs a ZIP R package Binary (for Windows) from a URL
#' @description Gets a character with a link to an R package Binary, downloads it, and installs it.
#' @details
#' To my knowledge, there is currently three ways to install packages on R:
#' 1. To get the package through a repository (such as CRAN or RForge) through install.packages.
#' 2. To manually download a ZIP file locally to the computer, and use install.packages on it.
#' 3. To get the package from github, by using devtools (but this will require you to first install RTools, and not everyone wishes to do it for just some package).
#' This function aims to combine option 1 and 2, by automatically downloading the ZIP file locally and then running install.packages on it. After being downloaded and installed, the binary is erased from the computer.
#' @param zip_URL a link to a ZIP R package Binary.
#' @return Invisible NULL
#' @export
#' @seealso \code{\link{install.packages}}
#' @examples
#' \dontrun{
#' install.packages.zip("http://cran.r-project.org/bin/windows/contrib/r-release/devtools_1.1.zip")
#' }
install.packages.zip <- function(zip_URL) {
   # zip_URL is the URL for the package_name.zip file
   zip_filename <- file.path(tempdir(), file.name.from.url(zip_URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(zip_URL, destfile=zip_filename, mode = 'wb')   
   install.packages(pkgs= zip_filename, repos=NULL)   
   unlink(zip_filename)
   invisible(NULL)
}
# a simple example of use:
# install.packages.zip(zip_URL="http://cran.r-project.org/bin/windows/contrib/r-release/TeachingSampling_2.0.1.zip")



#' @title Downloads and runs a .exe installer file for some software from a URL
#' @description Gets a character with a link to an installer file, downloads it, runs it, and then erases it.
#' @details
#' This function is used by many functions in the installr package.
#' The .exe file is downloaded into a temporary directory, where it is erased after installation has started (by default - though this can be changed)
#' @param exe_URL A character with a link to an installer file (with the .exe file extension)
#' @param keep_install_file If TRUE - the installer file will not be erased after it is downloaded and run.
#' @param wait should the R interpreter wait for the command to finish? The default is to NOT wait.
#' @param ... parameters passed to 'shell'
#' @return invisible(TRUE/FALSE) - was the installation successful or not. (this is based on the output of shell of running the command being either 0 or 1/2.  0 means the file was succesfully installed, while 1 or 2 means there was a failure in running the installer.)
#' @seealso \link{shell}
#' @export
#' @author GERGELY DAROCZI, Tal Galili
#' @examples
#' \dontrun{
#' install.URL("adfadf") # shows the error produced when the URL is not valid.
#' }
install.URL <- function(exe_URL, keep_install_file = FALSE, wait = TRUE, ...) {
   # source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
   # input: a url of an .exe file to install
   # output: it runs the .exe file (for installing something)   
   exe_filename <- file.path(tempdir(), file.name.from.url(exe_URL))   # the name of the zip file MUST be as it was downloaded...   
   tryCatch(download.file(exe_URL, destfile=exe_filename, mode = 'wb'), 
            error = function(e) cat("\nExplenation of the error: You didn't enter a valid .EXE URL. \nThis is likely to have happened because there was a change in the software download page, \nand the function you just ran no longer works. \n\n This is often caused by a change in the URL of the installer file in the download page of the software \n(making our function unable to know what to download). \n\n Please e-mail: tal.galili@gmail.com and let me know this function needs updating/fixing - thanks!\n"))  
   if(!keep_install_file & !wait) {
      wait <- TRUE
      warning("wait was set to TRUE since you wanted to installation file removed. In order to be able to run the installer AND remove the file - we must first wait for the isntaller to finish running before removing the file.")
   }
   shell_output <- shell(exe_filename, wait = wait,...) # system(exe_filename) # I suspect shell works better than system
   if(!keep_install_file) unlink(exe_filename, force = TRUE) # on.exit(unlink(exe_filename)) # on.exit doesn't work in case of problems in the running of the file
   # unlink can take some time until done, for some reason.
      #    file.remove(exe_filename)
      #    file.info(exe_filename)
      #    file.access(exe_filename, mode = 0)
      #    file.access(exe_filename, mode = 1)
      #    file.access(exe_filename, mode = 2)
      #    file.access(exe_filename, mode = 3)
   return(invisible(shell_output == 0))
   # error code 1/2 means that we couldn't finish running the file
   # # 0 means - the file was succesfully installed.   
}



#' @title Downloads and installs pandoc
#' @description Downloads and installs the latest version of pandoc for Windows.
#' @details
#' pandoc is a free open source software for converting documents from many filetypes to many filetypes.  For details, see \url{http://johnmacfarlane.net/pandoc/}.
#' 
#' Credit: the code in this function is based on GERGELY DAROCZIs coding in his answer on the Q&A forum StackOverflow, and also G. Grothendieck for the non-XML addition to the function. 
#' I thank them both!
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @author GERGELY DAROCZI, G. Grothendieck, Tal Galili
#' @param URL a link to the list of download links of pandoc
#' @param use_regex (default TRUE) should the regex method be used to extract exe links, or should the XML package be used.
#' @param ... extra parameters to pass to \link{install.URL}
#' @source \url{http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command}
#' @examples
#' \dontrun{
#' install.pandoc() 
#' }
install.pandoc <- function(
   URL = 'http://code.google.com/p/pandoc/downloads/list',
   use_regex = TRUE,...
) {
   page_with_download_url <- URL
   # source: http://stackoverflow.com/questions/15071957/is-it-possible-to-install-pandoc-on-windows-using-an-r-command
   # published on: http://www.r-statistics.com/2013/02/installing-pandoc-from-r-on-windows/
   
   
   if(use_regex) {
      page     <- readLines(page_with_download_url, warn = FALSE)
      #"//pandoc.googlecode.com/files/pandoc-1.11.1.msi"
      pat <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+.msi"
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
   
   install.URL(URL,...)
}

#' @title Check if a number is integer
#' @description Returns TRUE/FALSE on whether a number is integer or not.
#' @details
#' Surprising as it may be, R doesn't come with a handy function to check if the number is integer.
#' This function does just this.
#' @param N A number (if a vector is supplied only the first element is checked - without warning)
#' @return TRUE/FALSE on whether a number is integer or not.
#' @author VitoshKa
#' @source \url{http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer}
#' @examples
#' check.integer <- installr:::check.integer
#' check.integer(4) # TRUE
#' check.integer(3243) #TRUE
#' check.integer(3243.34) #FALSE
#' check.integer("sdfds") #FALSE
#' check.integer(1e4) #TRUE
#' check.integer(1e6) #TRUE
#' check.integer(1e600) #FALSE - the function is having a hardtime with Inf...
#' rm(check.integer)
check.integer <- function(N){
   # source: http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
   # author: VitoshKa
   
   # notice that the function "is.integer" is used by based R for checking the object if it is of type integer.
   
   # TODO: one day - to vectorize this.
   
   !length(grep("[^[:digit:]]", format(N[1], scientific = FALSE)))
}
# check.integer(3)


#' @title Asks the user for a row number from a data.frame table
#' @description The function gets a data.frame and asks the user to choose a row number.  Once choosen, that row number is returned from the function.
#' @details
#' This function is used in \code{installr} when we are not sure what version of the software to download, or when various actions are available for the user to choose from.
#' If the user doesn't give a valid row number, the function repeats its questions until a valid row number is chosen (or the user escapes)
#' @param TABLE a data.frame table with rows from which we wish the user to choose a row.  If TABLE is not a data.frame, it will be coerced into one.
#' @param header_text the text the users sees (often a question) as a title for the printed table - explaining which row he should choose from
#' @param questions_text the question the users see after the printing of the table - explaining which row he should choose from.
#' (the default is: "Please review the table of versions 
#' from above, and enter the row number of the file-version 
#' you'd like to install: ")
#' @return The row number the user has choosen from the data.frame table.
#' @source On how to ask the user for input:
#' 
#' \url{http://stackoverflow.com/questions/5974967/what-is-the-correct-way-to-ask-for-user-input-in-an-r-program}
#' @examples
#' \dontrun{
#' version_table <- data.frame(versions = c("devel", "V 1.0.0", "V 2.0.0"))
#' installr:::ask.user.for.a.row(version_table)
#' }
ask.user.for.a.row <- function(TABLE, 
                               header_text = "Possible versions to download (choose one)",
                               questions_text) {
   # http://stackoverflow.com/questions/5974967/what-is-the-correct-way-to-ask-for-user-input-in-an-r-program
   # based on code by Joris Meys

   if(missing(questions_text)) questions_text <- "Please review the table of versions from above,
and enter the row number of the file-version you'd like to install: "
   
   if(class(TABLE) != "data.frame") {
      TABLE <- as.data.frame(TABLE)
      colnames(TABLE)[1] <- "Choose:"
   }
   
   rownames(TABLE) <- seq_len(nrow(TABLE))# makes sure that the table's row names are in the "rownames"
   
   correct_input <- F
   nrow_TABLE <- nrow(TABLE)
   
   while(!correct_input){# n is the row number from the user
      cat("=============",header_text,"====================\n")      
      print(TABLE)
      ROW_id <- readline(questions_text)
      ROW_id <- as.numeric(ROW_id)
      correct_input <- 
         !is.na(ROW_id) && # don't check other condition if this is not met.
         check.integer(ROW_id) & # is integer AND
         ROW_id >= 1 & # make sure our ROW_id is within range (between 1 and the number of rows in the table)
         ROW_id <= nrow_TABLE
      if(!correct_input) cat("Wrong input: Please enter a valid number (integer, between 1 to the number of rows) \n  for the row number of your choice\n")
      # if(is.na(n)){break}  # breaks when hit enter
   }
   
   ROW_id
}
# version_table <- data.frame(versions = c("devel", "V 1.0.0", "V 2.0.0"))
# ask.user.for.a.row(version_table)











# 
# if(FALSE) {
#    # version_info is taken from https://github.com/hadley/devtools/blob/master/R/rtools.r
#    version_info <- list(
#       "2.11" = list(
#          version_min = "2.10.0",
#          version_max = "2.11.1",
#          path = c("bin", "perl/bin", "MinGW/bin")
#       ),
#       "2.12" = list(
#          version_min = "2.12.0",
#          version_max = "2.12.2",
#          path = c("bin", "perl/bin", "MinGW/bin", "MinGW64/bin")
#       ),
#       "2.13" = list(
#          version_min = "2.13.0",
#          version_max = "2.13.2",
#          path = c("bin", "MinGW/bin", "MinGW64/bin")
#       ),
#       "2.14" = list(
#          version_min = "2.13.0",
#          version_max = "2.14.2",
#          path = c("bin", "MinGW/bin", "MinGW64/bin")
#       ),
#       "2.15" = list(
#          version_min = "2.14.2",
#          version_max = "2.15.1",
#          path = c("bin", "gcc-4.6.3/bin")
#       ),
#       "2.16" = list(
#          version_min = "2.15.2",
#          version_max = "3.0.0",
#          path = c("bin", "gcc-4.6.3/bin")
#       ),
#       "3.0" = list(
#          version_min = "2.15.2",
#          version_max = "3.0.0",
#          path = c("bin", "gcc-4.6.3/bin")
#       )
#    )      
#    
#    require(plyr)
#    version_info2 <- ldply(version_info, function(xx) {data.frame(version_min=xx$version_min, version_max = xx$version_max)})
#    colnames(version_info2)[1] <- "version"
#   version_info2[,2] <- as.character(version_info2[,2])
#    version_info2[,3] <- as.character(version_info2[,3])
#    dput(version_info2)
# }



#' @title Downloads and installs Rtools
#' @aliases install.rtools
#' @description Allows the user to choose, downloads and install - the latest version of Rtools for Windows.  By default, the function searches if RTools is installed, if not, it checks if it knows which version to isntall for the current R version, and if not - it asks the user to choose which Rtools version to install.
#' @details
#' RTools is a collection of software for building packages for R under Microsoft Windows, or for building R itself (version 1.9.0 or later).
#' The original collection was put together by Prof. Brian Ripley; it is currently being maintained by Duncan Murdoch.
#' @param choose_version if TRUE, allows the user to choose which version of RTools to install.  Useful if you wish to install the devel version of RTools, or if you are running on an old version of R which requires an old version of R.
#' @param check checks if we need to install Rtools or not.  Relies on the "find_rtools" function in the {devtools} package.
#' @param use_GUI Should a GUI be used when asking the user questions? (defaults to TRUE)
#' @param page_with_download_url the URL of the RTools download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return invisible(TRUE/FALSE) - was the installation successful or not.
#' @export
#' @source
#' Some parts of the code are taken from the devtools, see \url{https://github.com/hadley/devtools/blob/master/R/rtools.r}
#' @references
#' RTools homepage (for other resources and documentation): \url{http://cran.r-project.org/bin/windows/Rtools/}
#' @examples
#' \dontrun{
#' install.Rtools() # installs the latest version of RTools (if one is needed)
#' install.Rtools(TRUE) # if one is needed - asks the user to choose the latest 
#' # version of RTools to install
#' 
#' install.Rtools(TRUE, FALSE) # asks the user to choose 
#' # the latest version of RTools to install 
#' # (regardless if one is needed)
#' }
install.Rtools <- function(choose_version = FALSE,                           
                           check=TRUE,
                           use_GUI = TRUE,
                           page_with_download_url = 'http://cran.r-project.org/bin/windows/Rtools/',
                           ...
) {
   # choose_version==T allows the user to choose which version of Rtools he wishes to install
   # latest_Frozen==T means we get the latest Rtools version which is Frozen (when writing this function it is Rtools215.exe)
   # latest_Frozen==F means we get the latest Rtools version which is not Frozen (when writing this function it is Rtools30.exe)

   if(check & require(devtools)) { # if we have devtools we can check for the existance of rtools
      found_rtools <- find_rtools()
      if(found_rtools) {
         cat("No need to install Rtools - You've got the relevant version of Rtools installed\n")
         return(invisible(FALSE))
      }
   }# if we reached here - it means we'll need to install Rtools.


   version_info2 <- structure(list(version = c("2.11", "2.12", "2.13", "2.14", "2.15", 
                                               "2.16", "3.0"), version_min = c("2.10.0", "2.12.0", "2.13.0", 
                                                                               "2.13.0", "2.14.2", "2.15.2", "2.15.2"), version_max = c("2.11.1", 
                                                                                                                                        "2.12.2", "2.13.2", "2.14.2", "2.15.1", "3.0.0", "3.0.0")), .Names = c("version", 
                                                                                                                                                                                                               "version_min", "version_max"), row.names = c(NA, -7L), class = "data.frame")
      
#    version_info2
   
   # try to fit the best Rtools to isntall
   Rversion <- as.character(getRversion())
   Rversion_number <- turn.version.to.number(Rversion)      
   ss_min <- Rversion_number >= turn.version.to.number(version_info2$version_min)
   ss_max <- Rversion_number <= turn.version.to.number(version_info2$version_max)
   version_to_install <- tail(version_info2$version[ss_min & ss_max], 1) # get the latest version that fits our current R version.

   
   if(length(version_to_install) > 0 & !choose_version) { # e.g: there is some version to install      
      version_to_install_no_dots <- gsub("\\.","", version_to_install)
      exe_filename <-   paste("Rtools" , version_to_install_no_dots , ".exe", sep = "")
   } else { # else - it means we have a version of R which is beyond our current knowledge of Rtools (or that the user asked to choose a version), so we'll have to let the user decide on what to do.
      if(!require(XML)) {
         cat("You need to install the {XML} package in order to use this function.")
         install_XML <- ask.user.yn.question("Do you wish to install the {XML} package?", use_GUI = use_GUI)
         if(install_XML) install.packages("XML")         
      }
      TABLE <- readHTMLTable(page_with_download_url, header=T,stringsAsFactors=F)[[1]]
      # example: http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package
      
      # choose a version:
      cat("Please remember you are using: ", R.version$version.string , "\n")
      choices <- paste(TABLE[,"Download"], " (",TABLE[,2],")", sep = "")      
      ROW_id <- menu(choices, graphics = use_GUI, title = "Which Rtools would you like to download?")      
      
      exe_filename <- TABLE[ROW_id,"Download"] # the version the user asked for
   }      
   
   # install Rtools!
   URL <- paste(page_with_download_url, exe_filename, sep = '')   
   install.URL(URL,...)   
}


#' @export
install.rtools <- function(...) install.Rtools(...)





#' @title Downloads and installs git and git-gui for windows
#' @description Allows the user to downloads and install the latest version of git for Windows.
#' @details
#' Git is a distributed revision control and source code management system with an emphasis on speed.
#' @param page_with_download_url the URL of the git download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' git homepage: \url{http://git-scm.com/}
#' git download page: \url{http://git-scm.com/download/win}
#' @examples
#' \dontrun{
#' install.git() # installs the latest version of git
#' }
install.git <- function(page_with_download_url="http://git-scm.com/download/win",...) {
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
   install.URL(URL,...)   
}






#' @title Downloads and installs Notepad++ for windows
#' @description Allows the user to downloads and install the latest version of Notepad++ for Windows.
#' @details
#' Notepad++ is a free (as in "free speech" and also as in "free beer") source code editor and Notepad replacement that supports several languages. Running in the MS Windows environment, its use is governed by GPL License.
#' Based on the powerful editing component Scintilla, Notepad++ is written in C++ and uses pure Win32 API and STL which ensures a higher execution speed and smaller program size. By optimizing as many routines as possible without losing user friendliness, Notepad++ is trying to reduce the world carbon dioxide emissions. When using less CPU power, the PC can throttle down and reduce power consumption, resulting in a greener environment.
#' @param page_with_download_url the URL of the Notepad++ download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return invisible TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' homepage: \url{http://notepad-plus-plus.org/}
#' download page: \url{http://notepad-plus-plus.org/download/}
#' @examples
#' \dontrun{
#' install.notepadpp() # installs the latest version of git
#' }
install.notepadpp <- function(page_with_download_url="http://notepad-plus-plus.org/download/",...) {
   # "http://git-scm.com/download/win"
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://download.tuxfamily.org/notepadplus/6.3.1/npp.6.3.1.Installer.exe
   pat <- "http://download.tuxfamily.org/notepadplus/[0-9.]+/npp.[0-9.]+.Installer.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   
   # install.
   install.URL(URL,...)   
}






#' @title Downloads and installs NppToR for windows
#' @description Allows the user to downloads and install the latest version of NppToR extension for Notepad++ for Windows.
#' @details
#' Similar to the windows R gui built in editor, NppToR aims to extend the functionality of code passing to the Notepad++ code editor. In addition to passing to the R gui, NppToR provides optional passing to a PuTTY window for passing to an R instance a remote machine.
#' 
#' NppToR is a companion utility that facilitates communication between R and Notepad++. It provides code passing from Notepad++ into the windows R Gui. NppToR also provides an autocompletion database which is dynamically generated from the users' R library of packages, thanks to an addition by Yihui Xie. Notepad++ provides built it R code highlighting and folding.
#' @param URL the URL of the Notepad++ download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return invisible TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' homepage: \url{http://npptor.sourceforge.net/}
#' download page: \url{http://sourceforge.net/projects/npptor/}
#' @examples
#' \dontrun{
#' install.npptor() # installs the latest version of npptor
#' }
install.npptor <- function(URL="http://sourceforge.net/projects/npptor/files/npptor%20installer/",...) {
   page_with_download_url <- URL
   # "http://git-scm.com/download/win"
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # /npptor installer/NppToR-2.6.2.exe
   # http://sourceforge.net/projects/npptor/files/npptor%20installer/NppToR-2.6.2.exe/
   # http://sourceforge.net/projects/npptor/files/npptor%20installer/NppToR-[0-9.]+.exe
   pat <- "http://sourceforge.net/projects/npptor/files/npptor%20installer/NppToR-[0-9.]+.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m)[1] # (The http still needs to be prepended.
   
   # install.
   install.URL(URL,...)   
}



#' @title Downloads and installs MikTeX for windows
#' @aliases install.miktex
#' @description Allows the user to downloads and install the latest version of MikTeX for Windows.
#' @details
#' MiKTeX is a typesetting system for Microsoft Windows that is developed by Christian Schenk. It consists of an implementation of TeX and a set of related programs. MiKTeX provides the tools necessary to prepare documents using the TeX/LaTeX markup language, as well a simple tex editor (TeXworks).
#' 
#' MiKTeX is essential for using Sweave, knitr, and creating Vignette for R packages.
#' @param version gets the values 32 and 64. Deciding if we should install version 32bit or 64bit. If missing (default) the user is prompted for a decisioin.
#' @param page_with_download_url the URL of the MikTeX download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' MikTeX homepage: \url{http://miktex.org/}
#' MikTeX download page: \url{http://miktex.org/download}
#' @examples
#' \dontrun{
#' install.MikTeX() # installs the latest version of git
#' }
install.MikTeX  <- function(version, page_with_download_url="http://miktex.org/download",...) {
   if(missing(version)) {
      version <- ifelse(ask.user.for.a.row(data.frame(version = c(32, 64)), "Which version of MiKTeX do you want?") == 1,
                        32, 64)
   } else { 
      if(!(version %in% c(32,64)))       version <- ifelse(ask.user.for.a.row(data.frame(version = c(32, 64)), "Which version of MiKTeX do you want?") == 1,
                                                           32, 64)      
   }
   
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
   install.URL(URL,...)   
}

#' @export
install.miktex <- function(...) install.MikTeX(...)





#' @title Downloads and installs LyX for windows
#' @aliases install.lyx
#' @description Allows the user to downloads and install the latest version of LyX for Windows.
#' @details
#' LyX is an advanced open source document processor running on Linux/Unix, Windows, and Mac OS X.
#' It is called a "document processor", because unlike standard word processors, LyX encourages an approach to writing based on the structure of your documents, not their appearance.#' 
#' LyX lets you concentrate on writing, leaving details of visual layout to the software.
#' LyX automates formatting according to predefined rule sets, yielding consistency throughout even the most complex documents.
#' LyX produces high quality, professional output - using LaTeX, an open source, industrial strength typesetting engine, in the background. 
#' @param page_with_download_url the URL of the LyX download page.
#' @param new_installation boolean. TRUE means we should make a new installation of LyX. FALSE means to update an existing installation.  Missing - prompts the user to decide.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item LyX homepage: \url{http://www.lyx.org/}
#' } 
#' @examples
#' \dontrun{
#' install.LyX() # installs the latest version of git
#' }
install.LyX  <- function(page_with_download_url="http://www.lyx.org/Download", new_installation, ...) {    

   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   
   
   # decide which version to get:
   if(missing(new_installation) || is.logical(new_installation)) {
      choices <- c("New: install a new version of LyX",
                   "Update: update an existing installation of LyX")   
      the_answer <- menu(choices, graphics = FALSE, title = "Do you wish a new installation of LyX\n or an update to an existing installation?")                  
   } else {
      the_answer <- ifelse(new_installation, 1, 2)
   }

   pat <-    switch(the_answer,           
                    "ftp://ftp.lyx.org/pub/lyx/bin/[0-9.]+/LyX-[0-9.]+-Bundle-[0-9.]+.exe",
                    "ftp://ftp.lyx.org/pub/lyx/bin/[0-9.]+/LyX-[0-9.]+-Installer-[0-9.]+.exe"
                  )   
   
   # ftp://ftp.lyx.org/pub/lyx/bin/2.0.5.1/LyX-2.0.5.1-Bundle-4.exe
   # URL = "ftp://ftp.lyx.org/pub/lyx/bin/2.0.5.1/LyX-2.0.5.1-Installer-4.exe"
   # install.URL(URL)
   
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   
   # install.
   install.URL(URL,...)   
}


#' @export
install.lyx <- function(...) install.LyX(...)



#' @title Downloads and installs RStudio for windows
#' @aliases install.rstudio
#' @description Allows the user to downloads and install the latest version of RStudio for Windows.
#' @details
#' RStudio is a free and open source integrated development environment (IDE) for R, a programming language for statistical computing and graphics.
#' @param page_with_download_url the URL of the RStudio download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item RStudio homepage: \url{http://www.rstudio.com/}
#' \item devtools::source_url \url{http://rgm3.lab.nig.ac.jp/RGM/r_function?p=devtools&f=source_url}
#' } 
#' @examples
#' \dontrun{
#' install.RStudio() # installs the latest version of git
#' }
install.RStudio  <- function(page_with_download_url="http://www.rstudio.com/ide/download/desktop",...) {    
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://download1.rstudio.org/RStudio-0.97.318.exe#
   pat <- "//download1.rstudio.org/RStudio-[0-9.]+.exe"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL,...)   
}

#' @export
install.rstudio <- function(...) install.RStudio(...)



#' @title Downloads and installs ImageMagick for windows
#' @aliases install.imagemagick
#' @description Allows the user to downloads and install the latest version of ImageMagick for Windows.
#' @details
#' ImageMagick is a software suite to create, edit, compose, or convert bitmap images. It can read and write images in a variety of formats (over 100) including DPX, EXR, GIF, JPEG, JPEG-2000, PDF, PhotoCD, PNG, Postscript, SVG, and TIFF. Use ImageMagick to resize, flip, mirror, rotate, distort, shear and transform images, adjust image colors, apply various special effects, or draw text, lines, polygons, ellipses and Bezier curves.
#' This function downloads Win32 dynamic at 16 bits-per-pixel.
#' @param URL the URL of the ImageMagick download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item ImageMagick homepage: \url{http://www.imagemagick.org/script/index.php}
#' } 
#' @examples
#' \dontrun{
#' install.ImageMagick() # installs the latest version of git
#' }
install.ImageMagick  <- function(URL="http://www.imagemagick.org/script/binary-releases.php",...) {    
   page_with_download_url <- URL
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://www.imagemagick.org/download/binaries/ImageMagick-6.8.3-8-Q16-x86-dll.exe
   # http://www.imagemagick.org/download/binaries/ImageMagick-6.8.3-9-Q16-x86-dll.exe
   pat <- "//www.imagemagick.org/download/binaries/ImageMagick-[0-9.]+-[0-9]-Q16-x86-dll.exe"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL,...)   
}

#' @export
install.imagemagick <- function(...) install.ImageMagick(...)



#' @title Downloads and installs GraphicsMagick for windows
#' @aliases install.graphicsmagick
#' @description Allows the user to downloads and install the latest version of GraphicsMagick for Windows.
#' @details
#' GraphicsMagick is the swiss army knife of image processing. Comprised of 282K physical lines (according to David A. Wheeler's SLOCCount) of source code in the base package (or 964K including 3rd party libraries) it provides a robust and efficient collection of tools and libraries which support reading, writing, and manipulating an image in over 88 major formats including important formats like DPX, GIF, JPEG, JPEG-2000, PNG, PDF, PNM, and TIFF.
#' This function downloads Win32 dynamic at 16 bits-per-pixel.
#' @param URL the URL of the ImageMagick download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item GraphicsMagick homepage: \url{http://www.graphicsmagick.org/}
#' } 
#' @examples
#' \dontrun{
#' install.GraphicsMagick() # installs the latest version of GraphicsMagick
#' }
install.GraphicsMagick  <- function(URL="http://sourceforge.net/projects/graphicsmagick/",...) {    
   page_with_download_url <- URL
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://downloads.sourceforge.net/project/graphicsmagick/graphicsmagick-binaries/1.3.17/GraphicsMagick-1.3.17-Q16-windows-dll.exe?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fgraphicsmagick%2Ffiles%2F&ts=1362862824&use_mirror=garr
   # http://sourceforge.net/projects/graphicsmagick/files/graphicsmagick-binaries/1.3.17/GraphicsMagick-1.3.17-Q8-windows-dll.exe/download
   pat <- "//sourceforge.net/projects/graphicsmagick/files/graphicsmagick-binaries/[0-9.]+/GraphicsMagick-[0-9.]+-Q16-windows-dll.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http', URL, sep = ':')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL,...)   
}

#' @export
install.graphicsmagick <- function(...) install.GraphicsMagick(...)



#' @title Downloads and installs SWFTools for windows
#' @aliases install.swftools
#' @description Allows the user to downloads and install the latest version of SWFTools for Windows.
#' @details
#' SWFTools is a collection of utilities for working with Adobe Flash files (SWF files). The tool collection includes programs for reading SWF files, combining them, and creating them from other content (like images, sound files, videos or sourcecode). SWFTools is released under the GPL. 
#' This function downloads current releases and NOT the Development Snapshots.
#' This function is useful for saveSWF() in the {animation} package.
#' @param page_with_download_url the URL of the SWFTools download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item SWFTools homepage: \url{http://swftools.org/}
#' } 
#' @examples
#' \dontrun{
#' install.SWFTools() # installs the latest version of SWFTools
#' }
install.SWFTools  <- function(page_with_download_url="http://swftools.org/download.html",...) {    
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://swftools.org/swftools-0.9.0.exe
   pat <- "swftools-[0-9.]+.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http://swftools.org/', URL, sep = '')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL,...)   
}

#' @export
install.swftools <- function(...) install.SWFTools(...)



#' @title Downloads and installs LaTeX2RTF for windows
#' @aliases install.latex2rtf
#' @description Allows the user to downloads and install the latest version of LaTeX2RTF for Windows.
#' @details
#' Latex2rtf tries to convert your LaTeX file into a RTF file for opening in Microsoft Word. The general idea is to try and get the things that computers are good at correct: character conversion, graphic conversion, etc. Page layout suffers because control in RTF is pretty pathetic compared to TeX. Consequently, it is likely that manual reformatting will be needed.
#' @param page_with_download_url the URL of the SWFTools download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item SWFTools homepage: \url{http://latex2rtf.sourceforge.net/}
#' } 
#' @examples
#' \dontrun{
#' install.LaTeX2RTF() # installs the latest version of LaTeX2RTF
#' }
install.LaTeX2RTF  <- function(page_with_download_url="http://sourceforge.net/projects/latex2rtf/",...) {    
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   #   http://sourceforge.net/projects/latex2rtf/files/latex2rtf-win/2.3.3/latex2rtf-2.3.3_win.exe/download
   pat <- "http://sourceforge.net/projects/latex2rtf/files/latex2rtf-win/[0-9.]+/latex2rtf-[0-9.]+_win.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   # install.
   install.URL(URL,...)   
}

#' @export
install.latex2rtf <- function(...) install.LaTeX2RTF(...)





#' @title Downloads and installs Cygwin for windows
#' @aliases install.cygwin
#' @description Allows the user to downloads and install the latest version of Cygwin for Windows.
#' @details
#' Cygwin is a collection of tools which provide a Linux look and feel environment for Windows.
#' @param URL the URL of the Cygwin setup.exe file.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item Cygwin homepage: \url{http://cygwin.com/}
#' } 
#' @examples
#' \dontrun{
#' install.Cygwin() # installs the latest version of SWFTools
#' }
install.Cygwin  <- function(URL = "http://cygwin.com/setup.exe",...) {    
#    # get download URL:
#    page     <- readLines(page_with_download_url, warn = FALSE)
#    # http://swftools.org/swftools-0.9.0.exe
#    pat <- "swftools-[0-9.]+.exe"
#    target_line <- grep(pat, page, value = TRUE); 
#    m <- regexpr(pat, target_line); 
#    URL      <- regmatches(target_line, m) # (The http still needs to be prepended.   
   # install.
   install.URL(URL,...)   
}

#' @export
install.cygwin <- function(...) install.Cygwin(...)






#' @title Downloads and installs 7-Zip for windows
#' @description Allows the user to downloads and install the latest version of 7-Zip for Windows.
#' @details
#' 7-Zip is open source software. Most of the source code is under the GNU LGPL license. The unRAR code is under a mixed license: GNU LGPL + unRAR restrictions. Check license information here: 7-Zip license.
#' You can use 7-Zip on any computer, including a computer in a commercial organization. You don't need to register or pay for 7-Zip.
#' *The main features of 7-Zip
#' *High compression ratio in 7z format with LZMA and LZMA2 compression
#' *Supported formats:
#' **Packing / unpacking: 7z, XZ, BZIP2, GZIP, TAR, ZIP and WIM
#' **Unpacking only: ARJ, CAB, CHM, CPIO, CramFS, DEB, DMG, FAT, HFS, ISO, LZH, LZMA, MBR, MSI, NSIS, NTFS, RAR, RPM, SquashFS, UDF, VHD, WIM, XAR and Z.
#' For ZIP and GZIP formats, 7-Zip provides a compression ratio that is 2-10 % better than the ratio provided by PKZip and WinZip
#' *Strong AES-256 encryption in 7z and ZIP formats
#' *Self-extracting capability for 7z format
#' *Integration with Windows Shell
#' *Powerful File Manager
#' *Powerful command line version
#' *Plugin for FAR Manager
#' *Localizations for 79 languages
#' 
#' @param page_with_download_url the URL of the 7-Zip download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item 7-zip homepage: \url{http://www.7-zip.org/}
#' } 
#' @examples
#' \dontrun{
#' install.7zip() # installs the latest version of SWFTools
#' }
install.7zip  <- function(page_with_download_url="http://www.7-zip.org/download.html",...) {    
   # get download URL:
   page     <- readLines(page_with_download_url, warn = FALSE)
   # http://downloads.sourceforge.net/sevenzip/7z920.exe
   pat <- "7z[0-9.]+.exe"
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   URL      <- regmatches(target_line, m) # (The http still needs to be prepended.
   URL      <- paste('http://downloads.sourceforge.net/sevenzip/', URL, sep = '')[1] # we might find the same file more than once - so we'll only take its first one
   
   # install.
   install.URL(URL,...)   
}


# 
# # ' @title Unzips a file using 7z
# # ' @param page_with_download_url the URL of the FFmpeg download page.
# # ' @param ... NOT used
# # ' @return the shell output of 7z
# # ' @references
# # ' \url{http://stackoverflow.com/questions/14122732/unzip-files-7-zip-via-cmd-command}
# # ' @examples
# # ' \dontrun{
# # ' 
# # ' }
# un7zip <- function(zip_file, the_7zip_path = "C:\\Program Files (x86)\\7-Zip\\",...)
# {
#    shell(paste("'",the_7zip_path,"7z.exe' -x '", a_7z_filename, "'", sep = ""), intern = TRUE, translate = TRUE)
# }
# # This function doesn't quite work...




#' @title Downloads and installs FFmpeg for windows
#' @aliases install.ffmpeg
#' @description Allows the user to downloads the latest version of FFmpeg for Windows.
#' IMPORTANT NOTE: The user (YOU) are responsible for unpacking the 7zip file into the relevant directory.  All that this function does is to download the 7zip file and "run" it.
#' @details
#' FFmpeg is a complete, cross-platform solution to record, convert and stream audio and video. It includes libavcodec - the leading audio/video codec library. See the documentation for a complete feature list and the Changelog for recent changes.
#' This function downloads current releases and NOT the Development Snapshots.
#' This function is useful for saveVideo() in the {animation} package.
#' @param page_with_download_url the URL of the FFmpeg download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return NULL
#' @export
#' @references
#' \itemize{
#' \item FFmpeg homepage: \url{http://FFmpeg.org/}
#' } 
#' @examples
#' \dontrun{
#' install.FFmpeg() # installs the latest version of FFmpeg 
#' }
install.FFmpeg   <- function(page_with_download_url="http://ffmpeg.zeranoe.com/builds/",...) {    
   URL      <- "http://ffmpeg.zeranoe.com/builds/win32/static/ffmpeg-latest-win32-static.7z" # it's always the same URL. The challange is to extract the 7z from it.
   a_7z_filename <- file.path(tempdir(), file.name.from.url(URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(URL, destfile=a_7z_filename, mode = 'wb') # Downloads the 7zip file

   shell(a_7z_filename,wait=FALSE)
   NULL
}

#' @export
install.ffmpeg <- function(...) install.FFmpeg(...)






#' @title Returns the search path for executable files
#' @export
#' @description Returns the search path for executable files based on %PATH%
#' @return A character vector with the search path for executable files
#' @references
#' \url{http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/ntcmds_shelloverview.mspx?mfr=true}
#' @examples
#' \dontrun{
#' system.PATH() # 
#' }
system.PATH <- function() strsplit(shell("echo %PATH% ", intern= TRUE), ";")[[1]]

#' @title Checks if some .exe is available in on the Windows machine search PATH
#' @description Checks the existence of an .exe extention in the search path for executable files
#' @param exe_file a character with the name of the 
#' @return A boolean vector indication the existence of each program on the system.
#' @examples
#' \dontrun{
#' is.exe.installed(c("zip.exe", "Rgui.exe", "blablabla")) # [1]  TRUE  TRUE FALSE
#' is.exe.installed("7z") 
#' }
is.exe.installed <- function(exe_file) {
   all_installed_exe <- list.files(system.PATH())
   exe_file %in% all_installed_exe
}
 
# ' @title Extends the current path with more possible softwares
# ' @description Useful for adding new softwares to the current search path
# ' @param exe_folder the folder where the relevant .exe file is.
# ' @return The updated search PATH
# ' @references
# ' \url{http://stackoverflow.com/questions/14122732/unzip-files-7-zip-via-cmd-command}
# ' \dontrun{
# ' set.PATH("C:\\Program Files (x86)\\7-Zip\\")
# ' is.exe.installed("7z") 
# ' }
# set.PATH <- function(exe_folder){
#    shell(paste('setx PATH=%PATH%;',exe_folder, sep = ""))         
#    system.PATH()
# }





#' @title Downloads and installs GitHub for windows
#' @aliases install.github
#' @description Allows the user to downloads and install the latest version of GitHub for Windows.
#' @details
#' "The easiest way to use Git on Windows." (at least so they say...)
#' @param URL the URL of the GitHub download page.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @references
#' \itemize{
#' \item GitHub homepage: \url{https://github.com/}
#' \item GitHub for windows download page: \url{http://windows.github.com/}
#' } 
#' @examples
#' \dontrun{
#' install.GitHub() # installs the latest version of git
#' }
install.GitHub <- function(URL = "http://github-windows.s3.amazonaws.com/GitHubSetup.exe",...) {
   # https://help.github.com/articles/set-up-git
   install.URL(URL,...)
}

#' @export
install.github <- function(...) install.GitHub(...)









#' @title Read R Code from a File in an https URL
#' @description source.https causes R to accept its input from a File in an https URL.
#' Input is read and parsed from that file until the end of the file is reached, then the parsed expressions are evaluated sequentially in the chosen environment.
#' @details
#' "The easiest way to use Git on Windows." (at least so they say...)
#' @param URL the URL of the .r file to download and source.
#' @param ... parameters to pass to \link{source}
#' @param remove_r_file if to remove the .r file after it was sourced.
#' @return Nothing.
#' @export
#' @seealso \link{source}
#' @references
#' Other solutions to the source.https problem:
#' \itemize{
#' \item Using RCurl: \url{http://tonybreyal.wordpress.com/2011/11/24/source.https-sourcing-an-r-script-from-github/}
#' \item devtools::source_url \url{http://rgm3.lab.nig.ac.jp/RGM/r_function?p=devtools&f=source_url}
#' \item A erlevant (OLD) discussion: http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
#' }
#' @examples
#' \dontrun{
#' source.https("https://raw.github.com/talgalili/installr/master/R/install.r") 
#' }
source.https <- function(URL,..., remove_r_file = T) {
   # this is an alternative to this code: http://tonybreyal.wordpress.com/2011/11/24/source.https-sourcing-an-r-script-from-github/
   # but one which does not require RCurl
   r_filename <- file.path(tempdir(), file.name.from.url(URL))   # the name of the zip file MUST be as it was downloaded...
   download.file(URL, destfile=r_filename, mode = 'wb')     
   source(r_filename,...)
   if(remove_r_file) unlink(r_filename)
   invisible(T)
}

# 
# # being able to source from github
# source("http://www.r-statistics.com/wp-content/uploads/2012/01/source.https.r.txt")
# source.https("source url for install.packages.zip")
# install.packages.zip("URL/installR.zip")
# # actually use functions...
# 
# 






#' @title Loading Packages (and Installing them if they are missing)
#' @export
#' @description  require2 load add-on packages by passing it to \link{require}.  However, if the package is not available on the system, it will first install it (through \link{install.packages}), and only then try to load it again.
#' 
#' @param package A character of the name of a package (can also be without quotes).
#' @param ask Should the user be asked to install the require packaged, in case it is missing? (default is TRUE)
#' @param ... not used
#' 
#' @return  returns (invisibly) a logical indicating whether the required package is available.
#' @examples
#' \dontrun{
#' a= require2("devtools")
#' a
#' a= require2(geonames)
#' a
#' }
require2 <- function(package, ask= TRUE, ...) {
   package <- as.character(substitute(package))
   if(!suppressWarnings(require(package=package, character.only = TRUE))) {
      install_package <- ask.user.yn.question(paste("Package ",package, " is not installed. Should it be installed?"))
      if(install_package) install.packages(pkgs=package)
   }
   require(package=package, character.only = TRUE)
}








#' @title Restart RGui from RGui
#' @export
#' @description Start a new RGui session and then quites the current one.
#' 
#' This is a Windows only function.
#' @param ... passed to q()
#' 
#' @return  q(...)
#' @examples
#' \dontrun{
#' restart_RGui()
#' }
restart_RGui <- function(...) {
	# start a new RGui
	# .Last <- function() 
   if(!is.windows()) stop("This function only works on Windows OS")
	if(!is.Rgui()) stop("This function only works when running it from Rgui.exe")
	shell(file.path(R.home("bin"),"Rgui.exe"), wait = FALSE)
	# close this one:
	q(...)
}










#' @title Installing software from R
#' @export
#' @description Gives the user the option to download software from within R.
#' @param use_GUI a logical indicating whether a graphics menu should be used if available.  If TRUE, and on Windows, it will use \link{winDialog}, otherwise it will use \link[utils]{menu}.
#' @param ... not in use
#' @return TRUE/FALSE - if the software was installed succesfully or no.
#' @seealso \link{updateR}, \link{install.R}, 
#' \link{install.RStudio}, \link{install.Rtools}, \link{install.pandoc}, 
#' \link{install.MikTeX}, \link{install.git}, \link{install.git},
#' \link{install.GraphicsMagick}, \link{install.ImageMagick},
#' \link{check.for.updates.R}, \link{install.URL}, \link{install.packages.zip},
#' 
#' @examples
#' \dontrun{
#' installr()
#' }
installr <- function(use_GUI = TRUE, ...) {
   choices <- c("R (updateR)",
                "RStudio",
                "Rtools",
                "git",
                "MikTeX",
                "LyX",
                "pandoc",
                "LaTeX2RTF",
                "GitHub",
                "ImageMagick",
                "GraphicsMagick",
                "SWFTools",
                "FFmpeg",
                "7-zip",
                "NotePad++",
                "NppToR (R extension to NotePad++)",
                "Cygwin",
                "Cancel")
   
   the_answer <- menu(choices, graphics = use_GUI, title = "Which software (for Windows) would you like to install?")            
   
   switch(the_answer, 
          updateR(),
          install.RStudio(),
          install.Rtools(),
          install.git(),
          install.MikTeX(),
          install.LyX(),
          install.pandoc(),
          install.LaTeX2RTF(),
          install.GitHub(),
          install.ImageMagick(),
          install.GraphicsMagick(),
          install.SWFTools(),
          install.FFmpeg(),
          install.7zip(),
          install.notepadpp(),
          install.npptor(),
          install.Cygwin(),
          return(FALSE)
   )
}





