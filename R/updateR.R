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










#' @title Check and Create MD5 Checksum Files
#' @export
#' @description checkMD5sums checks the files against a file 'MD5'.  This extends the default checkMD5sums from package tools by adding a new parameter "md5file"
#' @param package the name of an installed package
#' @param dir the path to the top-level directory of an installed package.
#' @param md5file the exact path of the md5file to compare the dir with
#' @param omit_files a character vector with the files or file diractories to not include in the checksums
#' @param ... not used. (but good for future backward compatibility)
#' @return checkMD5sums returns a logical, NA if there is no 'MD5' file to be checked.
#' @seealso \link[tools]{checkMD5sums}
#' @examples
#' \dontrun{
#' checkMD5sums2(dir=R.home()) # doesn't work for R 3.0.0 or R 3.0.1
#' checkMD5sums2(dir=R.home(), omit_files = c("etc/Rconsole", "etc/Rprofile.site")) # will work!
#' # tools::md5sum(file.path(R.home(), "MD5"))
#' 
#' }
checkMD5sums2 <- function (package, dir, md5file, omit_files,...)
{
   # library(tools) # making sure this is used.
   # R code in the package should call library or require only exceptionally. Such calls are never needed for packages listed in ?Depends? as they will already be on the search path. It used to be common practice to use require calls for packages listed in ?suggests? in functions which used their functionality, but nowadays it is better to access such functionality via :: calls.
   # from: http://cran.r-project.org/doc/manuals/R-exts.html


   
   if (missing(dir)) 
      dir <- find.package(package, quiet = TRUE)
   if (!length(dir)) 
      return(NA)
   if(missing(md5file)) {
      md5file <- file.path(dir, "MD5") # the "if-missing" is the only line added to the original function.
      if (!file.exists(md5file)) # I've moved this section into the "if" since file.exists fails if md5file is on the internet
         return(NA)
   }
   inlines <- readLines(md5file)
   
   # added in order to remove files from the checksums (used for problemetic cases with R)
   if(!missing(omit_files)) {
      omit_files <- paste(omit_files, collapse = "|")
      inlines <- inlines[-grep(omit_files, inlines)]      
   }   
   
   xx <- sub("^([0-9a-fA-F]*)(.*)", "\\1", inlines)
   nmxx <- names(xx) <- sub("^[0-9a-fA-F]* [ |*](.*)", "\\1", 
                            inlines)
   dot <- getwd()
   if (is.null(dot)) 
      stop("current working directory cannot be ascertained")
   setwd(dir)
   x <- tools::md5sum(dir(dir, recursive = TRUE))
   setwd(dot)
   x <- x[names(x) != "MD5"]
   nmx <- names(x)
   res <- TRUE
   not.here <- !(nmxx %in% nmx)
   if (any(not.here)) {
      res <- FALSE
      if (sum(not.here) > 1L) 
         cat("Warning: files", paste(sQuote(nmxx[not.here]), collapse = ", "), 
             "are missing\n", sep = " ")
      else cat("file", sQuote(nmxx[not.here]), "is missing\n", 
               sep = " ")
   }
   nmxx <- nmxx[!not.here]
   diff <- xx[nmxx] != x[nmxx]
   if (any(diff)) {
      res <- FALSE
      files <- nmxx[diff]
      if (length(files) > 1L) 
         warning(paste("files", paste(sQuote(files), collapse = ", "), 
             "have the wrong MD5 checksums\n", sep = " "))
      else warning(paste("file", sQuote(files), "has the wrong MD5 checksum\n"))
   }
   res
}









#' @title Asks the user for one yes/no question.
#' @export
#' @description Asks the user for one yes/no question.  If the users replies with a "yes" (or Y, or y) the function returns TRUE.  Otherwise, FALSE. (also exists as the function devtools::yesno)
#' @param question a character string with a question to the user.
#' @param GUI a logical indicating whether a graphics menu should be used if available.  If TRUE, and on Windows, it will use \link{winDialog}, otherwise it will use \link[utils]{menu}.
#' @param add_lines_before if to add a line before asking the question.  Default is TRUE.
#' @return TRUE/FALSE - if the user answeres yes or no.
#' @seealso \link[utils]{menu}, (yesno in the package {devtools}) 
#' @references \url{http://stackoverflow.com/questions/15250487/how-to-add-a-menu-item-to-rgui} 
#' (my thanks goes to Dason for his answer and help)
#' @examples
#' \dontrun{
#' ask.user.yn.question("Do you love R?")
#' ask.user.yn.question(question = "Do you love R?", GUI = TRUE) # the same one as before
#' ask.user.yn.question(question = "Do you love R?", GUI = FALSE) 
#' # reverts to command line questions
#' 
#' ask.user.yn.question("Lorem ipsum dolor sit amet, consectetur adipisicing elit, 
#' sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
#' Ut enim ad minim veniam, quis nostrud exercitation
#' 
#'   ullamco laboris nisi ut aliquip 
#'   ex ea commodo consequat. Do \n you \n love R?")
#'    # checking how it deals with multi lines, and a lot of text (very good actually)
#'   
#' }
ask.user.yn.question <- function(question, GUI = TRUE, add_lines_before = TRUE) {
   choices <- c("Yes", "No")
   
   if(GUI & is.windows()) {
      the_answer <- winDialog(type = "yesno", message = question)
      the_answer <- ifelse(the_answer == "YES", 1L, 2L) # turns it to 1/2
   } else { # no Windows, and/or no GUI
      if(add_lines_before & !GUI) cat("------------------------\n")   
      the_answer <- menu(choices, graphics = GUI, title = question)            
   }
   
   ifelse(the_answer == 1L, TRUE, FALSE)   # returns TRUE or FALSE
}


#' @title Checks if there is a newer version of R
#' @export
#' @description 
#' Fetches the latest (not development!) R version and compares it with your currently installed R version (the version of the R session from which you are running this function).
#' 
#' @param notify_user if to print to you (the user) what is the latest version and what version you are currently using.
#' @param GUI a logical indicating whether a graphics menu should be used if available.  If TRUE, and on Windows, it will use \link{winDialog}, otherwise it will use \link{cat}.
#' @param page_with_download_url the URL of the page from which R can be downloaded.
#' @param pat pattern to search for when looking for a newer R version
#' @return TRUE/FALSE - if there is a newer version of R to install or not.
#' @examples
#' \dontrun{
#' check.for.updates.R() 
#' # Possible output:
#' #  There is a newer version of R for you to download!
#' #  You are using R version:  2.15.0 
#' #  And the latest R version is:  2.15.3 
#' #  [1] TRUE
#' }
check.for.updates.R <- function(notify_user = TRUE, 
                                GUI = TRUE, 
                                page_with_download_url = "https://cran.rstudio.com/bin/windows/base/",
                                pat = "R-[0-9.]+.+-win\\.exe") {
   # stringr::str_extract("R-3.2.4-win.exe", "R-[0-9.]+.+-win\\.exe")
   # stringr::str_extract("R-3.2.4revised-win.exe", "R-[0-9.]+.+-win\\.exe")
   
   page   <- readLines(page_with_download_url, warn = FALSE)    
   filename <- na.omit(stringr::str_extract(page, pat))[1]

   latest_R_version  <- stringr::str_extract(filename, "[0-9.]+")
   
   pat <- "Last change: [0-9.]+-[0-9.]+-[0-9.]+"; 
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   latest_R_date  <- regmatches(target_line, m) 
   latest_R_date  <- gsub(pattern="Last change: " ,"", latest_R_date) # remove junk text
   
   
   current_R_version <- as.character(getRversion()) # paste(R.version$major, R.version$minor, sep=".")
   
   # Turn the version character into a number
   latest_R_version_long <- turn.version.to.number(latest_R_version)   
   current_R_version_long <- turn.version.to.number(current_R_version)   
   
   there_is_a_newer_version <- current_R_version_long < latest_R_version_long # TRUE = there IS a need to update (since the latest version is higher then what we currently have)
   
   if(there_is_a_newer_version) {
      message_text <-   paste("There is a newer version of R for you to download!\n\n",
                              "You are using R version:    \t", gsub("R version", "", R.version$version.string), "\n",
                              "And the latest R version is:\t ", latest_R_version, " (",latest_R_date,")", "\n",
                              sep = "")  
   } else {
      message_text <- paste("No need to update. You are using the latest R version: \n", R.version$version.string)
   }   
   
   
   if(notify_user) {
      if(GUI) {
         winDialog(type = "ok", message = message_text)   
      } else {
         cat(message_text)
      }
   }     
   
   
   return(there_is_a_newer_version)
}

# check.for.updates.R() 




#' @title See the NEWS file for the latest R release
#' @export
#' @description Sends the user the the NEWS html file on "https://cran.rstudio.com/bin/windows/base/NEWS.R-3.0.0.html" (URL changes with each version)
#' @param URL the URL of the page from which R can be downloaded.
#' @param ... for future use
#' @return invisible(NULL)
#' @examples
#' \dontrun{
#' browse.latest.R.NEWS() 
#' }
browse.latest.R.NEWS <- function(
   URL = "https://cran.rstudio.com/bin/windows/base/",...) {
   page_with_download_url <- URL
   page   <- readLines(page_with_download_url, warn = FALSE)
   pat <- "NEWS.R-[0-9.]+[^\">]*.html" # this is the structure of the link...
   target_line <- grep(pat, page, value = TRUE); 
   m <- regexpr(pat, target_line); 
   latest_news_path  <- regmatches(target_line, m)[1]
   URL <- paste(page_with_download_url, ifelse(is.na(latest_news_path), "", latest_news_path), sep = "")
   browseURL(URL)   
   
   return(invisible(NULL))
}


#' @title Downloads and installs the latest R version
#' @export
#' @description Fetches the latest (not development!) R version
#' @details
#' If you are not sure if you need to update R or not, 
#' It is better to use updateR for updating R, since it includes more options.
#' But in case you wish to only install R, with no other steps taken (for example, taking care of your old packages), then you can use install.R()
#' 
#' See the \link{install.Rdevel} function for installing the latest R-devel version.
#' @param page_with_download_url URL from which the latest stable version of R can be downloaded from.
#' @param pat the pattern of R .exe file to download
#' @param to_checkMD5sums Should we check that the new R installation has the files we expect it to (by checking the MD5 sums)? default is TRUE.  It assumes that the R which was isntalled is the latest R version.
#' @param keep_install_file If TRUE - the installer file will not be erased after it is downloaded and run.
#' @param download_dir A character of the directory into which to download the file. (default is \link{tempdir}())
#' @param silent If TRUE - enables silent installation mode.
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation of R successful or not.
#' @seealso \link{uninstall.R}, \link{install.Rdevel}, \link{updateR}, \link{system}
#' @references \url{https://cran.r-project.org/bin/windows/base/}
#' @examples
#' \dontrun{
#' install.R() 
#' }
install.R <- function(page_with_download_url = "https://cran.rstudio.com/bin/windows/base/", 
                      pat = "R-[0-9.]+.+-win\\.exe",
                      to_checkMD5sums = TRUE,
                      keep_install_file = FALSE,
                      download_dir = tempdir(),
                      silent = FALSE,
                      ...) {
   # I'm using the rsudio cran since it redirects to other servers wourld wide.
   # here there is a question on how to do it with the different mirrors. (maybe to add it as an option?)
   # this might be a good time for the "find the best mirror" function.   

      # stringr::str_extract("R-3.2.4revised-win.exe", pat)
   page   <- readLines(page_with_download_url, warn = FALSE)    
   filename <- na.omit(stringr::str_extract(page, pat))[1]
   
   URL <- paste(page_with_download_url, filename, sep = '')
   
   # add command line arguments for silent mode
   if(silent){
      installer_option <- "/SILENT"
   } else{
      installer_option <- NULL
   }
   
   did_R_install <- install.URL(URL, keep_install_file = keep_install_file, download_dir=download_dir, installer_option = installer_option, ...)
   if(!did_R_install) return(FALSE) 
   
   # checks the MD5sums from the new R installation:
   if(to_checkMD5sums)    {
      new_R_path <- get.installed.R.folders()[1]
      # library(tools)
      files_to_omit_from_MD5 <- 
         c("etc/Rconsole", "etc/Rprofile.site",  "bin/R.exe", "bin/Rscript.exe" )
      pass_checkMD5sums <- checkMD5sums2(dir=new_R_path, 
                                         omit_files =files_to_omit_from_MD5
                                            ) # will work!         
      if(!pass_checkMD5sums) {
         warning("There was some problem with installing R.  Some files are not what they should be (e.g: check MD5 sums did not pass all the tests). \n  You can try installing R again (either manually or through install.R()), \n  and if the problem persists you can file a bug report by running:  bug.report(package = 'installr') ")
         # return(FALSE)
      }
   }
   
   # if we got to the end it means we got to install R, and it passed the MD5 checksum test
   return(TRUE)      
   # str(R.version )
   # R.version$major    
   # R.version$minor 
   # R.version$version.string
}







#' @title Downloads and installs the latest Rdevel version
#' @export
#' @description Fetches the latest (development!) R version
#' @details
#' This is a development version of R. It likely contains bugs, 
#' so be careful if you use it. Please don't report bugs in this version through the usual 
#' R bug reporting system, please report them on the r-devel mailing list
#' ---but only if they persist for a few days.
#' @param exe_URL A character with a link to an installer file (with the .exe file extension)
#' @param ... extra parameters to pass to \link{install.URL}
#' @return TRUE/FALSE - was the installation of R successful or not.
#' @seealso \link{install.R}, \link{updateR}
#' @references \url{https://cran.r-project.org/bin/windows/base/rdevel.html}
#' @examples
#' \dontrun{
#' install.Rdevel() 
#' }
install.Rdevel <- function(exe_URL = "https://cran.rstudio.com/bin/windows/base/R-devel-win.exe", ...) {
   install.URL(exe_URL)   
}


# install.Rpatch # TODO someday




#' @title Turns version to number (for 1 value only)
#' @description 
#' Turns version to number (for 1 value only)
#' @param version_with_dots A character value - of the version of R (for example 2.15.2)
#' @return A "number" representation of the version (for example: 2015002)
#' @seealso \link{turn.version.to.number}
#' @examples
#' \dontrun{
#' turn.version.to.number1("2.15.2")
#' turn.version.to.number1("3.0.1")
#' }
turn.version.to.number1 <- function(version_with_dots) {
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



### @aliases turn.version.to.number1

#' @title Turns version to number (for a vector of values)
#' @description 
#' Turns version to number (for a vector of values)
#' @param version_with_dots - A character vector - of the version of R (for example 2.15.2)
#' @return A vector of "numbers" representing the versions (for example: 2015002).  The names of the vector is the original version character.
#' @examples
#' \dontrun{
#' turn.version.to.number(c("2.15.2", "2.15.2"))
#' }
turn.version.to.number <- function(version_with_dots) {
   if(length(version_with_dots) > 1) {
      return(sapply(version_with_dots, turn.version.to.number1))
   } else {# we have just one element in this vector
      return(turn.version.to.number1(version_with_dots) )
   }
}


# turn.version.to.number("2.15.11") # ---> 2015011

# number_to_dots = 2015011

#' @title Turns a vector of version-numbers back to version-character
#' @description 
#' Version Num to char
#' @param number_to_dots A numeric vector - of the number-version of R 
#' @return A vector of "numbers" representing the versions (for example: 2015002).  The names of the vector is the original version character.
#' @examples
#' \dontrun{
#' turn.number.version(turn.version.to.number(c("2.15.2", "2.15.2")))
#' turn.number.version(2015011) # "2.15.11"
#' }
turn.number.version <- function(number_to_dots) {
   # I'm not really using this...
   number_to_dots_1 <- trunc(number_to_dots/1000000)
   number_to_dots <- number_to_dots - number_to_dots_1*1000000
   number_to_dots_2 <- trunc(number_to_dots/1000)
   number_to_dots_3 <- number_to_dots - number_to_dots_2*1000
   paste(number_to_dots_1, number_to_dots_2, number_to_dots_3, sep =".")   
}









#' @title Get the version of the R installed in a folder
#' @export
#' @description 
#' Get the version of the R installed in a folder based on the structure of the filename README.R-... (where ... is a version number for R).
#' This function helps detect the version number of an R installation even if the name of the folder is not standard.
#' If multiple versions were installed, overwriting each other, the most recent is selected.
#' @param folder The folder for which we wish to know the R version.
#' @return Returns a character vector of the R version (or NA, if this is not an R installation folder)
#' @seealso \link{get.installed.R.folders}
#' @examples
#' \dontrun{
#' R_version_in_a_folder(folder = R.home()) 
#' # returns the version of the current R installation
#' }
R_version_in_a_folder <- function(folder) { 
#    folder = R.home()
   files <- list.files(folder)
   files <- gsub("patched", "", files)
   ss <- grep("README.R-[0-9]+.[0-9]+.[0-9]+$", files)
   if(length(ss)==0) return(NA) # this means that the current folder is NOT an R installation folder with the file README.R-numbers
   README_x <- files[ss] # for example: "README.R-3.0.1"   
   
   # alternative to start=10:  regexpr("[0-9]+.[0-9]+.[0-9]", README_x)
   versions <- sort(substr(README_x, start=10, stop = nchar(README_x)))
   # Only take last if there are multiple
   versions[length(versions)]
}
   





#' @title Returns folder names with R installations
#' @description 
#' The function finds the folders where there are R installations.  This is important for deciding what to uninstall, and where from and to to move libraries.
#' This function ignores installations of R-devel at this point.
#' Also, this function is based on only looking at the folders above the current installation of R.  If there are other isntallations of R outside the above folder, they will not be listed.
#' @param sort_by_version should the returned vector be sorted by the version number? (default is yes - so that the first element is of the newest version of R) should the user be given the option to choose between which two libraries to copy the packages?  If FALSE (default), the folders are copied from the before-newest R installation to the newest R installation.
#' @param add_version_to_name should the version number be added to the vector of folders? (default is yes)
#' @return Returns a character vector (possibly named, possibly sorted) of the folders where there are R installations.
#' @export
#' @seealso \link{copy.packages.between.libraries}
#' @examples
#' \dontrun{
#' get.installed.R.folders() 
#' # returns the sorted and named vector of
#' # folder names where R is installed (in different versions).
#' #  The first element is 
#' # the folder of the newest version of R.
#' 
#' get.installed.R.folders(F, F) 
#' # returns the folder names where R is 
#' # installed (in different versions) - no sorting of 
#' # the folder names was performed
#' }
get.installed.R.folders <- function(sort_by_version = TRUE, add_version_to_name = TRUE) {
   # get the parent folder of the current R installation
   R_parent_folder <- paste(head(strsplit(R.home(), "/|\\\\")[[1]], -1), collapse = "/") # the strsplit is seperating the path whether it is / or \\ (but since \\ is a problem, I need to cancel it with \\\\)      
   items_in_R_parent_folder <- list.files(R_parent_folder)
   
   R_folders <- file.path(R_parent_folder, items_in_R_parent_folder) # some of these may NOT be R folders
   R_folders_versions <- sapply(R_folders, R_version_in_a_folder)
   #    R_folders = "C:/R-3.0.2"     
   if(all(is.na(R_folders_versions))) {
      warning("Could not find any R installation on your system. (You might have installed your R version on 'c:\\R' without sub folders...")
      return(NULL)
   }
   
   # remove NON R installation folders (for example "library")
   ss_R_folders <- !is.na(R_folders_versions)
   R_folders <- R_folders[ss_R_folders]
   R_folders_versions <- R_folders_versions[ss_R_folders]
   
   # Fix thanks to Dieter Menne:
   R_folders_versions <- unlist(R_folders_versions)
   # this might resolve some cases - but it is not yet clear to me that it does.
   
   
### old way of doing this which relied on the folder being of the form:  D:/R/R-3.0.1  
#    ss_R_subfolders_in_R_parent_folder <- grepl("R-[0-9]+.[0-9]+.[0-9]+$", items_in_R_parent_folder)
#    # notice the use of $ at the end of the regex
#    # Good regex syntax http://laurikari.net/tre/documentation/regex-syntax/
#    R_subfolders <- items_in_R_parent_folder[ss_R_subfolders_in_R_parent_folder]
#    
#    R_folders <- file.path(R_parent_folder, R_subfolders) # a vector with the full path folders of all of the R installations (Assuming they are all on the same folder)
#    R_folders_versions <- gsub("R-", "", R_subfolders)

   
   
   R_folders_versions_number <- turn.version.to.number(R_folders_versions)   
   
   if(add_version_to_name) names(R_folders) <- R_folders_versions   
   
   if(sort_by_version) {
      ss_order_R_folders_by_version <- order(R_folders_versions_number,decreasing=T)
      R_folders <- R_folders[ss_order_R_folders_by_version]      
   } # else - if not - just return the order of the folders as they where extracted
   return(R_folders)            
}

# for testing:
#turn.version.to.number = installr:::turn.version.to.number




#' @title Copies all packages from one library folder to another
#' @export
#' @description 
#' Copies all packages from one folder to another.  This function is used if we wish to either:
#' \itemize{
#' \item Upgrade R to a new version - and copy all of the packages from the old R installation to the new one.
#' \item Move to a global library system - and wanting to copy all of packages from the local library folder to the global one
#' }
#' It takes into account that we don't want to copy packages which have "high" importance (such as MASS, boot, graphics, utils, rpart, Matrix and more GREAT packages...) to the new library folder.
#' Also, it assumes that within an R installation, the packages are located inside the "library" folder.
#' @param from a character vector for the location of the old library folder FROM which to copy files from.
#' @param to a character vector for the location of the old library folder TO which to copy files to.
#' @param ask should the user be given the option to choose between which two libraries to copy the packages?  If FALSE (default), the folders are copied from the before-newest R installation to the newest R installation.  This the overrides "from" and "to" parameters.
#' @param keep_old should the packages be COPIED to the new library folder, thus KEEPing the old package as they are?  Or should they be removed?
#' @param do_NOT_override_packages_in_new_R default TRUE  If FALSE, then If a package exists in both the "from" and "to" library folders - it would copy to "to" the version of the package from "from". (this parameter should rarely be FALSE)
#' @return TRUE if it copied (moved) packages, and FALSE if it did not.
#' @seealso \link{get.installed.R.folders}
#' @examples
#' \dontrun{
#' copy.packages.between.libraries(ask = T) 
#' # it will ask you from what R version 
#' # to copy the packages into which R version.  
#' # Since (do_NOT_override_packages_in_new_R = T) the function will 
#' # make sure to NOT override your newer packages.
#' 
#' # copy.packages.between.libraries(ask = T, keep_old = F) 
#' # As before, but this time it will MOVE (instead of COPY) the packages.
#' #  e.g: erase them from their old location.
#' }
copy.packages.between.libraries <- function(from, to, ask =FALSE,keep_old = TRUE, do_NOT_override_packages_in_new_R = TRUE) {
   
   installed_R_folders <- get.installed.R.folders()   
   installed_R_folders_TABLE <-data.frame("R_version" = names(installed_R_folders) , Folder = installed_R_folders)
   
   if(ask) {      
      ask_again <- T
      while(ask_again){# n is the row number from the user         
         ss_R_folder_from <- ask.user.for.a.row(installed_R_folders_TABLE,
                                                "From: Choose an R version/folder from which to copy the package library",
                                                "FROM: Write the row number of the R version/folder FROM which to copy (or move) the package library (and then press enter):\n")
         cat("\nThank you\n")
         ss_R_folder_to <- ask.user.for.a.row(installed_R_folders_TABLE,
                                              "To: Choose an R version/folder from INTO which the packages from your old R installations will be copied TO",
                                              "TO: Write the row number of the R version/folder INTO which to copy (or move) the package library (and then press enter):\n")
         cat("\nThank you\n")
         
         from <- installed_R_folders[ss_R_folder_from]
         to <- installed_R_folders[ss_R_folder_to]
         
         DECISION_text <- paste("You've chosen to move your packages from: ", from, "  to: ", to)
         ask_again_12 <- ask.user.for.a.row(c("yes", "no"), "Are you sure?",paste(DECISION_text, " - Is this you final decision? \n(for 'yes' press 1, and for 'no' press 2)\n" ))
         ask_again <- ifelse(ask_again_12==1, F, T) # if we need to ask          
      }      
   }
   
   if(missing(from)) from <- installed_R_folders[2] # copy FROM the one version before newest R
   if(missing(to)) to <- installed_R_folders[1] # copy inTO the newest R
   
   # the libraries
   from_library <- file.path(from , "library")
   to_library <- file.path(to , "library")
   
   # their packages
   packages_in_the_from_library <- list.files(from_library)
   packages_in_the_to_library <- list.files(to_library) # used for "packages_to_NOT_move" if do_NOT_override_packages_in_new_R==T
   
   # which packages do we want to keep and which do we want to move?
   packages_to_NOT_move <- unname(installed.packages(priority = "high")[,"Package"]) # some context on "high" packages: http://stackoverflow.com/questions/9700799/what-is-difference-between-r-base-and-r-recommended-packages
   if(do_NOT_override_packages_in_new_R) packages_to_NOT_move <- c(packages_to_NOT_move, packages_in_the_to_library )
   ss_packages_to_NOT_move_from <- packages_in_the_from_library %in% packages_to_NOT_move
   ss_packages_to_YES_move_from <- !ss_packages_to_NOT_move_from # just so there would be no confusion
   # the final list of packages to move:
   packages_to_YES_move_from <- packages_in_the_from_library[ss_packages_to_YES_move_from]
   paths_of_packages_to_copy_from <- file.path(from_library, packages_to_YES_move_from)
   
   
   if(length(packages_to_YES_move_from)==0) {
      cat("No packages to copy.  Goodbye :) \n")
      return(F)
   }
   
   # COPY old packages to the new global folder:
   cat("-----------------------","\n")
   cat("I am now copying ", length(packages_to_YES_move_from) ," packages from:", from_library, " ; into: ", to_library) 
   cat("-----------------------","\n")
   flush.console()  # refresh the console so that the user will see the message
   packages_to_YES_move_from
   folders.copied <- file.copy(from = paths_of_packages_to_copy_from,    # copy folders
                               to = to_library,
                               overwrite = !do_NOT_override_packages_in_new_R, # to be SURE that an old library will not override a new one.
                               recursive =TRUE)   
   cat("=====================","\n")
   cat("Done. We finished copying all the packages to the new location\n")
   
   if(!keep_old) {
      cat("Next: we will remove the packages from the old R installation ('FROM') \n")
      deleted_packages <- unlink(paths_of_packages_to_copy_from , recursive = TRUE)   # delete all the packages from the "original" library folder (no need for double folders)      
      cat("Done. The old packages were deleted.\n")
   }   
   
   return(TRUE)
}





#' @title Checks for the latest R version, and if there is a newer version of R - downloads and installs it.
#' @export
#' @aliases 
#' updater
#' @description This function performs the following steps:
#' \itemize{
#' \item Check what is the latest R version.  If the current installed R version is up-to-date, the function ends (and returns FALSE)
#' \item If a newer version of R is available, the user is asked if to review the NEWS of the latest R version - in order to decide if to install the newest R or not.
#' \item If the user wishes to - the function will download and install it. (you will need to press the "next" buttons on your own)
#' \item Once the installation is done, you should press "any-key", and the function will proceed with copying all of your packages from your old (well, current) R installation, into your newer R installation.
#' \item You can then erase all of the packages in your old R installation.
#' \item After your packages are moved (and the old ones possibly erased), you will get the option to update all of your packages in the new version of R.
#' \item You will be asked if to open the Rgui of your new R.
#' \item Lastely - you can close the current session of your old R. 
#' } 
#' 
#' @details
#' It is worth noting that the function assumes that you are installing R in the same directory as before. That is, if the old R was on: D:\R\R-3.0.0 then the new R will be on D:\R\R-3.0.1.
#' @param fast logical (default is FALSE). If TRUE, it overrides other parameters and uses a set of defaults to make the
#' R installation as fast as possible: no news, installr R, copy packages and Rprofile, keep
#' old packages, updated packages, without quiting current R or starting the new R.
#' don't use GUI, check MD5sums, keep installed file in the \link{getwd}.
#' @param browse_news if TRUE (and if there is a newer version of R) - it opens the browser to the NEWS of the latest version of R, for the user to read through
#' @param install_R TRUE/FALSE - if to install a new version of R (if one is available).  If missing (this is the default)  - the user be asked if to download R or not.Of course the installation part itself (the running of the .exe file) is dependent on the user.
#' @param copy_packages TRUE/FALSE - if to copy your packages from the old version of R to the new version of R. If missing (this is the default)  - the user will be asked for his preference (he should say yes, unless he is using a global library folder).
#' @param copy_Rprofile.site logical - if to copy your Rprofile.site from the old version of R to the new version of R. If missing (this is the default)  - the user will be asked for his preference (he should say yes, unless he is using a global library folder).
#' @param update_packages TRUE/FALSE - if to update your packages in the new version of R (all packages will be updated without asking confirmation per package) If missing (this is the default)  - the user will be asked for his preference (he should say yes, unless he is using a global library folder).  This is done by calling the Rscript in the new R.
#' @param keep_old_packages - if the keep the packages in the library of the old R installation. If missing (this is the default)  - the user will be asked for his preference (he should say yes, unless he is using a global library folder).
#' @param start_new_R TRUE/FALSE - if to start the new R (Rgui) after we will quit the old R. Default is TRUE. It will try to start the 64 bit R version, if it does not exist, the 32 bit will be started. This may be less useful for people using RStudio or the likes.
#' @param quit_R TRUE/FALSE - if to quite R after the installation and package copying or not. If missing (this is the default) - the user is asked what to do.
#' @param print_R_versions if to tell the user what version he has and what is the latest version (default is TRUE)
#' @param GUI a logical indicating whether a graphics menu should be used if available.  If TRUE, and on Windows, it will use \link{winDialog}, otherwise it will use \link[utils]{menu}.
#' @param to_checkMD5sums Should we check that the new R installation has the files we expect it to (by checking the MD5 sums)? default is TRUE.  It assumes that the R which was isntalled is the latest R version. parameter is passed to install.R()
#' @param keep_install_file If TRUE - the installer file will not be erased after it is downloaded and run.
#' @param download_dir A character of the directory into which to download the file. (default is \link{tempdir}())
#' @param silent If TRUE - enables silent installation mode.
#' @param setInternet2 logical. Should setInternet2(TRUE) be run.
#' @param ... Other arguments (this is currently not used in any way)
#' @return a TRUE/FALSE value on whether or not R was updated.
#' @seealso \link{check.for.updates.R}, \link{install.R}, 
#' \link{copy.packages.between.libraries}, \link{uninstall.R}
#' @examples
#' \dontrun{
#' 
#' updateR(TRUE) # This sets "fast" to be TRUE
#' # # the fastest/safest upgrade option: 
#' # install R while keeping a copy in the working directory,
#' # copy packages, keep old packages, 
#' # update packages in the new installation.
#' 
#' updateR() # will ask you what you want at every decision.
#' }
updateR <- function(fast = FALSE, 
                    browse_news, install_R, copy_packages, copy_Rprofile.site,
                    keep_old_packages,  update_packages, start_new_R, quit_R,  print_R_versions=TRUE, GUI = TRUE, 
                    to_checkMD5sums = FALSE, keep_install_file = FALSE, download_dir = tempdir(),
                    silent = FALSE, 
                    setInternet2 = TRUE, ...) {
   # this function checks if we have the latest version of R
   # IF not - it notifies the user - and leaves.
   # If there is a new version - it offers the user to download and install it.   

   
   if(fast) { # set a bunch of parameters... (they are also better for blind people)
      browse_news <- FALSE
      install_R <- TRUE
      copy_packages <- TRUE
      copy_Rprofile.site <- TRUE
      keep_old_packages <- TRUE
      update_packages <- TRUE
      start_new_R <- FALSE
      quit_R <- FALSE
      print_R_versions  <-  TRUE
      GUI  <- FALSE
      to_checkMD5sums  <-  FALSE
      keep_install_file  <-  TRUE
      download_dir  <-  "."
      silent  <-  FALSE
      setInternet2 <- TRUE
   }
   
   if(setInternet2) setInternet2(TRUE)
   
   old_R_path <- get.installed.R.folders()[1]

   there_is_a_newer_version_of_R <- check.for.updates.R(print_R_versions, GUI = GUI)
   
   if(!there_is_a_newer_version_of_R) return(FALSE) # if we have the latest version - we might as well stop now...
   
   # else - there_is_a_newer_version_of_R==T
   
   # should we open the NEWS?
   if(missing(browse_news)) browse_news <- ask.user.yn.question("Do you wish to see the NEWS regarding this new version of R?", GUI = GUI)
   if(browse_news) browse.latest.R.NEWS()
   
   
   # should we install R?
   if(missing(install_R)) install_R <- ask.user.yn.question("Do you wish to install the latest version of R?", GUI = GUI)
   if(!install_R) return(FALSE) # if not - return F
   
   
   if(identical(options()$device, "RStudioGD")) {
      stop_because_of_RStudio <- ask.user.yn.question("It is best to run 'updateR()' from Rgui and not from RStudio. Would you like to abort the installation and run it again from RGui?", GUI = GUI)
      if(stop_because_of_RStudio) return(FALSE)
   }
   
   # if we got this far, the user wants to install the latest version of R (and his current version is old)
   cat("Installing the newest version of R,\n please wait for the installer file to be download and executed.\n Be sure to click 'next' as needed...\n")
   did_R_install <- install.R(to_checkMD5sums = to_checkMD5sums, keep_install_file = keep_install_file, download_dir = download_dir, silent = silent) 
   if(!did_R_install) return(FALSE) 
   new_R_path <- get.installed.R.folders()[1]
   
   if(is.null(new_R_path)) {
      warning("You seem to have installed R in an unusual folder structure. It seem to have installed correctly - but you will need to run update.packages(checkBuilt=TRUE, ask=FALSE) manually on your new installation.")
      return(TRUE)
   }
   
   
   # I could have also used:
   #    if(unname(up_folder(new_R_path))!=unname(up_folder(old_R_path))) {
   # but if the new R is installed somehwere else, then when fetching
   # the new R version, it will still search for it in the old R installation
   # folder.  Hence, the new and old R paths will be the same.
   if(new_R_path==old_R_path) {
      cat("
We can not seem to find the location if the new R you have installed.
The rest of the updating process is aborted, please take care to copy
your packages to the new R installation.\n")
      return(TRUE)
   }
   
   if(missing(copy_packages)) copy_packages <- ask.user.yn.question("Do you wish to copy your packages from the older version of R to the newer version of R?")
   
   if(copy_packages) {
      #       ask.user.for.a.row(c("Yes"), "Did you finish running the installer for the new R?", "Press 1 (and Enter) if the installation of R is finished:")
      # should we keep the old packages?
      if(missing(keep_old_packages)) keep_old_packages <- ask.user.yn.question("Once your packages are copied to the new R, \ndo you wish to KEEP the packages from the library in the OLD R installation? \n(if you choose 'NO' - you will erase your packages in the old R version) ", GUI = GUI)
      # Next, copy (or MOVE):
      copy.packages.between.libraries(keep_old=keep_old_packages)   
   }

   
   if(missing(copy_Rprofile.site)) copy_Rprofile.site <- ask.user.yn.question("Do you wish to copy your 'Rprofile.site' from the older version of R to the newer version of R?")
   
   if(copy_Rprofile.site) {
      old_R_path_etc <- file.path(old_R_path, "etc")
      new_R_path_etc <- file.path(new_R_path, "etc")
      if("Rprofile.site" %in% list.files(old_R_path_etc)) {
         file.copy(from = file.path(old_R_path_etc, "Rprofile.site"),
                   to = file.path(new_R_path_etc, "Rprofile.site"))         
      } else {
         warning('"Rprofile.site" does not exist in your old R-etc folder')
      }      
   }
   
   
   # should we update_packages?
   if(missing(update_packages)) update_packages <- ask.user.yn.question("Do you wish to update your packages in the newly installed R? ", GUI = GUI)
   
   if(update_packages & copy_packages) { # we should not update packages if we didn't copy them first...
      new_Rscript_path <- file.path(new_R_path, "bin/Rscript.exe") # make sure to run the newer R to update the packages.
      update_packages_expression <- paste(new_Rscript_path, ' -e " setInternet2(TRUE); options(repos=structure(c(CRAN=\'https://cran.rstudio.com/\'))); update.packages(checkBuilt=TRUE, ask=FALSE) "')
      #    update_packages_expression <- paste(new_Rscript_path, ' -e "date()"')
      #    update_packages_expression <- paste(new_Rscript_path, ' -e "print(R.version)"')
      system(update_packages_expression, wait = TRUE, intern = TRUE, show.output.on.console = TRUE)
      # makes sure the user will not be able to move on to open the new Rgui, until all of its packages are updated
      # also, makes sure the user will see the output of the update.packages function.
   }
   
#    a <- paste0(file.path("C:/R/R-3.1.2", "bin/Rscript.exe"), " -e setInternet2(TRUE); update.packages(checkBuilt=TRUE, ask=FALSE)")
#    a <- paste0(file.path("C:/R/R-3.1.2", "bin/Rscript.exe"), " -e print(R.version)")
#    system(a, wait = TRUE, show.output.on.console = F) 
#    system(a, wait = TRUE, show.output.on.console = T) 
   
   
   # should we turn Rgui on?
   if(missing(start_new_R)) start_new_R <- ask.user.yn.question("Do you wish to start the Rgui.exe of your new R installation? ", GUI = GUI)
   if(start_new_R) {
      new_Rexe_path <- file.path(new_R_path, "bin/x64/Rgui.exe")      
      if(!file.exists(new_Rexe_path)) new_Rexe_path <- file.path(new_R_path, "bin/i386/Rgui.exe")
      system(new_Rexe_path, wait = F) # start new R gui.  The wait =F makes sure we will be able to close R afterwords.
   }   
   
   # should we turn R off?
   if(missing(quit_R)) quit_R <- ask.user.yn.question("Do you wish to quit R (your workspace will NOT be saved)? ", GUI = GUI)
   if(quit_R) quit(save = "no")
   
   
   return(TRUE)
}



#' @export
updater <- function(...) updateR(...)





#' @title Uninstall an R version
#' @export
#' @aliases uninstall.r
#' @description 
#' Choose an R version to uninstall via a menubar. 
#' By default, the function allows the user to pick an R version to uninstall from a list.
#' Also, the function can be called with using "r_version", where multiple R versions can be supplied
#' and all will be uninstalled.
#' @param r_version a character vector for R versions to uninstall (the format is of the style: "2.15.3"). 
#' default is empty - resulting in a prompt message asking the user what to do.
#' @param GUI If asking the user which R version to uninstall, should the GUI be used? (default is TRUE) 
#' @return the output of \link{system} running the uninstaller
#' @seealso \link{install.R}, \link{updateR}, \link{system}
#' @examples
#' \dontrun{
#' uninstall.R() # choose an R version to uninstall
#' uninstall.R("2.15.3") # will uninstall R 2.15.3
#' uninstall.R(c("2.15.3", "2.14.0")) # will uninstall two R versions (if both exists)
#' uninstall.R("10.10.0") # would pop up the menu options (until R 10.10.0 will be released :D )
#' }
uninstall.R <- function(r_version, GUI = TRUE) {
   # notice that running the uninstall of R does not remove the old library folder!
   
   # get R folders
   R_folders <- get.installed.R.folders()
   choices <- names(R_folders)   
   
   #which R version to uninstall?
   if(!missing(r_version)) the_answer <- which(choices %in% r_version)   
   
   if(missing(r_version) || is.empty(the_answer)) {
            # note: the double | (||) is essential if r_version is missing - 
            #        then the_answer would not be created...
      the_answer <- menu(choices, graphics = GUI, title = "Which R version would you like to UN-install?")            
   }
   
   # uninstall R!
   for(i in seq_along(the_answer)) {
      exe_path <- file.path(R_folders[the_answer[i]], "unins000.exe")
      system(exe_path, wait = F) # start new R gui.  The wait =F makes sure we will be able to close R afterwords.
   }
}




#' @export
uninstall.r <- function(...) { uninstall.R(...) }



#############################################
## Adding a menu item for Rgui (not RStudio)
#############################################

# probably I should add updateR_console, updateR_GUI, and have updateR direct to each of them based on a parameter.
# the function should 





