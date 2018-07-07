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

#' @title Downloads and installs python 2 or 3
#' @description Downloads and installs the latest version of python 2 or 3 for Windows.
#' @details
#' Python is a programming language which has two versions under active development. 
#' Make sure you know which version is required for the code you have to run, or alternatively, make sure you are developing code that is fit for your chosen version of Python. In addition, the Python installers are specific to 32 or 64 bit windows architectures. 
#' 
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @author Tal Galili and A. Jonathan R. Godfrey
#' @param page_with_download_url a link to the list of download links for Python
#' @param x64 logical: fetch a 64 bit version. default checks architecture of current R session.
#' @param version_number Either 2 or 3. Version 2/3 will lead to download of v2.7.xx/3.6.xx respectively.
#' @param ... extra parameters to pass to \link{install.URL}
#' @examples
#' \dontrun{
#' install.python() 
#' install.python(,3)
#' install.python(,2)
#' }
install.python = function (page_with_download_url = "https://www.python.org/downloads/windows/",
                           version_number = 3,
                           x64 = is.x64(),
                             ...)
{
  page <- readLines(page_with_download_url, warn = FALSE)
  pat <- paste0("Latest Python ",version_number," Release")
  pat_exact_version <- paste0("Python ",version_number,"[0-9.]+")
  target_line <- grep(pat, page, value = TRUE)
  m <- regexpr(pat_exact_version, target_line)
  VersionNo <- regmatches(target_line, m)[1]
  VersionNo <- gsub("Python ", "", VersionNo)
  
  # https://www.python.org/ftp/python/3.5.1/python-3.5.1.exe
  # "https://www.python.org/ftp/python/3.5.1/python-3.5.1.exe"
  # or annoyingly, the extension for the Python 2.7 installer is msi. and requires
  # https://www.python.org/ftp/python/2.7.11/python-2.7.11.msi
  file_extension <- switch(as.character(version_number), 
							"2" = "msi",
							"3" = "exe")
  URL <- paste0("https://www.python.org/ftp/python/",VersionNo, "/python-",VersionNo  , ".", file_extension)[1]
  if(x64){ #different filenames for P3  Py27#Py3
    URL <- sub(".exe", "-amd64.exe", URL)  #Py3
    URL <- sub(".msi", ".amd64.msi", URL)  #Py2.7
  }  

  install.URL(URL, ...) 
}

