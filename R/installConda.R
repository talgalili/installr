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

#' @title Downloads and installs miniconda
#' @description Downloads and installs the latest version of miniconda for Windows.
#' @details
#' Miniconda is minimal version of anaconda for python.
#' 
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @author Tal Galili and A. Jonathan R. Godfrey
#' @param version 2 or 3. Default is 3
#' @param binNo 32 or 64. Defaults is "auto" to check system.
#' @param ... extra parameters to pass to \link{install.URL}
#' @examples
#' \dontrun{
#' install.conda() 
#' install.conda(version = 3)
#' install.conda(3)
#' }
install.conda = function (version = 3,
                          bitNo = "auto",
                           ...)
{
  bitNo <- as.character(bitNo)
  if(bitNo == "auto"){
    if(is.x64()){
      bitNo <- "x86_64"
    }else{
      bitNo <- "x86"
    }
  }
  if(bitNo == "64"){
    bitNo <- "x86_64"
  }
  if(bitNo == "32"){
    bitNo <- "x86"
  }
  
  URL <- paste0("https://repo.continuum.io/miniconda/Miniconda",version,"-latest-Windows-",bitNo,".exe")
  
  install.URL(URL, ...) 
}

