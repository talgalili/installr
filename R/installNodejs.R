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

#' @title Downloads and installs nodejs LTS or Current
#' @description Downloads and installs the latest version of nodejs LTS or Current for Windows.
#' @details
#' Nodejs is a programming language which has two versions under active development. 
#' Make sure you know which version is required for the code you have to run, or alternatively, make sure you are developing code that is fit for your chosen version of Nodejs.
#' 
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @author Tal Galili and A. Jonathan R. Godfrey and Chanyub Park
#' @param page_with_download_url a link to the list of download links for Nodejs
#' @param version_number Either LTS or Current. Version LTS will lead to download of v6.11.X
#' @param ... extra parameters to pass to \link{install.URL}
#' @examples
#' \dontrun{
#' install.nodejs() 
#' install.nodejs(,"Current")
#' install.nodejs(,"LTS")
#' }
install.nodejs = function (page_with_download_url = "https://nodejs.org/en/download/",
                           version_number = "LTS",
                           ...)
{
  if(version_number=="Current"){
    page_with_download_url <- paste0(page_with_download_url,"current/")
  }
  page <- readLines(page_with_download_url, warn = FALSE)
  pat <- paste0("Latest ",version_number," Version")
  pat_exact_version <- paste0("[0-9]+\\.[0-9]+\\.[0-9]+")
  target_line <- grep(pat, page, value = TRUE)
  m <- regexpr(pat_exact_version, target_line)
  VersionNo <- regmatches(target_line, m)[1]
  
  if(is.x64()){
    bitNo<-"-x64"
  }else{
    bitNo<-"-x86"
  }
  
  URL <- paste0("https://nodejs.org/dist/v",VersionNo,"/node-v",VersionNo,bitNo,".msi")
  
  install.URL(URL, ...) 
}
