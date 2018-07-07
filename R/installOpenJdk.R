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
#' @title Downloads and set path openjdk
#' @description Downloads and set path the latest version of openjdk for Windows.
#' @details
#' install openjdk 9 or 10 version for windows.
#' 
#' @return TRUE/FALSE - was the installation successful or not.
#' @export
#' @author Tal Galili and A. Jonathan R. Godfrey
#' @param version 9 or 10 is passible. Default is 10.
#' @param page_with_download_url where to download. Default is \url{http://jdk.java.net/java-se-ri/}
#' @param path where to set java. Defulat path is C:/java
#' @examples
#' \dontrun{
#' install.java() 
#' install.java(version = 10)
#' install.java(10)
#' }
install.java <- function(version = 10,
                         page_with_download_url = "http://jdk.java.net/java-se-ri/",
                         path = "C:/java") {
  page <- readLines(paste0(page_with_download_url, version), warn = F)
  target_line <- grep("windows", page, value = T)[1]
  
  pat <- "https://.+?(zip|gz)"
  m <- regexpr(pat, target_line)
  
  URL      <- regmatches(target_line, m)
  
  filename <- file.path(tempdir(), file.name.from.url(URL))
  download.file(URL, destfile = filename, mode = 'wb')
  if (grepl("zip$", URL)) {
    unzip(zipfile = filename, exdir = path)
  }
  if (grepl("gz$", URL)) {
    untar(tarfile = filename, exdir = path)
  }
  
  path_list <- list.dirs(path)
  home_path <- grep("jdk-[0-9]+$", path_list, value = T)
  home_path <- grep(version, home_path, value = T)
  
  profiled <- paste0("Sys.setenv(JAVA_HOME='", home_path, "')")
  
  if (!file.exists("~/.Rprofile")) {
    file.create("~/.Rprofile")
  }
  
  pre <- readLines("~/.Rprofile", warn = F)
  
  if(any(grepl("JAVA_HOME",pre))){
    pre <- pre[-grep("JAVA_HOME", pre)]
  }
  profiled <- c(pre, profiled)
  write(profiled, "~/.Rprofile")
  Sys.setenv(JAVA_HOME=home_path)
}

#' @export
install.Java <- function(...) install.java(...)

#' @export
install.jdk <- function(...) install.java(...)

#' @export
install.Jdk <- function(...) install.java(...)

#' @export
install.openjdk <- function(...) install.java(...)

#' @export
install.OpenJdk <- function(...) install.java(...)
