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





#' @title Rename files' extensions in a folder from .r to .R
#' @description 
#' Rename files' extensions in a folder from .r to .R.
#' @details
#' This came after a discussion with Hadley, JJ, and Martin
#' leading to the realization that since we are using the
#' R language (and not the r language), the standard is to use
#' .R files instead of .r
#' @param subdir sub folder from the current working directoryl 
#' in which the files should be changed
#' @param ... not used.
#' @return (integer) the number of files changed
#' @examples
#' \dontrun{
#' rename_r_to_R("R") # fixing the file ending inside a package directory
#' }
rename_r_to_R <- function(subdir, ...) {
   
   if(!missing(subdir)) {
      old_wd <- getwd()
      setwd(file.path(old_wd, subdir))
      on.exit(setwd(old_wd))
   }
   
   files <- list.files()
   # find files with .r
   files_without_ext <- substr(files, 1, nchar(files)-2)
   files_with_R <- paste(files_without_ext, ".R", sep = "")

   ss_r <- grepl(".r$", files)
   file.rename(from = files[ss_r], to = files_with_R[ss_r])
   
   n_changes <- sum(ss_r)
   cat("We renamed ", n_changes, " files from .r to .R")
   
   return(invisible(n_changes))
}

# turn_r_to_R("R")

