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
#' 
#' Be careful when using the recursive argument.
#' And remember that source("miao.r") and source("miao.R")
#' Are NOT the same...
#' @param subdir (character) sub folder from the current working directoryl 
#' in which the files should be changed. Default is "".
#' @param recursive (logical) FALSE. Should the function keep going
#' into folders and check them as well?
#' @param massage (logical) should we output how many
#' files were changed. (defualt is FALSE)
#' @param ... not used.
#' @return (integer) the number of files changed
#' @examples
#' \dontrun{
#' rename_r_to_R() # changes only .r in the current wd
#' rename_r_to_R("R") # fixing the file ending inside a package directory
#' rename_r_to_R(recursive = TRUE) # Changes 
#' rename_r_to_R(recursive = TRUE, massage = FALSE) # Changes 
#' # ALL of the .r files underneath the current 
#' # working directory
#' }
rename_r_to_R <- function(subdir = ".", recursive = FALSE, massage = TRUE, ...) {
   
   wd <- getwd()
   dir_to_work_on <- file.path(wd, subdir)
#    on.exit(setwd(old_wd))
   
   files <- list.files(dir_to_work_on)
   # Remove dirs!
   isdir <- file.info(file.path(dir_to_work_on,files))$isdir
   files <- files[!isdir]

   # find files with .r
   files_without_ext <- substr(files, 1, nchar(files)-2)
   files_with_R <- paste(files_without_ext, ".R", sep = "")

   ss_r <- grepl("\\.r$", files)
   file.rename(from = file.path(dir_to_work_on,files)[ss_r],
               to = file.path(dir_to_work_on,files_with_R)[ss_r])
   

   n_changes <- sum(ss_r) 
   if(massage) cat("We renamed ", n_changes, " files from .r to .R\n\n")
   if(massage & n_changes>0) cat("We renamed: \n", paste(files[ss_r], collapse = "\n"))

   if(recursive) {
      all_dirs <- list.dirs(full.names = FALSE, recursive = TRUE)
      n_changes_dirs <- sapply(all_dirs, rename_r_to_R, massage = massage)
      n_changes <- n_changes + n_changes_dirs
   }
   
   return(invisible(n_changes))
}

# rename_r_to_R("R")
# rename_r_to_R()
# rename_r_to_R(recursive = TRUE)
# rename_r_to_R(recursive = TRUE, massage = FALSE)


# grepl("\\.r$", c(".r", "aaa.r", "aaa.r.a", "aa.aar"))

# sapply("R", rename_r_to_R, massage = massage)
# rename_r_to_R("R")
