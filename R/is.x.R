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





#' @title Checks if some .exe is available in on the Windows machine search PATH
#' @description Checks the existence of an .exe extention in the search path for executable files
#' @param exe_file a character with the name of the executable to be looked for
#' @export
#' @return A boolean vector indicating the existence of each program's availability  on the system.
#' @examples
#' \dontrun{
#' is.exe.installed(c("zip.exe", "Rgui.exe", "blablabla")) # [1]  TRUE  TRUE FALSE
#' is.exe.installed("7z") 
#' }
is.exe.installed <- function(exe_file) {
   all_installed_exe <- list.files(system.PATH())
   exe_file %in% all_installed_exe
}




#' @title Checks if the running OS is windows
#' @description Returns TRUE/FALSE if the R session is running on Windows or not.
#' @details
#' This function is run when the 'installr' package is first loaded in order to check if the current running OS is Windows.
#' If you are running a different OS, then the installr package (at its current form) does not have much to offer you.
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the R session is running on Windows or not. 
#' @export
#' @examples
#' \dontrun{
#' is.windows() # returns TRUE on my machine.
#' }
is.windows <- function(...) unname(Sys.info()["sysname"] == "Windows")
# inspiration: http://dennislwm-star.blogspot.sg/2012/11/r-is-almost-platform-independent.html

#' @title Checks if the running OS is x64
#' @description Returns TRUE/FALSE if the R session is running on Windows 64-bit or not.
#' @details
#' This function is run when the 'installr' package is first loaded in order to check if the current running OS is Windows 64-bit.
#' If you are running a different OS, then the installr package (at its current form) does not have much to offer you.
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the R session is running on Windows 64-bit or not. 
#' @export
#' @examples
#' is.x64() # returns TRUE on my machine.

is.x64 <- function(...)  .Platform$r_arch == "x64"


#' @title Checks if the R session is running within RStudio
#' @description Returns TRUE/FALSE if the R session is running within RStudio or not.
#' @details
#' This function is used in order to check if a GUI can be added to the session or not.
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the R session is running within RStudio or not.
#' @export
#' @examples
#' \dontrun{
#' is.RStudio() 
#' }
is.RStudio <- function(...) {
   "tools:rstudio"  %in% search()
}




#' @title Checks if the R session is running within Rgui (Windows OS)
#' @description Returns TRUE/FALSE if the R session is running within Rgui or not.
#' @details
#' This function is used in order to check if a GUI can be added to the session or not.
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the R session is running within Rgui or not.
#' @export
#' @seealso \link{is.RStudio}, \link{is.windows}
#' @examples
#' \dontrun{
#' is.Rgui() 
#' }
is.Rgui <- function(...) {
   .Platform$GUI == "Rgui" #if running on MAC OS, this would likely be "AQUA"
}






#' @title Checks if an object is empty (e.g: of zero length)
#' @description 
#' Checks if an object is empty (e.g: of zero length) and
#' returns TRUE/FALSE 
#' @details
#' Uses identical and avoids any attribute problems by using the fact that it is the 
#' empty set of that class of object and combine it with an element of that class.
#' @author James (\url{http://stackoverflow.com/users/269476/james})
#' @param x an object
#' @param mode is the object an empty (zero length) 
#' object of this mode (can be "integer", "numeric", and so on...)
#' @param ... none are available.
#' @return Returns TRUE/FALSE if the object is empty or not.
#' @export
#' @seealso \link{integer}, \link{identical}
#' @source \url{http://stackoverflow.com/questions/6451152/how-to-catch-integer0}
#' @examples
#' 
#' is.empty(integer(0)) #TRUE
#' is.empty(0L)         #FALSE
#' is.empty(numeric(0)) #TRUE
#' is.empty(NA) # FALSE 
#' is.empty(FALSE) # FALSE 
#' is.empty(NULL) # FALSE (with a warning)
#' 
#' a <- which(1:3 == 5)
#' b <- numeric(0)
#' is.empty(a)
#' is.empty(a,"numeric")
#' is.empty(b)
#' is.empty(b,"integer")
#' 
is.empty <- function(x, mode=NULL,...){
   
   if(is.null(x)) {
      warning("x is NULL")
      return(FALSE)
   }
      
   if(is.null(mode)) mode <- class(x)
   identical(vector(mode,1),c(x,vector(class(x),1)))
}


# example(is.empty)
# is.empty(xxxxxx)
# fo <- function(x) missing(x)
# fo(xx) 
# fo(xxxxxxxxx) 


