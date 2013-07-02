
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


#' @title Checks if the R session is running within RStudio
#' @description Returns TRUE/FALSE if the R session is running within RStudio or not.
#' @details
#' This function is used in order to check if a GUI can be added to the session or not.
#' @return Returns TRUE/FALSE if the R session is running within RStudio or not.
#' @export
#' @examples
#' \dontrun{
#' is.RStudio() 
#' }
is.RStudio <- function() {
   "tools:rstudio"  %in% search()
}




#' @title Checks if the R session is running within Rgui (Windows OS)
#' @description Returns TRUE/FALSE if the R session is running within Rgui or not.
#' @details
#' This function is used in order to check if a GUI can be added to the session or not.
#' @return Returns TRUE/FALSE if the R session is running within Rgui or not.
#' @export
#' @seealso \link{is.RStudio}, \link{is.windows}
#' @examples
#' \dontrun{
#' is.Rgui() 
#' }
is.Rgui <- function() {
   .Platform$GUI == "Rgui"
}


