
#' @title Add a code line to Rprofile.site .First
#' @description 
#' Goes through Rprofile.site text, finds the .First function - and add
#' a line of code to the beginning of it.
#' 
#' @param code A character scalar with code to add at the beginning of the
#' .First function in Rprofile.site
#' @param indent a character scalar indicating the text to be added before
#' code. Default is a tab.
#' @param ... not used.
#' @return NULL
#' @references
#' \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
#' \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
#' \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
#' \url{http://www.statmethods.net/interface/customizing.html}
#' @examples
#' \dontrun{
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' add_to_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # TRUE
#' remove_from_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' # this would still leave .First
#' }
add_to_.First_in_Rprofile.site <- function(code, indent = "\t", ... ) {
   if(length(code) != 1) stop("'code' should be a character scalar (e.g: of length 1)")
   Rprofile.site_text <- readLines(file.path(R.home(), "etc", "Rprofile.site"))
   nrows <- length(Rprofile.site_text)
   
   # looks for .First with only spaces (or no spaces at all, that is why I use * instead of +), 
   #     before it.
   ss_.First_location <- grep("^ *\\.First", Rprofile.site_text)
   if(length(ss_.First_location)==0) {
      # add a new .First
      new_Rprofile.site_text <- c(
         Rprofile.site_text,
         ".First <- function(){",
         paste(indent,code),
         "}"
      )
      
   } else {
      # add "txt" at the beginning of .First
      ss_open_curly_location <- grep(" *\\{", Rprofile.site_text)
      # let's get only the open curly brackets { wjocj cp,e after the .First
      ss_open_curly_location <- ss_open_curly_location[ss_open_curly_location >=ss_.First_location]
      ss_.First_open_curly_location <- ss_open_curly_location[1]
      
      new_Rprofile.site_text <- c(
         Rprofile.site_text[1:ss_.First_open_curly_location],
         paste(indent,code),
         Rprofile.site_text[c(ss_.First_open_curly_location+1):nrows])
      
   }
   
   writeLines(new_Rprofile.site_text, file.path(R.home(), "etc", "Rprofile.site"))
   
   
   return(NULL)
}






#' @title Remove a code line from Rprofile.site .First
#' @description 
#' Goes through Rprofile.site text, finds a line of code - and removes it.
#' @param code A character scalar with code to add at the beginning of the
#' .First function in Rprofile.site
#' @param indent a character scalar indicating the text to be added before
#' code. Default is a tab.
#' @param ... passed to \link{grep}
#' @param fixed passed to \link{grep}
#' @return logical. Did we remove that line or not (in case it was not there)
#' @references
#' \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
#' \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
#' \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
#' \url{http://www.statmethods.net/interface/customizing.html}
#' @examples
#' \dontrun{
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' add_to_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # TRUE
#' remove_from_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' # this would still leave .First
#' }
remove_from_.First_in_Rprofile.site <- function(code, fixed = TRUE, ... ) {
   if(length(code) != 1) stop("'code' should be a character scalar (e.g: of length 1)")
   Rprofile.site_text <- readLines(file.path(R.home(), "etc", "Rprofile.site"))
   #    nrows <- length(Rprofile.site_text)
   
   # looks for .First with only spaces (or no spaces at all, that is why I use * instead of +), 
   #     before it.
   ss_.First_location <- grep(code, Rprofile.site_text, fixed = fixed, ...)
   if(length(ss_.First_location) == 0) return(FALSE)
   
   new_Rprofile.site_text <- Rprofile.site_text[-ss_.First_location]
   
   writeLines(new_Rprofile.site_text, file.path(R.home(), "etc", "Rprofile.site"))
   return(TRUE)
}








#' @title Remove a code line from Rprofile.site .First
#' @description 
#' Goes through 
#' @param code A character scalar with code to add at the beginning of the
#' .First function in Rprofile.site
#' @param indent a character scalar indicating the text to be added before
#' code. Default is a tab.
#' @param fixed passed to \link{grep}
#' @param ... passed to \link{grep}
#' @return logical, if code is in Rprofile.site or not.
#' @references
#' \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
#' \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
#' \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
#' \url{http://www.statmethods.net/interface/customizing.html}
#' @examples
#' \dontrun{
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' add_to_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # TRUE
#' remove_from_.First_in_Rprofile.site("suppressMessages(require(installr))")
#' is_in_.First_in_Rprofile.site("suppressMessages(require(installr))") # FALSE
#' # this would still leave .First
#' }
is_in_.First_in_Rprofile.site <- function(code, fixed= TRUE, ... ) {
   if(length(code) != 1) stop("'code' should be a character scalar (e.g: of length 1)")
   Rprofile.site_text <- readLines(file.path(R.home(), "etc", "Rprofile.site"))
   
   is_it_in_it <- any(grepl(code, Rprofile.site_text,fixed=fixed, ...))
   
   return(is_it_in_it)
}














#' Add menu item for having installr load on startup
#' @export
add_load_installr_on_startup_menu <- function(...) {
   
   add_installr_startup_txt <- "Load 'installr' on startup"
   remove_installr_startup_txt <- "Remove 'installr' from startup"
   
   if(is.windows() & is.Rgui() & !is.RStudio()){
      Update_in_winMenuNames <- "Update" %in% winMenuNames() # I'm making sure this function wasn't used before.  If it was, then running it again might cause bugs...   
      if(Update_in_winMenuNames) {
         
         update_menus <- names(winMenuItems("Update"))
         
         if(remove_installr_startup_txt %in% update_menus) {
            # remove menu-item
            winMenuDelItem("Update", remove_installr_startup_txt)
            # add menu-item
         }
         winMenuAddItem("Update", add_installr_startup_txt, "load_installr_on_startup()")
         
         
         return(invisible(TRUE))         
      } else {
         warning("Update menu item is not present - can not add menuitem for having installr load on startup")   
         return(invisible(FALSE))      
      }
   } else {
      return(invisible(FALSE))      
   }         
}





#' Add menu item for having installr NOT load on startup
#' @export
add_remove_installr_from_startup_menu <- function(...) {
   
   add_installr_startup_txt <- "Load 'installr' on startup"
   remove_installr_startup_txt <- "Remove 'installr' from startup"
   
   if(is.windows() & is.Rgui() & !is.RStudio()){
      Update_in_winMenuNames <- "Update" %in% winMenuNames() # I'm making sure this function wasn't used before.  If it was, then running it again might cause bugs...   
      if(Update_in_winMenuNames) {
         
         update_menus <- names(winMenuItems("Update"))
         
         if(add_installr_startup_txt %in% update_menus) {
            # remove menu-item
            winMenuDelItem("Update", add_installr_startup_txt)
         }
         # add menu-item
         winMenuAddItem("Update", remove_installr_startup_txt, "rm_installr_from_startup()")
         #          }
         
         return(invisible(TRUE))         
      } else {
         warning("Update menu item is not present - can not add menuitem for having installr load on startup")   
         return(invisible(FALSE))      
      }
   } else {
      return(invisible(FALSE))      
   }         
}

















#' @title Have the installr package load on startup
#' @export
#' @param ... not used.
#' @return invisible(NULL)
#' @references
#' \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
#' \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
#' \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
#' \url{http://www.statmethods.net/interface/customizing.html}
#' @examples
#' \dontrun{
#' load_installr_on_startup()
#' }
load_installr_on_startup <- function(...) {
   add_remove_installr_from_startup_menu()
   if(!is_in_.First_in_Rprofile.site("suppressMessages(require(installr))")) {
      add_to_.First_in_Rprofile.site("suppressMessages(require(installr))")
   }
   invisible(NULL)
}


#' @title Have the installr package NOT load on startup
#' @export
#' @param ... not used.
#' @return invisible(NULL)
#' @references
#' \url{http://stackoverflow.com/questions/1395301/how-to-get-r-to-recognize-your-working-directory-as-its-working-directory}
#' \url{http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile}
#' \url{http://www.noamross.net/blog/2012/11/2/rprofile.html}
#' \url{http://www.statmethods.net/interface/customizing.html}
#' @examples
#' \dontrun{
#' load_installr_on_startup()
#' rm_installr_from_startup()
#' }
rm_installr_from_startup <- function(...) {
   add_load_installr_on_startup_menu()
   if(is_in_.First_in_Rprofile.site("suppressMessages(require(installr))")) {
      remove_from_.First_in_Rprofile.site("suppressMessages(require(installr))")
   }
   invisible(NULL)
}


