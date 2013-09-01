


#' @title Adds a menu based GUI for updating R within Rgui
#' @description Adds a menu based GUI for updating R within Rgui.
#' @details
#' This function is used during .onLoad to load the menus for the installr package in Rgui.
#' @author Tal Galili, Dason
#' @return Returns invisible TRUE/FALSE if menus were added or not.
#' @references 
#' My thanks goes to Yihui and Dason, for the idea and help with implementation.  See also: 
#' \url{http://stackoverflow.com/questions/15250487/how-to-add-a-menu-item-to-rgui/}
#' @examples
#' \dontrun{
#' add.installr.GUI() 
#' }
add.installr.GUI <- function() {
   require(utils) # needed for winMenuNames etc.
   
   if(is.windows() & is.Rgui() & !is.RStudio()){
      Update_in_winMenuNames <- "Update" %in% winMenuNames() # I'm making sure this function wasn't used before.  If it was, then running it again might cause bugs...   
      if(!Update_in_winMenuNames) {
         winMenuAdd("Update")
         winMenuAddItem("Update", "Update R", "updateR()")
         winMenuAddItem("Update", "Update R packages", "update.packages(ask = F)")      
         winMenuAddItem("Update", "Install software", "installr()")
         winMenuAddItem("Update", "Manage Windows", "os.manage()")      
         
         
         # add a menu for adding/removing the installr package to startup
         # based on whether or not it is already setup to run on startup.
         if(is_in_.First_in_Rprofile.site("require(installr)")) {
            add_remove_installr_from_startup_menu()
         } else {
            add_load_installr_on_startup_menu()   
         }        
         
         
         return(invisible(TRUE))         
      } else {
         warning("Can not add a new menu item for installr since the menu already has 'Update' in it")   
         return(invisible(FALSE))      
      }
   } else {
      return(invisible(FALSE))      
   }      
}







#' @title Removes the menu based GUI for updating R within Rgui
#' @description Removes the menu based GUI for updating R within Rgui.
#' @details
#' This function is used during .Last.lib to remove the menus for the installr package in Rgui.
#' @return invisible(NULL)
#' @examples
#' \dontrun{
#' add.installr.GUI()  # add menus
#' remove.installr.GUI() # remove them
#' }
remove.installr.GUI <- function() {
   # Thanks to Dason: http://stackoverflow.com/questions/15250487/how-to-add-a-menu-item-to-rgui/15250992?iemail=1#15250992
   # Add GUI (only in Windows's Rgui)
   if(is.windows() & is.Rgui() & !is.RStudio()){
      if("Update" %in% winMenuNames()) winMenuDel("Update")
   }
   return(invisible(NULL))
}






.onLoad <- function(libname, pkgname){
   # if(!is.windows()) warning("The 'installr' package was designed for installing software on Windows. \nIt appears that you are NOT running R on the Windows OS - hence it is not clear if the package has any useful functions to offer you at this point (I'm sorry...).")
   # Thanks for Romain: http://stackoverflow.com/questions/4369334/first-lib-idiom-in-r-packages
   
   # adding and removing menus from the Rgui when loading and detaching the library
   setHook(packageEvent("installr", "attach"), {function(pkgname, libpath) {tryCatch(add.installr.GUI(), error = function(e) invisible(FALSE))}  } )
   setHook(packageEvent("installr", "detach"), {function(pkgname, libpath) {remove.installr.GUI()}  } )
   
}

# menus are added and removed as needed: !!
# require(installr)
# search()
# detach("package:installr")





.onAttach <- function(lib, pkg,...){
   packageStartupMessage(installrWelcomeMessage())
}


installrWelcomeMessage <- function(){
   
   paste("\n",     
         "Welcome to installr version ", utils:::packageDescription("installr")$Version, "\n",
         "\n",
         # "Type ?installr to access the overall documentation and\n",
         # "vignette('installr') for the package vignette.\n",
         # "You can execute a demo of the package via: demo(installr)\n",
         # "\n",  
         "More information is available on the installr project web-site:\n",
         "https://github.com/talgalili/installr/\n",
         "\n",               
         "Contact: <tal.galili@gmail.com>\n",
         "Suggestions and bug-reports can be submitted at: https://github.com/talgalili/installr/issues\n",
         "\n",
         "\t\t\tTo suppress this message use:\n",
         "\t\t\tsuppressPackageStartupMessages(library(installr))\n",  
         sep="")
}




# For some reason, non of the two seems to work.  This is worth checking at some point.
# .onUnload <- function(libpath){
#    remove.installr.GUI()   
# }
# .Last.lib <- function(libpath){
#    installr:::remove.installr.GUI()   
# }



# using "zzz.r" like in devtools...

# When adding new files, make sure they are listed in DESCRIPTION:
# Collate:
#    'create.global.library.r'
# 'install.r'
# 'updateR.r'


# IMPORTANT NOTICE: this will add Collate to the DESCRIPTION file, and if any new r file is added - it will need to be updated.
# Collate:
# +    'create.global.library.r'
# +    'install.r'
# +    'updateR.r'
# +    'zzz.r'

# when a function is renamed, its document in man must be removed - otherwise it may cause problems with the built check (it will try to run the code in the example, and will fail.)
# When all is done, run:
# require(devtools)
# check()
# check(args="--as-cran")
#                 Thanks to: http://stackoverflow.com/questions/10017702/r-cmd-check-options-for-more-rigorous-testing-2-15-0
# file.copy("NEWS", "NEWS.md")
# build_win()
# release()
