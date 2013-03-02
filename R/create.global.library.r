# How it all started, a long long time ago: http://stackoverflow.com/questions/1401904/painless-way-to-install-a-new-version-of-r-on-windows

# These functions are based on what is described here:
# http://www.r-statistics.com/2011/04/how-to-upgrade-r-on-windows-7/

# global library

# Run this in the old R version:

#Old.R.RunMe()

# It will:

# 1. Create a new global library folder (if it doesn't exist)

# 2. Copy to the new "global library folder" all of your packages from the old R installation



# Run this in the New R version:

#New.R.RunMe()

# It will:

# 1. Create a new global library folder (if it doesn't exist)

# 2. Premenantly point to the Global library folder

# 3. Make sure that in the current session - R points to the "Global library folder"

# 4. Delete from the "Global library folder" all the packages that already exist in the local library folder of the new R install

# 5. Update all packages.





# grepl("/|\\\\", "ab")
# strsplit("a/b\\c", "/|\\\\")
#




create.global.library <- function(global_library_folder)
{
   # global_library_folder = "C:/Program Files/R/library"
   # global_library_folder is null then if we assume it is of the shape: # "C:/Program Files/R/library"
   
   # Step 1: In case it is not defined - decide what the global folder is.   
   if(missing(global_library_folder) | is.null(global_library_folder))  {  # then decide what it should be
      # finding the parent directoy of R (into which we will add the library directory)
      R_parent_lib <- paste(head(strsplit(R.home(), "/|\\\\")[[1]], -1), collapse = "/") # the strsplit is seperating the path whether it is / or \\ (but since \\ is a problem, I need to cancel it with \\\\)      
      # if global_library_folder isn't defined, then we assume it is of the form: "C:\\Program Files\\R\\library"
      global_library_folder <- file.path(R_parent_lib, "library")         
      cat("global_library_folder wasn't specified, and is set by default to be: ", global_library_folder, "\n")
   }   
   
   # Step 2: check if the global lib folder exists -
   # if not -> create it.
   if(file.exists(global_library_folder)) # no better solution: http://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
   {   
      cat(paste("The path:" , global_library_folder, "already exist. (no need to create it) \n"))
      did_we_create_global_library_folder <- F
   } else { # If global lib folder doesn't exist - create it.
      dir.create(global_library_folder)
      did_we_create_global_library_folder <- T
   }
   
   return(did_we_create_global_library_folder)
}




xx.global.library <- function(
                                  copy_packges_to_global_library_folder = F, # should be T if this is the first time you are running this (either from the old or the new version of R you have just installed).
                                  # copy_packages_from_old_R_installation = F, # should be T if you are running this for the first time, from a new installation of R, after having an old installation of R (where all of your old packages are)                                  
                                  del_packages_that_exist_in_home_lib = F,
                                  update_all_packages = T,
                                  global_library_folder = NULL,
                                  quit_R
                                  )
{
   # global_library_folder = "C:/Program Files/R/library"
   # global_library_folder is null then if we assume it is of the shape: # "C:/Program Files/R/library"
   
   did_we_create_global_library_folder <- create.global.library(global_library_folder)
   

   # Step 3: Create a list of all the packages we have in the current .libPaths (shouldn't include the new folder, but just in case - we'll take care of that soon)
   # Copy packages from current lib folder to the global lib folder

   if(copy_packges_to_global_library_folder) {  # this is used if we have an old version of R and we've just installed a new version
      packages_in_libs <- NULL
      the_libPaths <- .libPaths()

      cat("Do you have packages from on OLD installation of R (not the one you are running this script from)\n from which you'd like to move packages into: " , global_library_folder, "  \n")
      answer <- readline("y(es)/n(o): ")
      if(substr(tolower(answer), 1,1) == "y") {
         # I am just going to ask the user 
         # I am NOT going to assume that the latest version of R is the last folder.  And that the previous version of R is the folder before it.
         old_R_folders <- data.frame(Folders = list.files(R_parent_lib), stringsAsFactors=F)
         ROW_id <- ask.user.for.a.row(old_R_folders, 
                                      header_text = 
                                         paste("From which of the following R folders \n would you like to copy your old packages to ",global_library_folder, " ?"))
         the_libPaths <- c(file.path(R_parent_lib, old_R_folders[ROW_id, 1], "library"),
                           the_libPaths) # add this old version of R to the pool of places to check.
      } 
      
      number_of_lib_dir <- length(the_libPaths)
      for(i in seq_len(number_of_lib_dir))
      {
         # this is good in case we already used some externel library folder, and we wish to move it...
         packages_in_libs <- c(packages_in_libs ,
                               paste( the_libPaths[i],"/", list.files(the_libPaths[i]),sep = "")
         )
      }
      
      packages_in_global_library_folder <- list.files(global_library_folder) # should be empty if we have just created this folder for the first time
            
      # Step 4: check if the current (old) library folder has any packages we might wish to move. e.g: non-"high" packages (e.g: packages installed after R was installed), or packages in the global_library_folder
      packages_to_NOT_move <- c(installed.packages(priority = "high")[,"Package"], # some context on "high" packages: http://stackoverflow.com/questions/9700799/what-is-difference-between-r-base-and-r-recommended-packages
                                packages_in_global_library_folder)  # we wish to NOT move into the global directory packages which are from "base R", and packages which are ALREADY in the packages_in_global_library_folder.
      names_of_packages_in_libs <- sapply(strsplit(packages_in_libs, "/|\\\\"), function(x) tail(x, 1))  # this is stripping the last location from each folder (hence: the package name)
      ss_packages_to_NOT_move_froms_libs <- names_of_packages_in_libs %in%  packages_to_NOT_move
      packages_in_libs_to_move <- packages_in_libs[!ss_packages_to_NOT_move_froms_libs]
      
      
      # COPY old packages to the new global folder:
      cat("-----------------------","\n")
      cat("I am now copying ", length(packages_in_libs_to_move) ," packages from the old librarys folders' to:","\n")
      cat(global_library_folder,"\n")
      cat("-----------------------","\n")
      flush.console()  # refresh the console so that the user will see the massage
      
      folders.copied <- file.copy(from = packages_in_libs_to_move,    # copy folders
                                  to = global_library_folder,
                                  overwrite = TRUE,
                                  recursive =TRUE)
      
      cat("Success.","\n")
      cat(paste("We finished copying all of your packages (" , sum(folders.copied), "packages ) to the new library folder at:"),"\n")
      cat(global_library_folder,"\n")
      cat("-----------------------","\n")     
      
      
   }   
   
   
   #####################################################
   #####################################################
   
   # Based on:
   # help(Startup)
   # checking if "Renviron.site" exists - and if not -> create it.
   Renviron.site.loc <- paste(R.home(), "\\etc\\Renviron.site", sep = "")
   if(!file.exists(Renviron.site.loc))
   {   # If "Renviron.site" doesn't exist (which it shouldn't be) - create it and add the global lib line to it.
      cat(paste("R_LIBS='",global_library_folder, "'\n",sep = "") ,
          file = Renviron.site.loc)				 
      cat(paste("The file:" , Renviron.site.loc, "Didn't exist - we created it and added your 'Global library link' (",global_library_folder,") to it.\n"))
   } else {
      cat(paste("The file:" , Renviron.site.loc, "existed and we could NOT add some lines!  make sure you add the following line by yourself:","\n"))
      cat(paste("R_LIBS=",global_library_folder,"\n", sep = "") )
      cat(paste("To the file:",Renviron.site.loc,"\n"))
   }
   
   
   
   # Setting the global lib for this session also
   .libPaths(global_library_folder)	# This makes sure you don't need to restart R so that the new Global lib settings will take effect in this session also
   # .libPaths(new="C:/PROGRA~1/R/library")
   # .libPaths()
   # This line could have also been added to:
   
   # /etc/Rprofile.site
   
   # and it would do the same thing as adding "Renviron.site" did
   
   cat("Your library paths are: ","\n")
   cat(.libPaths(),"\n")	
   flush.console()  # refresh the console so that the user will see the massage
   

   
   if(del_packages_that_exist_in_home_lib)
   {
      cat("We will now delete package from your Global library folder that already exist in the local-install library folder","\n")
      flush.console()  # refresh the console so that the user will see the massage
      package.to.del.from.global.lib <- 		paste( paste(global_library_folder, "/", sep = ""),
                                                 list.files(paste(R.home(), "\\library\\", sep = "")),
                                                 sep = "")			
      # this code can be made nicer
      packages_in_libs <- package.to.del.from.global.lib
      file.path(R.home(), "library", installed.packages(priority = "high")[,"Package"])
      packages_to_NOT_move <- installed.packages(priority = "high")[,"Package"] # we wish to NOT move into the global directory packages which are from "base R", and packages which are ALREADY in the packages_in_global_library_folder.
      names_of_packages_in_libs <- sapply(strsplit(packages_in_libs, "/|\\\\"), function(x) tail(x, 1))  # this is stripping the last location from each folder (hence: the package name)
      ss_packages_to_NOT_move_froms_libs <- names_of_packages_in_libs %in%  packages_to_NOT_move
      packages_in_libs_to_move <- packages_in_libs[!ss_packages_to_NOT_move_froms_libs]
      
      package.to.del.from.global.lib <- packages_in_libs_to_move
      
      number.of.packages.we.will.delete <- length(package.to.del.from.global.lib)
      if(package.to.del.from.global.lib >0 ) {
         # maybe add a user input here...
         deleted.packages <- unlink(package.to.del.from.global.lib , recursive = TRUE)   # delete all the packages from the "original" library folder (no need for double folders)      
         cat(paste(number.of.packages.we.will.delete,"Packages where deleted."),"\n")
      }
   }
   
   
   
   if(update_all_packages)
   {
      # Based on:
      # http://cran.r-project.org/bin/windows/base/rw-FAQ.html#What_0027s-the-best-way-to-upgrade_003f
      cat("We will now update all your packges \n")
      flush.console()  # refresh the console so that the user will see the massage
      update.packages(checkBuilt=TRUE, ask=FALSE)
   }
   
   
   # To quite R ?
   if(missing(quit_R))
   {
      cat("Can I close R?  (WARNING: your enviornment will *NOT* be saved)","\n")
      answer <- readline("y(es)/n(o): ")
      if(substr(tolower(answer), 1,1) == "y") {
         quit_R <- T
      } else {
         quit_R <- F
      }
   } else {
      quit_R <- F
   }
   if(quit_R) quit(save = "no")
}













































