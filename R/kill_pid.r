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







#' @title Get the running processes in windows task manager 
#' @export
#' @description Returns a data.frame with the current running processes (Windows only).
#' @param ... not used.
#' @return a data.frame with the current running processes.
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}
#' @examples
#' \dontrun{
#' # create several running processes of Rscript (to shitdown)
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' # here are there pid numbers:
#' get_Rscript_PID() 
#' # let's kill them:
#' kill_all_Rscript_s()
#' # they are gone...
#' get_Rscript_PID() # we no longer have Rscripts running
#' }
get_tasklist <- function(...) {
	tasklist_raw <- system("tasklist",intern = TRUE)
	
	tasklist_raw_mat <- do.call(rbind,strsplit(tasklist_raw[-1:-3], "   +")) # at least 3 spaces are needed (so to not have a problem with processes which includes a space in their name
	# the -1:-3 is to remove the headers.
	
	# could also use faster ways of doing it:
	# http://www.r-bloggers.com/concatenating-a-list-of-data-frames/
	
	image_name <- tasklist_raw_mat[,1]
	
	tasklist_raw_mat_fix_pids <- do.call(rbind,strsplit(tasklist_raw_mat[,2], " ")) 
	
	session_num <- tasklist_raw_mat[,3]
	
	mem_usage <- tasklist_raw_mat[,4]
	mem_usage <- gsub(" K|,", "", mem_usage)
	mem_usage <- as.numeric(mem_usage)
	
	
	tasklist <- data.frame(image_name, tasklist_raw_mat_fix_pids, session_num, mem_usage, stringsAsFactors  = FALSE)
	
	# colnames(tasklist_df) <- strsplit(tasklist_raw[2], "   +")[[1]]
	# won't work because of the names with 1 space
	colnames(tasklist) <- c("Image Name", "PID", "Session Name", "Session#" , "Mem Usage (K)")		

	tasklist[,"PID"] <- as.integer(tasklist[,"PID"]) # PID should be an integer!
	
	tasklist
}






#' @title kill (i.e.: stop) running processes by there pid
#' @export
#' @description kill (i.e.: stop) running processes by there pid.
#' It spawns a new Rscript which runs \link[tools]{pskill} on the pid-s
#' @param pid an integer vector with process id numbers (i.e.: can kill severa pid at once!)
#' @param s numeric. number of seconds to wait before killing the processes
#' @param m numeric. number of minutes to wait before killing the processes
#' @param h numeric. number of hours to wait before killing the processes
#' @param ... not used.
#' @return output from system
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' pskill details from microsoft homepage: \url{http://technet.microsoft.com/en-us/sysinternals/bb896683.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' @examples
#' \dontrun{
#' # create several running processes of Rscript (to shitdown)
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' # here are there pid numbers:
#' get_Rscript_PID() 
#' # let's kill them:
#' kill_pid(get_Rscript_PID())
#' # they are gone...
#' get_Rscript_PID() # we no longer have Rscripts running
#' }
kill_pid <- function(pid, s=0, m=0, h=0,...) {
   
   wait <- s + m*60 + h*60*60		
   
   # https://www.inkling.com/read/r-cookbook-paul-teetor-1st/chapter-3/recipe-3-13 
   # RScript options
   
   text_to_kill_pid <- 
      paste("Rscript --slave --no-restore --no-save --no-init-file  -e ",
            "Sys.sleep(", wait, ");",
            "require(tools);",
            "pskill(", pid, ");", 
            sep = "")
   # it works fine with \n :)
   # cat(text_to_kill_Rscript)
   
   # a vectorized form of "system". Accepting a vector of commands
   V_system <- Vectorize(system, vectorize.args="command")	
   
   V_system(text_to_kill_pid, wait = FALSE)	
}





#' @title Find the pid of a process by name
#' @export
#' @description Returns a vector with the process ID (pid) 
#' for all processes with a particular name.
#' @param process a character vector of process names.
#' @param exact logical (FALSE). should we get exact match to process name, or
#' can we use just partial matching.
#' @param ... not used.
#' @return an integer vector with the process ID (pid) of the processes.
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' @examples
#' \dontrun{
#' get_pid("rsession") # finds it
#' get_pid("rsession", exact = TRUE) # doesn't find it
#' get_pid("rsession.exe", exact = TRUE) # finds it
#' get_pid(c("wininit", "winlogon"), exact = TRUE) # doesn't find it
#' get_pid(c("wininit", "winlogon")) # finds it
#' }
get_pid <- function(process, exact = FALSE, ...) {
   tasklist <- get_tasklist()
   if(exact) {
      ss <- tasklist[,1] %in% process
   } else { # default
      ss <- grep(paste(process, collapse = "|"), tasklist[,1])
         
   }   
   process_pid <- tasklist[ss, "PID"]   
   process_pid
}
# str(get_Rscript_PID())





#' @title kill (i.e.: stop) running processes by there process name
#' @export
#' @description kill (i.e.: stop) running processes by there process name
#' It spawns a new Rscript which runs \link[tools]{pskill} on the pid-s
#' per process name.
#' @param process a character vector of process names.
#' @param s numeric. number of seconds to wait before killing the processes
#' @param m numeric. number of minutes to wait before killing the processes
#' @param h numeric. number of hours to wait before killing the processes
#' @param exact logical (FALSE). should we get exact match to process name, or
#' can we use just partial matching.
#' @param ... not used.
#' @return output from system
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' pskill details from microsoft homepage: \url{http://technet.microsoft.com/en-us/sysinternals/bb896683.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' @examples
#' \dontrun{
#' # create several running processes of Rscript (to shitdown)
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' # here are there pid numbers:
#' get_Rscript_PID() 
#' # let's kill them:
#' kill_process("Rscript")
#' # they are gone...
#' get_Rscript_PID() # we no longer have Rscripts running
#' }
kill_process <- function(process, s=0, m=0, h=0, exact = FALSE, ...) {
   the_pids <- get_pid(process, exact = exact)
   kill_pid(the_pids, s=s,m=m,h=h)   
}









#' @title Get the running "Rscript" processes PID
#' @export
#' @description Returns a vector with the process ID (pid) of the "Rscript" 
#' processes which are currently running.
#' @param ... not used.
#' @return an integer vector with the process ID (pid) of the "Rscript" processes.
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' @examples
#' \dontrun{
#' # create several running processes of Rscript (to shitdown)
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' # here are there pid numbers:
#' get_Rscript_PID() 
#' # let's kill them:
#' kill_all_Rscript_s()
#' # they are gone...
#' get_Rscript_PID() # we no longer have Rscripts running
#' }
get_Rscript_PID <- function(...) {
	Rscript_PID <- get_pid("Rscript.exe")      
	Rscript_PID
}
# str(get_Rscript_PID())






#' @title kill (i.e.: stop) all running "Rscript" processes
#' @export
#' @description kill (i.e.: stop) all running "Rscript" processes 
#' based on their process ID (pid)
#' @param s numeric. number of seconds to wait before killing the processes
#' @param m numeric. number of minutes to wait before killing the processes
#' @param h numeric. number of hours to wait before killing the processes
#' @param ... not used.
#' @return an integer vector with the process ID (pid) of the "Rscript" processes.
#' @references
#' tasklist details from microsoft homepage: \url{http://technet.microsoft.com/en-us/library/bb491010.aspx}
#' pskill details from microsoft homepage: \url{http://technet.microsoft.com/en-us/sysinternals/bb896683.aspx}
#' @seealso \code{\link{get_tasklist}}, \code{\link{get_Rscript_PID}}, \code{\link{get_pid}},
#' \code{\link{kill_pid}}, \code{\link{kill_all_Rscript_s}}, \link[tools]{pskill}
#' @examples
#' \dontrun{
#' # create several running processes of Rscript (to shitdown)
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' system("Rscript -e repeat{2+2}", wait = FALSE) # this process should be stuck   
#' # here are there pid numbers:
#' get_Rscript_PID() 
#' # let's kill them:
#' kill_all_Rscript_s()
#' # they are gone...
#' get_Rscript_PID() # we no longer have Rscripts running
#' }
kill_all_Rscript_s <- function(s=0, m=0, h=0,...) {
	# get Rscripts PID
	Rscript_PID <- get_Rscript_PID()
   # kill'em
	kill_pid(Rscript_PID, s=s, m=m, h=h, ...)	
}


