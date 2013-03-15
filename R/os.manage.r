
#' @title Shut down the operating system with the command `shutdown'
#' @export
#' @description
#' There is a command \command{shutdown} in both Windows and Linux,
#' and this function uses it to shut down a computer.
#'
#' After the time \code{wait} has passed, R will execute \command{shutdown -s -t 0} (for Windows) or \command{shutdown -h now} to shut down the computer.
#' 
#' This function is a modified version of Yihui's shutdown function from the {fun} package.
#' @param s time to wait before shutting down (in seconds), added to m and h; passed to \code{\link[base]{Sys.sleep}}
#' @param m time to wait before shutting down (in minutes), added to s and h; passed to \code{\link[base]{Sys.sleep}}
#' @param h time to wait before shutting down (in hours), added to s and m; passed to \code{\link[base]{Sys.sleep}}
#' @return The status code of \code{\link[base]{system}}.
#' @author Yihui Xie <\url{http://yihui.name}>, and Tal Galili
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}},
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references \url{http://cos.name/en/topic/shut-down-your-windows-with-r} , \url{https://github.com/yihui/fun/blob/master/R/shutdown.R}
#' @examples
#' \dontrun{
#' ## when your code is extremely time-consuming, you may need this function; e.g. you go to sleep, and R is running long computation... complex graphics... and long long computation... at last,
#' os.shutdown()
#' ## the next day you wake up, "thank you, R" :)
#' }
os.shutdown <- function(s=0, m=0, h=0) {
   
   wait <- s + m*60 + h*60*60
   Sys.sleep(wait)
   
   ifelse(is.windows(), 
          {
		  shell("shutdown -s -f -t 1", wait = F) # -f == forces the shutdown.  And I give R 2 seconds to close.
		  quit("no")
		  }
		  ,# without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)
          system("shutdown -h now"))
}







#' @title Sleeps the operating system (Windows) through a shell command
#' @export
#' @description
#' This sleeps Windows after set amount of time.
#' @param s time to wait before shutting down (in seconds), added to m and h; passed to \code{\link[base]{Sys.sleep}}
#' @param m time to wait before shutting down (in minutes), added to s and h; passed to \code{\link[base]{Sys.sleep}}
#' @param h time to wait before shutting down (in hours), added to s and m; passed to \code{\link[base]{Sys.sleep}}
#' @param first_turn_hibernate_off The command rundll32.exe powrprof.dll,SetSuspendState 0,1,0 for sleep is correct - however, it will hibernate instead of sleep if you don't turn the hibernation off.  I'm not sure this is true, but that's what is explained in the linke (see bellow)
#' @return The status code of \code{\link[base]{shell}}.
#' @author Tal Galili
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}}, 
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references
#' \url{http://superuser.com/questions/42124/how-can-i-put-the-computer-to-sleep-from-command-prompt-run-menu} , \url{http://www.howtogeek.com/howto/windows-vista/quick-tip-create-shutdown-restart-lock-icons-in-windows-vista/}, \url{http://superuser.com/a/135450/28536}
#' @examples
#' \dontrun{
#' ## when your code is extremely time-consuming, you may need this function to run at the end of the simulation.
#' os.sleep()
#' }
os.sleep <- function(s=0, m=0, h=0, first_turn_hibernate_off = TRUE) {
   
   wait <- s + m*60 + h*60*60
   Sys.sleep(wait)
   
   if(first_turn_hibernate_off & is.windows()) {
      shell("powercfg -hibernate off", wait = F) # without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)   
   }
   
   
   ifelse(is.windows(), 
          shell("rundll32.exe powrprof.dll,SetSuspendState 0,1,0", wait = F),# without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)
          warning("This function doesn't handle non-Windows OS (you are welcome to contribute code to let me know how to do it, e-mail: tal.galili@gmail.com)."))
}



#' @title Hibernate the operating system (Windows) through a shell command
#' @export
#' @description
#' This Hibernates Windows after set amount of time.
#' @param s time to wait before shutting down (in seconds), added to m and h; passed to \code{\link[base]{Sys.sleep}}
#' @param m time to wait before shutting down (in minutes), added to s and h; passed to \code{\link[base]{Sys.sleep}}
#' @param h time to wait before shutting down (in hours), added to s and m; passed to \code{\link[base]{Sys.sleep}}
#' @param first_turn_hibernate_on
#' @return The status code of \code{\link[base]{shell}}.
#' @author Tal Galili
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}}, 
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references \url{http://superuser.com/questions/42124/how-can-i-put-the-computer-to-sleep-from-command-prompt-run-menu} , \url{http://www.howtogeek.com/howto/windows-vista/quick-tip-create-shutdown-restart-lock-icons-in-windows-vista/}
#' @examples
#' \dontrun{
#' ## when your code is extremely time-consuming, you may need this function to run at the end of the simulation.
#' os.hibernate()
#' }
os.hibernate  <- function(s=0, m=0, h=0, first_turn_hibernate_on = TRUE) {
   
   wait <- s + m*60 + h*60*60
   Sys.sleep(wait)

   if(first_turn_hibernate_on & is.windows()) {
      shell("powercfg -hibernate on", wait = F) # without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)   
   }   
   
   ifelse(is.windows(), 
          shell("rundll32.exe powrprof.dll,SetSuspendState Hibernate", wait = F),# without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)
          warning("This function doesn't handle non-Windows OS (you are welcome to contribute code to let me know how to do it, e-mail: tal.galili@gmail.com)."))
}




#' @title Locks the operating system (Windows) through a shell command
#' @export
#' @description
#' This locks Windows after set amount of time.
#' @param s time to wait before shutting down (in seconds), added to m and h; passed to \code{\link[base]{Sys.sleep}}
#' @param m time to wait before shutting down (in minutes), added to s and h; passed to \code{\link[base]{Sys.sleep}}
#' @param h time to wait before shutting down (in hours), added to s and m; passed to \code{\link[base]{Sys.sleep}}
#' @return The status code of \code{\link[base]{shell}}.
#' @author Tal Galili
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}}, 
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references \url{http://superuser.com/questions/42124/how-can-i-put-the-computer-to-sleep-from-command-prompt-run-menu} , \url{http://www.howtogeek.com/howto/windows-vista/quick-tip-create-shutdown-restart-lock-icons-in-windows-vista/}
#' @examples
#' \dontrun{
#' ## when your code is extremely time-consuming, you may need this function to run at the end of the simulation.
#' os.lock()
#' }
os.lock  <- function(s=0, m=0, h=0) {
   
   wait <- s + m*60 + h*60*60
   Sys.sleep(wait)
   
   ifelse(is.windows(), 
          shell("Rundll32.exe User32.dll,LockWorkStation", wait = F),# without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)
          warning("This function doesn't handle non-Windows OS (you are welcome to contribute code to let me know how to do it, e-mail: tal.galili@gmail.com)."))
}




#' @title Restarts the operating system (Windows) through a shell command
#' @export
#' @description
#' This restarts Windows after set amount of time.
#' @param s time to wait before shutting down (in seconds), added to m and h; passed to \code{\link[base]{Sys.sleep}}
#' @param m time to wait before shutting down (in minutes), added to s and h; passed to \code{\link[base]{Sys.sleep}}
#' @param h time to wait before shutting down (in hours), added to s and m; passed to \code{\link[base]{Sys.sleep}}
#' @return The status code of \code{\link[base]{shell}}.
#' @author Tal Galili
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}}, 
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references \url{http://superuser.com/questions/42124/how-can-i-put-the-computer-to-sleep-from-command-prompt-run-menu} , \url{http://www.howtogeek.com/howto/windows-vista/quick-tip-create-shutdown-restart-lock-icons-in-windows-vista/}
#' @examples
#' \dontrun{
#' os.restart()
#' }
os.restart  <- function(s=0, m=0, h=0) {
   
   wait <- s + m*60 + h*60*60
   Sys.sleep(wait)
   
   ifelse(is.windows(), 
          shell("Shutdown.exe -r -t 00", wait = F),# without wait =F, the shuting down will not work properly since Windows will wait for R to close (which will be waiting for Windows to shutdown)
          warning("This function doesn't handle non-Windows OS (you are welcome to contribute code to let me know how to do it, e-mail: tal.galili@gmail.com)."))
}





#' @title Gives managing option to the current OS (shutdown, restart, sleep, hibernate, etc...)
#' @export
#' @description
#' A centeral function to run functions for shuting down, restarting, sleeping (etc.) your computer.
#' This will run these functions immediatly.
#' @param use_GUI a logical indicating whether a graphics menu should be used if available.  If TRUE, and on Windows, it will use \link{winDialog}, otherwise it will use \link[utils]{menu}.
#' @param ... not in use
#' @return The status code of \code{\link[base]{system}}.
#' @seealso \code{\link[base]{system}},\code{\link[base]{shell}}, \code{\link[base]{Sys.sleep}}, 
#' \code{\link{is.windows}}, \code{\link{os.shutdown}}, \code{\link{os.sleep}}, \code{\link{os.hibernate}}, \code{\link{os.lock}}, \code{\link{os.restart}}
#' @references \url{http://superuser.com/questions/42124/how-can-i-put-the-computer-to-sleep-from-command-prompt-run-menu} , \url{http://www.howtogeek.com/howto/windows-vista/quick-tip-create-shutdown-restart-lock-icons-in-windows-vista/}
#' \url{http://cos.name/en/topic/shut-down-your-windows-with-r} 
#' @examples
#' \dontrun{
#' ## when your code is extremely time-consuming, you may need this function; e.g. you go to sleep, and R is running long computation... complex graphics... and long long computation... at last,
#' os.manage()
#' ## the next day you wake up, "thank you, R" :)
#' }
os.manage  <- function(use_GUI = TRUE, ...) {
   choices <- c("Shutdown",
                "Sleep",
                "Hibernate",
                "Lock",
                "Restart",
                "Cancel")
   
   the_answer <- menu(choices, graphics = use_GUI, title = "Manage your OS (for Windows)")            
   
   switch(the_answer, 
          os.shutdown(),
          os.sleep(),
          os.hibernate(),
          os.lock(),
          os.restart(),
          return(FALSE)
   )
}
