# file.name.from.url <- function(URL) tail(strsplit(URL,   "/")[[1]],1)
# # sapply(urls, file.name.from.url)


#' @title Download RStudio CRAN mirror data files into a folder
#' @export
#' @description 
#' 
#' This function downlaod these files based on the code from the downlaod page (\url{http://cran-logs.rstudio.com/}) into a temporary folder.
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param START the defaults is 5 days before today. A character string of the START date for files to be downloaded. The date format is "YYYY-MM-DD".
#' @param END the defaults is today. A character string of the END date for files to be downloaded. 
#' The date format is "YYYY-MM-DD".
#' @param log_folder the folder into which we would like the files to be downloaded to. Default is the temporary folder picked by \link{tempdir}.
#' @param trunc_END_date_to_today default is TRUE. Makes sure that if END date is later then today,
#'  the END date will be change to today
#'  (since otherwise, we will only get many 404 errors)
#' @param ... not in use.
#' @return Returns the value of log_folder.
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' # RStudio_CRAN_data_folder <- download_RStudio_CRAN_data()
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' 
#' # barplots: (more functions can easily be added in the future)
#' barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
#' barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#' }
download_RStudio_CRAN_data <- function(START = as.Date(Sys.time())-5, END = as.Date(Sys.time()), log_folder = tempdir(), trunc_END_date_to_today = TRUE,...) {
   # Here's an easy way to get all the URLs in R
   START <- as.Date(START)
   END <- as.Date(END)
   
   # If END date is much further away than today (based on system definitions), it should be tka
   if((END > as.Date(Sys.time())+1) & trunc_END_date_to_today) END <- as.Date(Sys.time())+1 # the +1 is just for the case of a difference in times between the computer and the RStudio server.
   
   all_days <- seq(START, END, by = 'day')
   
   year <- as.POSIXlt(all_days)$year + 1900
   urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')
   # You can then use download.file to download into a directory.
   
   # If you only want to download the files you don't have, try:
   missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), TRUE))


   # download files
   for(i in seq_along(urls)) {
      zip_filename <- file.path(log_folder, file.name.from.url(urls[i]))
      tryCatch(download.file(urls[i], destfile=zip_filename, mode = 'wb'), error = function(e) e)
   }

   return(log_folder)
}
# unlink(list.files(tempdir()))

# http://www.r-bloggers.com/where-is-the-r-activity/
# source:  http://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/



#' @title Reads RStudio CRAN mirror data files from a folder
#' @export
#' @description 
#' This function reads files downloaded from the downlaod page (\url{http://cran-logs.rstudio.com/}).
#' 
#' This function relies on data.table to run faster.
#' WARNING: this function can be quite slow...
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param log_folder the folder which contains the RStudio CRAN log files that were downloaded to. Default is the temporary folder picked by \link{tempdir}.
#' @param use_data_table default is TRUE. A switch for wether or not to use thte data.table pacakge
#' in order to merge the log files using rbindlist. This function is MUCH faster then the alternative.
#' @param ... not in use.
#' @author Felix Schonbrodt, Tal Galili
#' @return Returns the combined data file.
#' @source \url{http://www.nicebread.de/finally-tracking-cran-packages-downloads/}
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' 
#' # barplots: (more functions can easily be added in the future)
#' barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
#' barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#' }
read_RStudio_CRAN_data <- function(log_folder = tempdir(), use_data_table = TRUE, ...) {
   file_list <- file.path(log_folder, list.files(log_folder))
   file_list <- file_list [ grep("[0-9]+-[0-9]+-[0-9]+.csv.gz", file_list)] # include only the relevant type of files, such as: "2013-04-02.csv.gz"  

   # removes empty files
   file_list_info <- file.info(file_list)
#    colnames(file_list_info)
   ss_non_0_files <- file_list_info$size > 0 
   file_list <- file_list[ss_non_0_files]      
   # this version is slower   
   
   # read files
   logs <- list()
   for (file in file_list) {
      print(paste("Reading", file, "..."))
      logs[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                                 dec = ".", fill = TRUE, stringsAsFactors = FALSE,
                                 comment.char = "", as.is=TRUE)
   }

   
   # rbind the files.
   if(use_data_table) is_data_table_loaded <- require2(data.table)
   if(use_data_table & is_data_table_loaded) {
      dataset <- rbindlist(logs) # MUCH faster...
   } else {
      dataset <- do.call("rbind",logs)
   }
   
   return(dataset)
}



#' @title Format the RStudio CRAN mirror data into the data.table format
#' @export
#' @description 
#' This function makes sure the the RStudio CRAN mirror data object has correct 
#' classes for the columns date, package, country. It also adds the columns weekday and week. Lastly, it also sets a key.
#' 
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param dataset the RStudio CRAN mirror data object
#' @param ... not in use.
#' @author Felix Schonbrodt, Tal Galili
#' @return Returns the re-formated data object.
#' @source \url{http://www.nicebread.de/finally-tracking-cran-packages-downloads/}
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' my_RStudio_CRAN_data <- format_RStudio_CRAN_data(my_RStudio_CRAN_data)
#' head(my_RStudio_CRAN_data)
#' lineplot_package_downloads(pkg_names = c("ggplot2", "reshape", "plyr", "installr"),
#'                            dataset = my_RStudio_CRAN_data)
#' 
#' # older plots:
#' # barplots: (more functions can easily be added in the future)
#' barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
#' barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#' }
format_RStudio_CRAN_data <- function(dataset, ...) {
   is_data_table_loaded <- require2(data.table)
   if(!is_data_table_loaded) stop("The 'data.table' package MUST be installed/loaded in order for this function to work.")
   
   if(("data.table" %in% class(dataset))) {
      dataset <- as.data.frame(dataset)
   }

   # add some keys and define variable types
   dataset <- within(dataset, {
      date=as.Date(date)
      package=factor(package)
      country=factor(country)
      weekday=weekdays(date)
      week=strftime(as.POSIXlt(date),format="%Y-%W")
   })
#    dataset[, date:=as.Date(dataset$date)]
#    dataset[, package:=factor(dataset$package)]
#    dataset[, country:=factor(dataset$country)]
#    dataset[, weekday:=weekdays(dataset$date)]
#    dataset[, week:=strftime(as.POSIXlt(dataset$date),format="%Y-%W")]
   
   dataset <- as.data.table(dataset)
   setkey(dataset, package, date, week, country)  
   
   return(dataset)
}



#' @title barplot for the number of users installation of a package
#' @export
#' @description 
#' This function is a first template for creating a barplot of the number of downloads a package had in a time period.
#' This function is based on some other functions, have a look at the example for more details.
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param pkg_name a string of the package we are interested in checking.
#' @param dataset a dataset output from running \link{read_RStudio_CRAN_data}.
#' @param remove_dups default is TRUE. Should the duplicate user ids (based on their ips) be removed.  If TRUE, then the plot is the number of unique users who have downloaded our package everyday.
#' @param ... not in use.
#' @return Returns the total number of downloads of the package for that time period.
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' my_RStudio_CRAN_data <- format_RStudio_CRAN_data(my_RStudio_CRAN_data)
#' head(my_RStudio_CRAN_data)
#' lineplot_package_downloads(pkg_names = c("ggplot2", "reshape", "plyr", "installr"),
#'                            dataset = my_RStudio_CRAN_data)
#' 
#' # older plots:
#' # barplots: (more functions can easily be added in the future)
#' barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
#' barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#' }
barplot_package_users_per_day <- function(pkg_name, dataset, remove_dups = TRUE, ...) {

   # subset our data only for our package:
   ss <- grepl(pkg_name, dataset$package) 
   pkg_dataset <- dataset[ss,]   
   if(remove_dups) pkg_dataset <- pkg_dataset[!duplicated(pkg_dataset$ip_id),]
   
   # number of installation per day
   installation_per_day <- aggregate(pkg_dataset$date , list(pkg_dataset$date), length)
   colnames(installation_per_day) <- c("date", "times")
   
   # barplot
      tmp_mar <- par()$mar
      par(mar = c(7.1, 4.1, 4.1, 2.1))
   with(installation_per_day, barplot(height=times, names.arg= date, las = 2, main = paste("Total installations for the {", pkg_name, "} package", sep = "" )))
      par(mar = tmp_mar)
   
   return(list(total_installations = sum(installation_per_day$times) )) # return the total number of installations
}





#' @title barplot for the number of users installation of a package
#' @export
#' @description 
#' This function gets a vector of package names, and returns a line plot of 
#' number of downloads for these packages per week.
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param pkg_names a character vector of packages we are interested in checking.
#' @param dataset a dataset output from running \link{read_RStudio_CRAN_data}, after going through \link{format_RStudio_CRAN_data}.
#' @param by_time by what time frame should packages be plotted? defaults to "date", but can also be "week"
#' @param ... not in use.
#' @author Felix Schonbrodt, Tal Galili
#' @return invisible aggregated data that was used for the plot
#' @source \url{http://www.nicebread.de/finally-tracking-cran-packages-downloads/}
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' my_RStudio_CRAN_data <- format_RStudio_CRAN_data(my_RStudio_CRAN_data)
#' head(my_RStudio_CRAN_data)
#' lineplot_package_downloads(pkg_names = c("ggplot2", "reshape", "plyr", "installr"),
#'                            dataset = my_RStudio_CRAN_data)
#' 
#' # older plots:
#' # barplots: (more functions can easily be added in the future)
#' barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
#' barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#' }
lineplot_package_downloads <- function(pkg_names, dataset, by_time = c("date", "week"), ...) {   
   require2("ggplot2")
   require2("plyr")
   
   by_time <- by_time[1]
   
   # plot 1: Compare downloads of selected packages on a weekly basis
#    agg1 <- dataset[J(pkg_names), length(unique(dataset$ip_id)), by=c(by_time, "package")]
   
   #-----
   # only added in order to avoid the "note" in the check before uploading to CRAN...
   V1 <- NA 
   package <- NA
   #-----
   
   agg1 <- ddply(dataset[dataset$"package" %in% pkg_names,], .(time= get(by_time), package), function(xx) {c(V1 = length(unique(xx$ip_id)))})
   
#    suppressWarnings(colnames(agg1)[1] <- "time")   
   o <- ggplot(agg1, aes(x=time, y=V1, color=package, group=package)) + geom_line() + ylab("Downloads") + theme_bw() + theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5))   
   print(o)
   
   return(invisible(agg1))
}



#' @title Most downloaded packages
#' @export
#' @description 
#' Gives the top "x" most downloaded packages.
#' @details
#' RStudio maintains its own CRAN mirror, \url{http://cran.rstudio.com} and offers its log files.
#' @param dataset a dataset output from running \link{read_RStudio_CRAN_data}, after going through \link{format_RStudio_CRAN_data}.
#' @param n the number of top packages to show.
#' @param ... not in use.
#' @return a table of top packages by downloads (a numeric vector with packages as names)
#' @source \url{http://www.nicebread.de/finally-tracking-cran-packages-downloads/}
#' @seealso \link{download_RStudio_CRAN_data}, \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
#' @examples
#' \dontrun{
#' # The first two functions might take a good deal of time to run (depending on the date range)
#' RStudio_CRAN_data_folder <- 
#'       download_RStudio_CRAN_data(START = '2013-04-02',
#'                                  END = '2013-04-05') 
#'                                  # around the time R 3.0.0 was released
#' my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
#' my_RStudio_CRAN_data <- format_RStudio_CRAN_data(my_RStudio_CRAN_data)
#' head(my_RStudio_CRAN_data)
#' most_downloaded_packages(my_RStudio_CRAN_data)
#' 
#' top_packages <- names(most_downloaded_packages(my_RStudio_CRAN_data))
#' lineplot_package_downloads(pkg_names = top_packages, dataset = my_RStudio_CRAN_data)
#' 
#' }
most_downloaded_packages <- function(dataset, n = 6L,...) {
   head(sort(table(dataset$package),decreasing=TRUE), n = n)   
}
