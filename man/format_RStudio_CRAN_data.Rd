\name{format_RStudio_CRAN_data}
\alias{format_RStudio_CRAN_data}
\title{Format the RStudio CRAN mirror data into the data.table format}
\source{
  \url{http://www.nicebread.de/finally-tracking-cran-packages-downloads/}
}
\usage{
  format_RStudio_CRAN_data(dataset, ...)
}
\arguments{
  \item{dataset}{the RStudio CRAN mirror data object}

  \item{...}{not in use.}
}
\value{
  Returns the re-formated data object.
}
\description{
  This function makes sure the the RStudio CRAN mirror data
  object has correct classes for the columns date, package,
  country. It also adds the columns weekday and week.
  Lastly, it also sets a key.
}
\details{
  RStudio maintains its own CRAN mirror,
  \url{http://cran.rstudio.com} and offers its log files.
}
\examples{
\dontrun{
# The first two functions might take a good deal of time to run (depending on the date range)
RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(START = '2013-04-02', END = '2013-04-05') # around the time R 3.0.0 was released
my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
my_RStudio_CRAN_data <- format_RStudio_CRAN_data(my_RStudio_CRAN_data)
head(my_RStudio_CRAN_data)
lineplot_package_downloads(pkg_names = c("ggplot2", "reshape", "plyr", "installr"), dataset = my_RStudio_CRAN_data)

# older plots:
# barplots: (more functions can easily be added in the future)
barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
}
}
\author{
  Felix Schonbrodt, Tal Galili
}
\seealso{
  \link{download_RStudio_CRAN_data},
  \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
}

