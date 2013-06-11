\name{read_RStudio_CRAN_data}
\alias{read_RStudio_CRAN_data}
\title{Reads RStudio CRAN mirror data files from a folder}
\usage{
  read_RStudio_CRAN_data(log_folder = tempdir(), ...)
}
\arguments{
  \item{log_folder}{the folder which contains the RStudio
  CRAN log files that were downloaded to. Default is the
  temporary folder picked by \link{tempdir}.}

  \item{...}{not in use.}
}
\value{
  Returns the combined data file.
}
\description{
  This function reads files downloaded from the downlaod
  page (\url{http://cran-logs.rstudio.com/}).

  WARNING: this function can be quite slow...
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

# barplots: (more functions can easily be added in the future)
barplot_package_users_per_day("installr", my_RStudio_CRAN_data)
barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
}
}
\seealso{
  \link{download_RStudio_CRAN_data},
  \link{read_RStudio_CRAN_data},\link{barplot_package_users_per_day}
}

