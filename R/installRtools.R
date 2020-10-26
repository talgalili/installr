get_compatible_rtools_version <- function(r_version = getRversion()) {
  require2("pkgbuild", min_version = "1.1.0")
  rtools_version_string <- pkgbuild::rtools_needed(r_version)
  if (!stringr::str_detect(rtools_version_string, "[\\d.]+")) {
    stop(
      "Can't determine compatible Rtools version, please report this at",
      "\nhttps://github.com/talgalili/installr/issues"
    )
  }
  str_extract(rtools_version_string, "[\\d.]+")
}

get_rtools_url <- function(rtools_version = get_compatible_rtools_version(),
                           arch = R.version$arch) {
  base_url <- "https://cran.r-project.org/bin/windows/Rtools/"
  rtools_version_nodots <- str_replace(rtools_version, "\\.", "")
  if (rtools_version < 4.0) {
    filename <- str_glue("Rtools{rtools_version_nodots}.exe")
  } else {
    filename <- str_glue("rtools{rtools_version_nodots}-{arch}.exe")
  }
  url <- str_glue("{base_url}{filename}")
  url
}


#' @title Downloads and installs Rtools
#' @aliases install.rtools
#' @description Install compatible version of Rtools for Windows.
#' By default, the function searches if a compatible Rtools is installed,
#' if not, it offers to install the latest compatible version.
#' @details
#' Rtools is a collection of software for building packages for R under Microsoft Windows,
#' or for building R itself (version 1.9.0 or later).
#' The original collection was put together by Prof. Brian Ripley;
#' it is currently being maintained by Duncan Murdoch.
#' @param check checks if we need to install Rtools or not.
#' @param check_r_update checks if there is an R update available (ignores patch versions),
#' if so, asks if user wants to install the R update first. (defaults to TRUE)
#' @param GUI Should a GUI be used when asking the user questions? (defaults to TRUE)
#' @param ... extra parameters to pass to \link{install.URL}
#' @return invisible(TRUE/FALSE) - was the installation successful or not.
#' @export
#' @references Rtools homepage: \url{https://cran.r-project.org/bin/windows/Rtools/}
#' @examples
#' \dontrun{
#' # installs the latest compatible version of Rtools if a compatible version is not yet installed
#' install.Rtools()
#' # (re)installs the latest compatible version of Rtools
#' install.Rtools(check = F)
#'
#' # skip R version check
#' install.Rtools(checkRupdate = F)
#' }
install.Rtools <- function(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...) {
  if (check_r_update && r_update_available(ignore_patchlevel = T)) {
    update_r <- ask.user.yn.question(
      "A newer R version is available, do you want to update R first?", GUI = GUI)
    if (update_r) {
      updateR(GUI = GUI, print_R_versions = F, install_R = T, start_new_R = T, quit_R = F)
      print("Please run installr::install.Rtools using the newly installed R")
      return(invisible(FALSE))
    }
  }

  # Use pkgbuild to check if matching rtools is installed
  if (check) {
    require2("pkgbuild")
    found_rtools <- pkgbuild::has_rtools()
    if (found_rtools) {
      cat("No need to install Rtools - You've got the relevant version of Rtools installed\n")
      return(invisible(FALSE))
    }
  }

  # if we reached here - it means we'll need to install Rtools.
  tryCatch({
      rtools_url <- get_rtools_url()
      install.URL(rtools_url, ...)
    },
    error = function(e) {
      message("You'll need to go to the site and download this yourself.",
              " I'm now going to try and open the url for you.")
      browseURL("https://cran.r-project.org/bin/windows/Rtools/")
      stop(e)
    }
  )
}


#' @export
install.rtools <- function(...) install.Rtools(...)
