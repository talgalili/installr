# installr

## Introduction

The *installr* package offers a set of R functions for the installation and updating of software (currently, only on Windows OS), with a special focus on R itself. This package has two main goals:

1. To make updating R (on windows) as easy as running a function.
2. To make it as easy as possible to install all of the needed software for R development (such as git, RTools, etc), as well as for reproducible research using R (such as MikTeX, pandoc, etc).

## Motivation


While for Linux users, the installation process of new software may be just running a short line of code, for the regular Windows user it often includes browsing online, finding the latest version, downloading it, running the installer, and deleting the installation file. All of these steps are automatically done using functions in this package.

## Installation

To install the stable version on CRAN:

```r
install.packages('installr')
```

To install the latest installr version from GitHub use:

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr')
#install_Rtools()
install_github('installr', 'talgalili')
```

## Usage

If you are using the Rgui, you will see a new menu added on your top right (just by "help"), giving you the option to update R, or install new software.

For command line use you can **update R** by running:

```r
require(installr)
updateR() # this will open dialog boxes to take you through the steps.
```

Or **install a new software** simply by running:

```r
require(installr)
installr() #  user can easily select (via a GUI interface) a software to install.
```


## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/talgalili/installr/issues>
* send a pull request on: <https://github.com/talgalili/installr/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Available functions are:

* R install/update/uninstall related functions:
   * updateR() - serves as a "check for updates" function of R itself.  Running the function will check for a newer version of R than the one currently used.  If one exists, the function will download the most updated R version and run its installer.  Once done, the function will offer to copy (or move) all of the packages from the old R library to the new R library. It will then offer to update the moved packages, offer to open the new Rgui, and lastely, it will quit the old R.
   * uninstall.R
   * require2 - just like "require", only makes sure to download and install the package in case it is not present on the system (useful for examples...)
   * restart_RGui - a function to restart Rgui from Rgui

* Installing software from withing R:
   * install.RStudio() - download and runs the installer for RStudio.
   * install.Rtools() - download and runs the installer for Rtools (allowing the user to choose which version to download)	
   * install.pandoc() - download and runs the installer for pandoc.
   * install.MikTeX() - download and runs the installer for MikTeX.
   * install.git() - download and runs the installer for git-gui.
   * install.packages.zip() - for installing package from a url of a ZIP file.  Currently, it is the only option I know of for downloading+installing a direct ZIP of an R package.
   * install.URL() - gets a URL of a zipped file, and makes sure to download and run it.
   * install.LyX()
   * install.LaTeX2RTF()
   * install.GitHub()
   * install.ImageMagick()
   * install.GraphicsMagick()
   * install.SWFTools()
   * install.FFmpeg()
   * install.7zip()
   * install.notepadpp()
   * install.npptor()
   * install.Cygwin()

* Operation system managing functions
   * manage.os() - for shutdown/sleep of a Windows computer (useful for running at the end of a simulation).  Controls all the following functions.
   * os.hibernate()
   * os.lock()
   * os.restart()
   * os.shutdown()
   * os.sleep()
   * is.Rgui()
   * is.RStudio()
   * is.windows()

* CRAN and network related functions:
   * cranometer - Estimates the speed of each CRAN mirror by measuring the time it takes to download the NEWS file.
   * myip - return your ip address.
   * freegeoip - Geolocate IP addresses in R (contributed by Heuristic Andrew)
   * download_RStudio_CRAN_data
   * read_RStudio_CRAN_data
   * barplot_package_users_per_day
   * lineplot_package_downloads
   * format_RStudio_CRAN_data
   * most_downloaded_packages

* Misc functions:
   * is.empty - function added for checking if an object is empty (e.g: of zero length)

