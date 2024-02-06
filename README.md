[![Project Status: Unsupported – The project has reached a stable, usable state but the author(s) have ceased all work on it. A new maintainer may be desired.](https://www.repostatus.org/badges/latest/unsupported.svg)](https://www.repostatus.org/#unsupported)

:exclamation: **This repository is unsupported** (see https://github.com/talgalili/installr/issues/183), a more broadly applicable alternative is Winget/WingetUI (https://www.marticliment.com/wingetui/) (see the discussion in https://github.com/talgalili/installr/issues/183#issuecomment-1725363286).

---

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/installr)](https://cran.r-project.org/package=installr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/talgalili/installr?branch=master&svg=true)](https://ci.appveyor.com/project/talgalili/installr)
![](http://cranlogs.r-pkg.org/badges/installr?color=yellow)
![](http://cranlogs.r-pkg.org/badges/grand-total/installr?color=yellowgreen)
<!-- badges: end -->

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
if (!require('remotes')) install.packages('remotes'); # make sure you have Rtools installed first! if not, then run:
#install.packages('installr')
#install.Rtools()
remotes::install_github('talgalili/installr')
```

## Usage

If you are using the Rgui, you will see a new menu added on your top right (just by "help"), giving you the option to update R, or install new software.

For command line use you can **update R** by running:

```r
if(!require("installr")) install.packages('installr')
library("installr")
updateR() # this will open dialog boxes to take you through the steps.
# OR use:
# updateR(TRUE) # this will use common defaults and will be the safest/fastest option
```

Or **install a new software** simply by running:

```r
library("installr")
installr() #  user can easily select (via a GUI interface) a software to install.
```


## Troubleshooting

If you get either of the following errors from some commands:

```r
Error in download.file(URL, destfile = ...) : 
  unsupported URL scheme
```

Or:

```r
install.RStudio()
Error in file(con, "r") : cannot open the connection
updateR()
Error in file(con, "r") : cannot open the connection
```


Try running:

```r
setInternet2(TRUE)
```

Sources: 
* https://stackoverflow.com/questions/21857264/error-in-download-file-unsupported-url-scheme
* https://stackoverflow.com/questions/19890633/r-produces-unsupported-url-scheme-error-when-getting-data-from-https-sites


## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/talgalili/installr/issues>
* send a pull request on: <https://github.com/talgalili/installr/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Available functions are:

You can see the most recent changes to the package in the NEWS.md file:

http://talgalili.github.io/installr/news/index.html


# Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/talgalili/installr/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

