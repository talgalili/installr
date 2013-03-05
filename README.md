# installr

The *installr* package offers a set of R functions for the installation and updating of software (currently, only on Windows OS), with a special focus on R itself. This package has two main goals:

1. To make updating R (on windows) as easy as running a function.
2. To make it easy to install all of the needed software for R development (such as git, and RTools), and reproducible research using R (such as MikTeX and pandoc), as easy as possible.


The available functions are:

* updateR() - serves as a "check for updates" function of R itself.  Running the function will check for a newer version of R than the one currently used.  If one exists, the function will download the most updated R version and run its installer.  Once done, the function will offer to copy (or move) all of the packages from the old R library to the new R library. It will then offer to update the moved packages, offer to open the new Rgui, and lastely, it will quit the old R.

* install.RStudio() - download and runs the installer for RStudio.
* install.Rtools() - download and runs the installer for Rtools (allowing the user to choose which version to download)	
* install.pandoc() - download and runs the installer for pandoc.
* install.MikTeX() - download and runs the installer for MikTeX.
* install.git() - download and runs the installer for git-gui.
* install.packages.zip() - for installing package from a url of a ZIP file.  Currently, it is the only option I know of for downloading+installing a direct ZIP of an R package.
* install.URL() - gets a URL of a zipped file, and makes sure to download and run it.

You are welcome to submit suggestions and bug-reports at: https://github.com/talgalili/installr/issues
I also welcome patches on: https://github.com/talgalili/installr/
And any friendly e-mail to: tal.galili@gmail.com


