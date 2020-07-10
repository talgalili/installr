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





#' Using R to Install Stuff (Such As: R, Rtools, RStudio, Git, and More!)
#' 
#' R is great for installing software.  Through the 'installr'
#' package you can automate the updating of R (on Windows, using updateR())
#' and install new software. Software installation is initiated through a
#' GUI (just run installr()), or through functions such as: install.Rtools(),
#' install.pandoc(), install.git(), and many more. The updateR() command
#' performs the following: finding the latest R version, downloading it,
#' running the installer, deleting the installation file, copy and updating
#' old packages to the new R installation.
#' 
#' @name installr-package
#' @docType package
#' @keywords installr
#' @import stats
#' @import utils
#' @import graphics
NULL
