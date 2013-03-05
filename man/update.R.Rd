\name{update.R}
\alias{update.R}
\title{Checks for the latest R version, and if there is a newer version of R - downloads and installs it.}
\usage{
  update.R(browse_news, install_R, copy_packages,
    keep_old_packages, update_packages, start_new_R,
    quit_R, print_R_versions = T)
}
\arguments{
  \item{browse_news}{if TRUE (and if there is a newer
  version of R) - it opens the browser to the NEWS of the
  latest version of R, for the user to read through}

  \item{install_R}{TRUE/FALSE - if to install a new version
  of R (if one is available).  If missing (this is the
  default) - the user be asked if to download R or not.Of
  course the installation part itself (the running of the
  .exe file) is dependent on the user.}

  \item{copy_packages}{TRUE/FALSE - if to copy your
  packages from the old version of R to the new version of
  R. If missing (this is the default) - the user will be
  asked for his preference (he should say yes, unless he is
  using a global library folder).}

  \item{update_packages}{TRUE/FALSE - if to update your
  packages in the new version of R (all packages will be
  updated without asking confirmation per package) If
  missing (this is the default) - the user will be asked
  for his preference (he should say yes, unless he is using
  a global library folder).  This is done by calling the
  Rscript in the new R.}

  \item{keep_old_packages}{- if the keep the packages in
  the library of the old R installation. If missing (this
  is the default) - the user will be asked for his
  preference (he should say yes, unless he is using a
  global library folder).}

  \item{start_new_R}{TRUE/FALSE - if to start the new R
  (Rgui) after we will quit the old R. Default is TRUE. It
  will try to start the 64 bit R version, if it does not
  exist, the 32 bit will be started. This may be less
  useful for people using RStudio or the likes.}

  \item{quit_R}{TRUE/FALSE - if to quite R after the
  installation and package copying or not. If missing (this
  is the default) - the user is asked what to do.}

  \item{print_R_versions}{if to tell the user what version
  he has and what is the latest version (default is TRUE)}
}
\value{
  a TRUE/FALSE value on whether or not R was updated.
}
\description{
  This function performs the following steps: \itemize{
  \item Check what is the latest R version.  If the current
  installed R version is up-to-date, the function ends (and
  returns FALSE) \item If a newer version of R is
  available, the user is asked if to review the NEWS of the
  latest R version - in order to decide if to install the
  newest R or not. \item If the user wishes to - the
  function will download and install it. (you will need to
  press the "next" buttons on your own) \item Once the
  installation is done, you should press "any-key", and the
  function will proceed with copying all of your packages
  from your old (well, current) R installation, into your
  newer R installation. \item You can then erase all of the
  packages in your old R installation. \item After your
  packages are moved (and the old ones possibly erased),
  you will get the option to update all of your packages in
  the new version of R. \item Lastely - you can close the
  current session of your old R. }
}
\examples{
\dontrun{
update.R(T, T, T, T, T, T, T) # the safest upgrade option: See the NEWS, install R, copy packages, keep old packages, update packages in the new installation, start the Rgui of the new R, and quite current session of R
update.R() # will ask you what you want at every decision.
}
}
\seealso{
  \link{check.for.updates.R}, \link{install.R},
  \link{copy.packages.between.libraries},
}

