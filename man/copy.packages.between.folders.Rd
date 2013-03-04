\name{copy.packages.between.folders}
\alias{copy.packages.between.folders}
\title{Copies all packages from one folder to another}
\usage{
  copy.packages.between.folders(keep_old = T, ask = F)
}
\arguments{
  \item{ask}{should the user be given the option to choose
  between which two libraries to copy the packages?  If
  FALSE (default), the folders are copied from the
  before-newest R installation to the newest R
  installation.}

  \item{keep_old}{should the packages be copied to the new
  library folder, thus KEEPing the old package as they are?
  Or should they be removed?}
}
\value{
  TRUE/FALSE if we created a new folder or not.
}
\description{
  Copies all packages from one folder to another.  This
  function is used if we wish to either: \itemize{ \item
  Upgrade R to a new version - and copy all of the packages
  from the old R installation to the new one. \item Move to
  a global library system - and wanting to copy all of
  packages from the local library folder to the global one
  } It takes into account that we don't want to copy
  packages which have "high" importance (such as MASS,
  rpart, Matrix and more...) to the new library folder.
}
\examples{
copy.packages.between.folders()
}

