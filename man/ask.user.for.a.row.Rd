\name{ask.user.for.a.row}
\alias{ask.user.for.a.row}
\title{Asks the user for a row number from a data.frame table}
\source{
  On how to ask the user for input:
  \link{http://stackoverflow.com/questions/5974967/what-is-the-correct-way-to-ask-for-user-input-in-an-r-program}
}
\usage{
  ask.user.for.a.row(TABLE,
    header_text = "Possible versions to download (choose one)",
    questions_text = "Please review the table of versions from above, \n  and enter the row number of the file-version you'd like to install: ")
}
\arguments{
  \item{TABLE}{a data.frame table with rows from which we
  wish the user to choose a row.  If TABLE is not a
  data.frame, it will be coerced into one.}

  \item{header_text}{the text the users sees (often a
  question) as a title for the printed table - explaining
  which row he should choose from}

  \item{questions_text}{the question the users see after
  the printing of the table - explaining which row he
  should choose from}
}
\value{
  The row number the user has choosen from the data.frame
  table.
}
\description{
  The function gets a data.frame and asks the user to
  choose a row number.  Once choosen, that row number is
  returned from the function.
}
\details{
  This function is used in \code{installr} when we are not
  sure what version of the software to download, or when
  various actions are available for the user to choose
  from. If the user doesn't give a valid row number, the
  function repeats its questions until a valid row number
  is chosen (or the user escapes)
}
\examples{
version_table <- data.frame(versions = c("devel", "V 1.0.0", "V 2.0.0"))
installr:::ask.user.for.a.row(version_table)
}

