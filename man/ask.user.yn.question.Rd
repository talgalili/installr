\name{ask.user.yn.question}
\alias{ask.user.yn.question}
\title{Asks the user for one yes/no question.}
\usage{
  ask.user.yn.question(question, yn_text = "(y/n):",
    add_lines_before = T)
}
\arguments{
  \item{question}{a character string with a question to the
  user.}

  \item{yn_text}{the y/n text after the question.}

  \item{add_lines_before}{if to add a line before asking
  the question.  Default is TRUE.}
}
\value{
  TRUE/FALSE - if the user answeres yes or no.
}
\description{
  Asks the user for one yes/no question.  If the users
  replies with a "yes" (or Y, or y) the function returns
  TRUE.  Otherwise, FALSE. (also exists as the function
  devtools::yesno)
}
\examples{
\dontrun{
ask.user.yn.question("Do you love R?")
}
}

