\name{install.pandoc}
\alias{install.pandoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
install.pandoc()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    if (require(XML)) {
        page <- readLines("http://code.google.com/p/pandoc/downloads/list", 
            warn = FALSE)
        pagetree <- htmlTreeParse(page, error = function(...) {
        }, useInternalNodes = TRUE, encoding = "UTF-8")
        URL <- xpathSApply(pagetree, "//tr[2]//td[1]//a ", xmlAttrs)[1]
        URL <- paste("http", URL, sep = ":")
    }
    else {
        page <- readLines("http://code.google.com/p/pandoc/downloads/list", 
            warn = FALSE)
        pat <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+-setup.exe"
        target.line <- grep(pat, page, value = TRUE)
        m <- regexpr(pat, target.line)
        URL <- regmatches(target.line, m)
        URL <- paste("http", URL, sep = ":")
    }
    install.URL(URL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
