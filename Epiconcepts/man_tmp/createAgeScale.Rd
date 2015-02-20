\name{createAgeScale}
\alias{createAgeScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createAgeScale(x, lower = 0, upper = 100, by = 10, sep = "-", above.char = "+")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lower}{
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{above.char}{
%%     ~~Describe \code{above.char} here~~
}
}
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
function (x, lower = 0, upper = 100, by = 10, sep = "-", above.char = "+") 
{
    if (by == 1) {
        labs <- c(seq(lower, upper, by = by))
    }
    else {
        labs <- c(paste(seq(lower, upper - by, by = by), seq(lower + 
            by - 1, upper - 1, by = by), sep = sep), paste(upper, 
            above.char, sep = ""))
    }
    cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf), 
        right = FALSE, labels = labs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
