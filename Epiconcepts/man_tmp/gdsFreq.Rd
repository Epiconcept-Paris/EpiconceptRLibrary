\name{gdsFreq}
\alias{gdsFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gdsFreq(x, by = NULL, where = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{where}{
%%     ~~Describe \code{where} here~~
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
function (x, by = NULL, where = NULL) 
{
    if (is.character(by)) {
        if (is.vector(where)) {
            R = table(GDS[where, x], GDS[where, by])
            return(R)
            names(R) <- c(x, by, "Freq")
            return(R)
        }
        R = table(VAL(x), VAL(by))
        return(R)
    }
    if (is.vector(where)) {
        R = data.frame(table(GDS[where, x]))
        names(R) <- c(x, "Freq")
        return(R)
    }
    R = data.frame(table(GDS[, x]))
    names(R) <- c(x, "Freq")
    return(R)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
