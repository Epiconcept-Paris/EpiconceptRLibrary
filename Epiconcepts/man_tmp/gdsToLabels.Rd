\name{gdsToLabels}
\alias{gdsToLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gdsToLabels(x, to = "", labels = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
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
function (x, to = "", labels = list()) 
{
    if (is.list(x)) {
        for (i in x) {
            gdsToLabels(i, labels = labels)
        }
        return(TRUE)
    }
    if (to != "") {
        CN = names(GDS)
        START_ = match(x, CN)
        END_ = match(to, CN)
        for (i in START_:END_) {
            gdsToLabels(CN[i], labels = labels)
        }
        return(TRUE)
    }
    GDS[, x] <<- as.factor(GDS[, x])
    GDS[, x] <<- revalue(GDS[, x], labels)
    return(TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
