\name{CC_AR}
\alias{CC_AR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cases-Control studies : Attribuable Fraction (estimate)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CC_AR(C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{
%%     ~~Describe \code{C} here~~
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
function (C) 
{
    T = epi.2by2(dat = C, method = "case.control")
    S <- summary(T)
    return(c(S$AFest[1], S$AFest[2], S$AFest[3]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
