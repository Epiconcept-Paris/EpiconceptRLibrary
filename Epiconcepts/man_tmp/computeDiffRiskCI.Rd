\name{computeDiffRiskCI}
\alias{computeDiffRiskCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
computeDiffRiskCI(RE, RU, NE, NU)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RE}{
%%     ~~Describe \code{RE} here~~
}
  \item{RU}{
%%     ~~Describe \code{RU} here~~
}
  \item{NE}{
%%     ~~Describe \code{NE} here~~
}
  \item{NU}{
%%     ~~Describe \code{NU} here~~
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
function (RE, RU, NE, NU) 
{
    A = RE - RU
    B = (RE * (1 - RE))/NE
    C = (RU * (1 - RU))/NU
    D = 1.96 * sqrt(B + C)
    R1 = A + D
    R2 = A - D
    return(c(R2, R1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
