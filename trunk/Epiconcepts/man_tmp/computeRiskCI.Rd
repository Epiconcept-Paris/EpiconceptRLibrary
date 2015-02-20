\name{computeRiskCI}
\alias{computeRiskCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
computeRiskCI(risk, X1, N1, X2, N2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{risk}{
%%     ~~Describe \code{risk} here~~
}
  \item{X1}{
%%     ~~Describe \code{X1} here~~
}
  \item{N1}{
%%     ~~Describe \code{N1} here~~
}
  \item{X2}{
%%     ~~Describe \code{X2} here~~
}
  \item{N2}{
%%     ~~Describe \code{N2} here~~
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
function (risk, X1, N1, X2, N2) 
{
    A = ((N1 - X1)/X1)/N1
    B = ((N2 - X2)/X2)/N2
    R1 = log(risk) + (1.96 * sqrt(A + B))
    R2 = log(risk) - (1.96 * sqrt(A + B))
    E1 = exp(R1)
    E2 = exp(R2)
    return(c(E2, E1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
