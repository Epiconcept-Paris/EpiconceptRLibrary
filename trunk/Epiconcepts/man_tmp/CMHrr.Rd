\name{CMHrr}
\alias{CMHrr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CMHrr(A, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function (A, B) 
{
    Ce1 = A[2, 2]
    Cu1 = A[1, 2]
    He1 = A[2, 1]
    Hu1 = A[1, 1]
    Te1 = Ce1 + He1
    Tu1 = Cu1 + Hu1
    T1 = Te1 + Tu1
    H1 = Hu1 + He1
    C1 = Cu1 + Ce1
    Ce2 = B[2, 2]
    Cu2 = B[1, 2]
    He2 = B[2, 1]
    Hu2 = B[1, 1]
    Te2 = Ce2 + He2
    Tu2 = Cu2 + Hu2
    T2 = Te2 + Tu2
    H2 = Hu2 + He2
    C2 = Cu2 + Ce2
    R1 = ((Ce1 * Tu1)/T1) + ((Ce2 * Tu2)/T2)
    R2 = ((Cu1 * Te1)/T1) + ((Cu2 * Te2)/T2)
    rrmh = R1/R2
    R3 = ((C1 * Te1 * Tu1) - (Ce1 * Cu1 * T1))/T1^2
    R4 = ((C2 * Te2 * Tu2) - (Ce2 * Cu2 * T2))/T2^2
    print(sprintf("R3: \%3.3f - R4: \%3.3f", R3, R4))
    R5 = R3 + R4
    print(sprintf("R5: \%3.3f", R5))
    R6 = R5/(R1 * R2)
    print(sprintf("R6: \%3.3f", R6))
    R7 = sqrt(R6)
    print(sprintf("R7: \%3.3f", R7))
    L = log(rrmh) - (1.96 * R7)
    H = log(rrmh) + (1.96 * R7)
    CIL = exp(L)
    CIH = exp(H)
    return(c(rrmh, CIL, CIH))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
