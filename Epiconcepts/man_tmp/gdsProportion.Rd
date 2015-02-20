\name{gdsProportion}
\alias{gdsProportion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gdsProportion(c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c}{
%%     ~~Describe \code{c} here~~
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
function (c) 
{
    set.seed(123)
    x <- GDS[, c]
    N_ = nrow(GDS)
    myprop <- function(x, i) {
        sum(x[i] == 0)/length(x)
    }
    B <- boot(x, myprop, 100)
    SD_ = sprintf("\%5.6f", sd(B$t))
    T = table(x)
    NAMES_ = names(T)
    M1_ = T[1]
    M2_ = T[2]
    NDF = c(c, "Obs.", "Proportion", "Std. Err.", "[95\% Conf. Interval]")
    P = prop.test(M1_, N_, conf = 0.95)
    PE1_ = sprintf("\%5.6f", P$estimate)
    CI1_ = sprintf("\%f \%f", P$conf.int[1], P$conf.int[2])
    P = prop.test(M2_, N_, conf = 0.95)
    PE2_ = sprintf("\%5.6f", P$estimate)
    CI2_ = sprintf("\%f \%f", P$conf.int[1], P$conf.int[2])
    R1 = c(NAMES_[1], M1_, PE1_, SD_, CI1_)
    R2 = c(NAMES_[2], M2_, PE2_, SD_, CI2_)
    DF = data.frame(rbind(`1` = R1, `2` = R2))
    names(DF) <- NDF
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
