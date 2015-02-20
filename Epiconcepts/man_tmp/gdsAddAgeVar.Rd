\name{gdsAddAgeVar}
\alias{gdsAddAgeVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gdsAddAgeVar(varname, birthdate, onsetdate, unit = "year")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varname}{
%%     ~~Describe \code{varname} here~~
}
  \item{birthdate}{
%%     ~~Describe \code{birthdate} here~~
}
  \item{onsetdate}{
%%     ~~Describe \code{onsetdate} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
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
function (varname, birthdate, onsetdate, unit = "year") 
{
    X <- as.Date(GDS[, birthdate])
    if (onsetdate \%in\% colnames(GDS)) {
        Y = as.Date(GDS[, onsetdate])
    }
    else {
        Y = as.Date(onsetdate)
    }
    X_AGE <- .getAge(X, Y, unit)
    GDS <<- cbind(GDS, X_AGE)
    gdsRenameCol("X_AGE", varname)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
