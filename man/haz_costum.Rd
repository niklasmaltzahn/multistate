\name{haz_costum}
\alias{haz_costum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{costumized intensity
%%  ~~function to do ... ~~
}
\description{This function can build constomized intensity functions based on splies
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
haz_costum(initial_grid, grid_length, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial_grid}{A two column matrix of time points and hazard values.
%%     ~~Describe \code{x} here~~
}
  \item{grid_length}{Grid resolution in spline approximation.
%%     ~~Describe \code{x} here~~
}
  \item{method}{Spline method.
%%     ~~Describe \code{x} here~~
}
  \item{plot}{If True a plot of the specified hazard is generated. Default is False.
%%     ~~Describe \code{x} here~~
}
  \item{type}{Character specifying to output hazard function "haz" or cumulative hazard function "cumhaz".
%%     ~~Describe \code{x} here~~
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
