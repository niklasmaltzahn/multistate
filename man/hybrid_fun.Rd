\name{hybrid_fun}
\alias{hybrid_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hybrid landmark function
%%  ~~function to do ... ~~
}
\description{Function extracting a subset of a multistate data frame based on a landmark time and state. However, only information in non markov transitions are as in the landmark sample. The rest of the transitions contains same information as in the original multistate data frame.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hybrid_fun(data, s, l, non_markov_trans)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Multistate data frame of type from sim_fun.
%%     ~~Describe \code{x} here~~
}
  \item{s}{Landmark time.
%%     ~~Describe \code{x} here~~
}
  \item{l}{Landmark state.
%%     ~~Describe \code{x} here~~
}
  \item{non_markov_trans}{Transitions for which only landmark data should be kept.
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
