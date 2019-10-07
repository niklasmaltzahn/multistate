\name{add_cov}
\alias{add_cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adding covariate to multistate data
%%  ~~function to do ... ~~
}
\description{The function merges a multistate dataset and a dataset of a time dependent covariate.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_cov(data, cov_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Multistate date of the type from sim_fun.
%%     ~~Describe \code{x} here~~
}
  \item{cov_data}{covariate data with column one with columns (id, time, value).
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
jump_types <- matrix(c(NA, "exp", "exp", NA, "exp",
                       "exp", NA ,NA,"exp", "exp",
                       "exp", NA, NA, "exp", "exp",
                       NA, "exp", "exp", NA, "exp",
                       rep(NA, times = 5)), 5, 5, byrow = T)
tmat <- tmat_function(jump_types)

lambda <- matrix(c(NA, 0.1, 0.15, NA, 0.08,
                   0.12, NA ,NA,0.15, 0.05,
                   0.1, NA, NA, 0.1, 0.08,
                   NA, 0.12, 0.1, NA, 0.05,
                   rep(NA, times = 5)), 5, 5, byrow = T)

par_list2 <- list(lambda = lambda)

data_sim <- sim_fun(init_probs = c(0.5,0,0.5,rep(0, times = 2)), jump_types, pars = par_list2,
                   cens_time = 1000, nbr = 1000)


new_states <- list(state1 = 1:4, state2 = 5)
    new_tmat <- matrix(NA, 2, 2)
    new_tmat[1,2] <- 1

data_margial <- merge_states(data_sim, new_states, new_tmat)

A_data <- cbind(data_sim[,c("id", "Tstop")], as.numeric(data_sim$from \%in\% c(2, 4)))
names(A_data) <- c("id", "Tstop", "A")

data_margial <- add_cov(data_margial, cov_data = A_data)
data_margial$L <- as.numeric(data_margial$from \%in\% c(3, 4))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
