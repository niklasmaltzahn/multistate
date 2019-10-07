\name{wbMarkov}
\alias{wbMarkov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Markov test function
%%  ~~function to do ... ~~
}
\description{Produces the grid and the point test of the Markov assumption.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wbMarkov(time_grid, data, grp1, grp2, trans, n_boot, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time_grid}{landmark time grid.
%%     ~~Describe \code{x} here~~
}
  \item{data}{multi state date to be tested.
%%     ~~Describe \code{x} here~~
}
  \item{grp1}{landmark group 1.
%%     ~~Describe \code{x} here~~
}
  \item{grp2}{landmark group 2.
%%     ~~Describe \code{x} here~~
}
  \item{trans}{transitions to be tested.
%%     ~~Describe \code{x} here~~
}
  \item{n_boot}{number of bootsrap samples of test statistics under the null hypothesis of being Markov.
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
################ Example 1: Dynamic intensity update
## Matrix of determaning interarrval distribution for each transition
jump_types <- matrix(
  c(NA,"exp", "exp",
    "exp", NA, "exp",
    NA, NA, NA), 3, 3, byrow = T)
tmat <- tmat_function(jump_types)

## Form parameter list acording to tmat numbers
lambda <- matrix(c(NA, 0.12, 0.03,
                   0.15, NA, 0.1,
                   NA, NA, NA), 3, 3, byrow = T)

## list of parameters
par_list2 <- list(lambda = lambda)



## update function to dynamicly update interarrival distributions
lambda_update <- function(lambda, vsts, state, jump_time, i, id){

  # individual frailty at transition 4
  if(state[i] == 2){
    lambda[2,1] <-  0.15 * frail[id]
  }

  lambda
}


frail <- rep_rgamma(n = 1000, var = 0.4)
data_sim <- sim_fun(init_probs = c(1,0,0), jump_types, pars = par_list2,
                    cens_time = 1000, nbr = 1000, hist_dep = T)


wb <- wbMarkov(time_grid = s_grid, data = df1$long_data, grp1 = 1,
               grp2 = 2, trans = c(1,2,3,4), n_boot = 100)


wb_frames <- frames_wbMarkov(wb)

## plot of bootsraded distriubtions (global then local)
plot_wbMarkov(wb)
plot_wbMarkov(wb, type = "point", time_pt = 6)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
