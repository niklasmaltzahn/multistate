\name{sim_fun}
\alias{sim_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{simulation function
%%  ~~function to do ... ~~
}
\description{sim_fun simulates interraival times in general multistate models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sim_fun(init_probs, jump_types, pars, cens_time, nbr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{init_probs}{Initial probabilities for each state.
%%     ~~Describe \code{x} here~~
}
  \item{jump_types}{A quadratic matrix with dimensions being the number of states in the multistate model. The indecies (i,j) in the matrix correspond to "from state i to state j". If there is no transition in (i,j) then the value NA should be given. If a transition is possible a character of type "exp", "wei", "gomp" or "cos" should be givin (see example).
%%     ~~Describe \code{x} here~~
}
  \item{pars}{A list of parameters specifying the interarival distributions.
%%     ~~Describe \code{x} here~~
}
  \item{cens_time}{Censoring times. Either a character determening the type of cencoring or a numeric constant as the upper bound of the time scale.
%%     ~~Describe \code{x} here~~
}
  \item{nbr}{Number of multistate observations to simulste.
%%     ~~Describe \code{x} here~~
}
  \item{hist_dep}{History dependence. Value should be True or False. Deafault is False. If set to True one can specify a dynamic update of the interarrival distribuions.
%%     ~~Describe \code{x} here~~
}
  \item{steps}{A maximum number of jumps pr multistate observation. To avoid conflict with cens_time the default is set to 10^3.
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


################ Example 2: Costumized hazard.
jump_types <- matrix(
  c(NA,"exp", "exp",
    "cos", NA, "exp",
    NA, NA, NA), 3, 3, byrow = T)
tmat <- tmat_function(jump_types)


# Exponential distrubtion parameters
lambda <- matrix(c(NA, 0.12, 0.03,
                   NA, NA, 0.1,
                   NA, NA, NA), 3, 3, byrow = T)

## costumizing hazard
# (time, value) grid
initial_grid1 <- cbind(c(0, 5, 16, 25, 60, 61, 100),
                       c(0.06, 0.06, 0.05, 0.04, 0.03, 0.03, 0.03))
# grid resolution for the spline approximation
grid_length1 <- 201
# spline method
method1 <- "hyman"

# Listing in which transition the costumized hazard should be used (i.e. transition 3)
ch_pred <- list(Fun = lapply(1:4, function(x) list()),
                t_max = 1000)
ch_pred$Fun[[3]] <- haz_costum(initial_grid1, grid_length1, method1, plot = T)

par_list2 <- list(lambda = lambda, ch_pred = ch_pred)


data_sim <- sim_fun(init_probs = c(1,0,0), jump_types, pars = par_list2,
                    cens_time = 1000, nbr = 1000)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
