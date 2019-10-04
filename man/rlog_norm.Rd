\name{rlog_norm}
\alias{rlog_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Log-normal (correlated) frailties
%%  ~~function to do ... ~~
}
\description{A simulation function simulating log normal frialties in dimension n >= 1. In particular one can simulate correlated frailties.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rlog_norm(n, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of frailties to be simulated.
%%     ~~Describe \code{x} here~~
}
  \item{sigma}{covariance matrix.
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

############## Simulating correlated frailties

jump_types <- matrix(
  c(NA,"exp", "exp",
    "exp", NA, "exp",
    NA, NA, NA), 3, 3, byrow = T)

tmat <- tmat_function(jump_types)

### Form parameter list acording to tmat numbers
lambda <- matrix(c(NA, 0.12, 0.03,
                   0.15, NA, 0.1,
                   NA, NA, NA), 3, 3, byrow = T)


# frailty distribution
Sigma <- matrix(c(0,0,0,0,
                  0,0,0,0,
                  0,0,0,0,
                  0,0,0,0), 4, 4, byrow = T)

# Z_12, Z_13, Z_21, Z_23
diag(Sigma) <- log(c(1.0, 0.3, 1.5, 0.4) + 1)
Sigma[1,2] <- Sigma[2,1] <- log(1 + 1)
Sigma[1,3] <- Sigma[3,1] <- log(1 - 0.3)
Sigma[1,4] <- Sigma[4,1] <- log(1 + 0.5)
Sigma[2,3] <- Sigma[3,2] <- log(1 - 0.1)
Sigma[2,4] <- Sigma[4,2] <- log(1 + 0.2)
Sigma[3,4] <- Sigma[4,3] <- log(1 - 0.5)

Sigma <- nearPD(Sigma)$mat
frail <- rlog_norm(n = 1000, Sigma)


lambda_update <- function(lambda, vsts, state, jump_time, i, id){

  # individual frailty at transition 3
  if(state[i] == 1){
    lambda[1,2] <-  0.12 * frail[id,1]
    lambda[1,3] <-  0.03 * frail[id,2]
  }
  if(state[i] == 2){
    lambda[2,1] <-  0.15 * frail[id,3]
    lambda[2,3] <-  0.1 * frail[id,4]
  }

  lambda
}

par_list2 <- list(lambda = lambda)

data_sim <- sim_fun(init_probs = c(1,0,0), jump_types, pars = par_list2,
                   cens_time = 1000, nbr = 1000, hist_dep = T)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
