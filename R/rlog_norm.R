rlog_norm <- function(n, sigma){

  if(max(dim(Sigma)) > 1){
    mu <- -0.5 * diag(sigma)
    exp(mvrnorm(n, mu, sigma))
  } else {
    exp(rnorm(n, - 0.5 * sigma, sigma))
  }

}
