########################################################### jump times
r_exp_wei <- function(lambda, nu){
  (-log(runif(n = 1)) / (lambda))^{(1/nu)}
}

r_gom <- function(lambda, alpha){
  log(1 - ((alpha * log(runif(n = 1))) / (lambda))) / alpha
}

### costumized hazard function
haz_costum <- function(initial_grid, grid_length, method, plot = FALSE, type = "cumhaz"){
  
  hazSpline <- spline(initial_grid[,1], initial_grid[,2], n = grid_length, method = method)
  cum_haz <- c(0, cumsum(hazSpline$y[-1] * diff(hazSpline$x)))
  cumhazSpline <- spline(hazSpline$x, cum_haz, n = grid_length, method = method)
  cumhazFit <- smooth.spline(cumhazSpline$x,cumhazSpline$y)
  
  if(plot){
    plt_df <- data.frame(time = rep(hazSpline$x, times = 2), 
                         value = c(hazSpline$y, cumhazSpline$y), 
                         type = rep(c("hazard","cumulative hazard"), each = length(hazSpline$x)))
    print(ggplot(plt_df) + geom_line(aes(time, value, colour = type)) +
            facet_wrap(~ type, scale = "free_y") + theme(legend.position = "none"))
  }
  
  if(type == "cumhaz"){
    pred_out <- function(x) predict(cumhazFit, x)$y
  }
  if(type == "haz"){
    hazFit <- smooth.spline(hazSpline$x, hazSpline$y)
    pred_out <- function(x) predict(hazFit, x)$y
  }
  
  pred_out
}


rT_costum <- function(pred_cumhaz, t_max){
  cum_haz_val <- -log(runif(1))
  T_wait <- uniroot(function(x) pred_cumhaz(x) - cum_haz_val, interval = c(0, t_max))
  T_wait$root
}


Interarrivals <- function(id, internal_tmat, jump_types, possible_jumps, state, jmp_time, 
                          pars, i, hist_dep, nbr_vsts){
  
  v_exp <-  which(jump_types[state[i], ] == "exp")
  v_wei <-  which(jump_types[state[i], ] == "wei")
  v_gom <-  which(jump_types[state[i], ] == "gom")
  v_cos <-  which(jump_types[state[i], ] == "cos")
  samp <- numeric(ncol(jump_types))
  
  ######### parameters
  if(hist_dep == F){
    lambda <- pars$lambda
  } else {
    v_vsts <- as.data.frame(nbr_vsts[1:i, ], byrow = T)
    v_state <- state[1:i]
    v_jump_time <- jmp_time[1:i]
    lambda <- lambda_update(pars$lambda, v_vsts, v_state, v_jump_time, i, id)
  }
  
  nu <- pars$nu
  alpha <- pars$alpha
  ch_pred <- pars$ch_pred
  
  ######### simulation
  if(length(v_exp) > 0){
    samp[v_exp] <-  sapply(v_exp, function(x){
      lambda <- lambda[state[i], x]
      r_exp_wei(lambda = lambda, nu = 1)
    })
  }
  
  if(length(v_wei) > 0){
    samp[v_wei] <-  sapply(v_wei, function(x){
      lambda <- lambda[state[i], x]
      r_exp_wei(lambda = lambda, nu = pars$nu[state[i], x])
    })  
  }
  
  if(length(v_gom) > 0){
    samp[v_gom] <-  sapply(v_gom, function(x){
      lambda <- lambda[state[i], x]
      r_gom(lambda = lambda, alpha = pars$alpha[state[i], x])
    })
  }
  
  if(length(v_cos) > 0){
    samp[v_cos] <-  sapply(v_cos, function(x){
      trans_nbr <- internal_tmat[state[i], x]
      rT_costum(ch_pred$Fun[[trans_nbr]], ch_pred$t_max)
    })
  }

  samp[possible_jumps]
}
