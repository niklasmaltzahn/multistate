########################################################### pr individual alg
alg1 <- function(id, internal_tmat, init_state, jump_types, steps, cens_time, state, pars,
                 jmp_time, hold_time, times ,s, b, hist_dep){

  id <- id
  ################################### Initial states
  state[1] <- init_state
  status_vec <- rep(NA, times = steps-1)

  ################################### nbr of returns to states
  nbr_vsts <- matrix(NA, ncol = ncol(jump_types), nrow = steps)
  colnames(nbr_vsts) <- paste("state", 1:ncol(jump_types), sep = "")
  nbr_vsts[1, ] <- sapply(1:ncol(jump_types), function(y){sum(state[1] == y)})

  ################################### Absorbing states
  h1 <- sapply(1:nrow(jump_types), function(x){sum(as.numeric(is.na(jump_types[x, ])))})
  abs_states <- which(h1 == ncol(jump_types))

  ################################### cencoring times
  if(is.numeric(cens_time)){
    cens_time <- cens_time
  } else if (cens_time$type == "exp"){
    cens_time <- rexp(1, rate = cens_time$rate)
  } else if (cens_time$type == "unif"){
    cens_time <- runif(1, min = cens_time$min, max = cens_time$max)
  }

  ################################### Interarrival times
    t_fun <- expression(Interarrivals(id, internal_tmat, jump_types, possible_jumps, state,
                                      jmp_time, pars, i, hist_dep, nbr_vsts))

  ################################### simulation
  for(i in 1:(steps-1)){

    possible_jumps <- which(!is.na(jump_types[state[i], ]))
    times[possible_jumps] <- eval(t_fun)
    hold_time[i] <- min(times[possible_jumps])
    jmp_time[i] <- sum(hold_time[1:i])
    state[i+1] <- which(hold_time[i] == times)
    nbr_vsts[(i+1), ] <- sapply(1:ncol(jump_types), function(y){sum(state[1:(i+1)] == y)})

    ##### Vector of possible states to jumo to. Important for long form
    s <- c(s, possible_jumps)
    b <- c(b, length(possible_jumps))

    ##### Status variable
    status_vec[i] <- as.numeric(jmp_time[i] <= cens_time)

    ##### If absorbing state is reached or cecoring occurs the stop
    break_criteria <- (sum(state[i + 1] == abs_states) > 0) | (jmp_time[i] > cens_time)
    if(break_criteria){ break }
  }

  k <- min(i, steps-1)
  if(k > 1){
    Tstart <- c(0, jmp_time[1:(k-1)])
  } else {
    Tstart <- 0
  }

  if(jmp_time[k] >= cens_time){
    jmp_time[k] <- cens_time
    hold_time[k] <- cens_time - Tstart[k]
  }

  status_vec <- status_vec[!is.na(status_vec)]
  ################################### preparating data
  L1 <-  list(Tstart = Tstart, Tstop = jmp_time[1:k],
              hold_time = hold_time[1:k], jmp_time = jmp_time[1:k],
              state = state[1:(k+1)],
              from = state[1:k], to = state[2:(k+1)], status_vec = status_vec,
              s = s[2:length(s)], b = b[2:(k+1)], k = k,
              nbr_vsts = nbr_vsts)

  df <- data.frame(Tstart = L1$Tstart, Tstop = L1$Tstop,
                   time = L1$hold_time,
                   status = status_vec,
                   from = L1$from,
                   to = L1$to)

  ################# adding transitions
  trans <- sapply(1:nrow(df), function(x){internal_tmat[df$from[x],df$to[x]]})
  df$trans <- trans

  ############ output
  list(L1, df)

}



######################################################### Simulation function
sim_fun <- function(init_probs, jump_types, pars, cens_time,
                    nbr = 1, hist_dep = F, steps = 1000){

  internal_tmat <- tmat_function(jump_types)
  init_state <- sample(1:ncol(jump_types), size = nbr, replace = T, prob = init_probs)
  state <- numeric(steps)
  jmp_time <- numeric(steps-1)
  hold_time <- numeric(steps)
  times <- numeric(ncol(jump_types))
  s <- 0
  b <- 0

  ####### expression
  dat_list <- list()

  for(i in 1:nbr){
    dat_list[[i]] <- alg1(id = i, internal_tmat, init_state[i], jump_types, steps, cens_time,
                          state, pars, jmp_time, hold_time, times ,s, b, hist_dep)
    dat_list[[i]][[2]]$id <- rep(i, times = nrow(dat_list[[i]][[2]]))
  }

  ############ output
  list_df <- lapply(1:length(dat_list), function(x) dat_list[[x]][[2]])
  df <- as.data.frame(rbindlist(list_df))

  order <- c("id", "from", "to", "trans", "Tstart", "Tstop", "time", "status")
  df <- df[,order]

  ### output also data_list if you wish to investigate something about simulations
  df
}








