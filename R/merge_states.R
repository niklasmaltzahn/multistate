merge_states <- function(df, new_states, new_tmat){

  df_new <- df
  n_new_state <- ncol(new_tmat)
  absorbing_state <- which(apply(new_tmat, 1, function(x) sum(is.na(x))) == ncol(new_tmat))
  trans_idx <- which(!is.na(new_tmat[-absorbing_state,-absorbing_state]), arr.ind = T)
  if(length(trans_idx) == 0){trans_idx <- matrix(c(1,2), 1, 2)}

  for(i in 1:nrow(trans_idx)){
    from <- trans_idx[i,1]
    to <- trans_idx[i,2]

    ## special index if from and to are equal
    w1 <- which((df$from %in% new_states[[from]]) & (df$to %in% new_states[[from]]))
    df_new$from[w1] <- n_new_state + 1
    df_new$to[w1] <- n_new_state + 1

    w1 <- which((df$from %in% new_states[[from]]) & (df$to %in% new_states[[to]]))
    df_new$from[w1] <- from
    df_new$to[w1] <- to

    w1 <- which((df$from %in% new_states[[from]]) & (df$to %in% new_states[[absorbing_state]]))
    df_new$from[w1] <- from
    df_new$to[w1] <- absorbing_state
  }

  ## removing transitions wherer from and to are equal
  df_new$double <- as.numeric(df_new$from == df_new$to)
  df_new <- subset(df_new, double == 0)

  ## setting up new multistate data
  id_start <- which(c(T, diff(df_new$id) == 1))
  id_mid <- which(!1:nrow(df_new) %in% id_start)

  df_new$Tstart <- NA
  df_new$Tstart[id_mid] <- df_new$Tstop[id_mid - 1]
  df_new$Tstart[id_start] <- 0

  df_new$trans <- sapply(1:nrow(df_new), function(x) new_tmat[df_new$from[x], df_new$to[x]])
  df_new$time <- df_new$Tstop - df_new$Tstart

  df_new[,1:8]
}









