log_rank <- function(grp1_data, grp2_data, wb_list = NULL, process = F){

  time <- sort(c(grp1_data$time, grp2_data$time))
  risk_grp1 <- risk_grp2 <- rep(NA, times = length(time))
  event_grp1 <- event_grp2 <- numeric(length(time))

  risk_grp1[time %in% grp1_data$time] <- na.omit(grp1_data$risk)
  risk_grp1[length(risk_grp1)] <- ifelse(is.na(risk_grp1[length(risk_grp1)]), 0, risk_grp1[length(risk_grp1)])
  risk_grp1 <- rev(na.locf(rev(risk_grp1)))

  risk_grp2[time %in% grp2_data$time] <- na.omit(grp2_data$risk)
  risk_grp2[length(risk_grp2)] <- ifelse(is.na(risk_grp2[length(risk_grp2)]), 0, risk_grp2[length(risk_grp2)])
  risk_grp2 <- rev(na.locf(rev(risk_grp2)))

  event_grp1[time %in% grp1_data$time] <- na.omit(grp1_data$event)
  event_grp2[time %in% grp2_data$time] <- na.omit(grp2_data$event)
  total_event <- event_grp1 + event_grp2

  if(!is.null(wb_list)){

    event_grp1[time %in% grp1_data$time] <- wb_list[[1]] * na.omit(grp1_data$event)
    event_grp2[time %in% grp2_data$time] <- wb_list[[2]] * na.omit(grp2_data$event)

  }

  # test statistic
  total_risk <- sapply(1:length(time), function(x) max(1, risk_grp1[x] + risk_grp2[x]))

  Z_grp1 <- sum(event_grp1) - sum((risk_grp1 / total_risk) * (event_grp1 + event_grp2))

  sigma_grp1 <- sum(((risk_grp1 * risk_grp2) / total_risk^2) * (total_event))
  X <- Z_grp1^2 / sigma_grp1
  p.value <- 1 - pchisq(X, df = 1)

  rslts <- c(X, p.value)
  names(rslts) <- c("test", "p_value")

  if(process){
    Z_grp1_proc <- cumsum(event_grp1 - (risk_grp1 / total_risk) * (event_grp1 + event_grp2))
    rslts <- list(log_rank_test = rslts, log_rank_proc = Z_grp1_proc)

    # output
    rslts

  } else {

  # output
  rslts
  }

}





na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}
