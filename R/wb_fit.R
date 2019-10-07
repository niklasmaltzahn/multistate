wb_fit <- function(data, n_iter, time_grid, trans_vec, grp1, grp2){

  n_time <- length(time_grid)
  n_trans <- length(trans_vec)

  count_data <- lapply(1:n_time, function(x){

    lm_grp1_data <- land_mark(data, time_grid[x], grp1)
    lm_grp2_data <- land_mark(data, time_grid[x] , grp2)
    lm_grp1_data$lm <- "grp1"
    lm_grp2_data$lm <- "grp2"
    jnt <- rbind(lm_grp1_data, lm_grp2_data)

    yy <- lapply(1:n_trans, function(x){
      cf <- coxph(Surv(Tstart, Tstop, status) ~ strata(lm),
                  data = subset(jnt, trans == trans_vec[x]),
                  timefix = FALSE)
      survfit(cf)
    })
    yy
  })


  strata_idx <- lapply(1:n_time, function(z) {
    lapply(1:n_trans, function(x) {
      n_strata <- length(count_data[[z]][[x]]$strata)
      strata_start <- c(0, count_data[[z]][[x]]$strata[n_strata - 1]) + 1
      strata_end <- cumsum(count_data[[z]][[x]]$strata)
      strata_idx <- lapply(1:n_strata, function(x) strata_start[x]:strata_end[x])
      strata_idx
    })})


  grp1_data <- lapply(1:n_time, function(z) {
    lapply(1:n_trans, function(x) {
      strata_idx <- strata_idx[[z]][[x]][[1]]
      data.frame(time = count_data[[z]][[x]]$time[strata_idx],
                 risk = count_data[[z]][[x]]$n.risk[strata_idx],
                 event = count_data[[z]][[x]]$n.event[strata_idx])
    })
  })

  grp2_data <- lapply(1:n_time, function(z) {
    lapply(1:n_trans, function(x) {

      strata_idx <- strata_idx[[z]][[x]][[2]]
      data.frame(time = count_data[[z]][[x]]$time[strata_idx],
                 risk = count_data[[z]][[x]]$n.risk[strata_idx],
                 event = count_data[[z]][[x]]$n.event[strata_idx])
    })
  })

  list(grp1_data = grp1_data, grp2_data = grp2_data)

}
