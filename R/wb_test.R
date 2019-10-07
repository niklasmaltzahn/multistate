wb_test <- function(wb_log_ranks, n_trans, n_time, n_boot, wb_point_p_val){

  point_test <- sapply(1:n_trans, function(k) wb_log_ranks[[k]]$lr_test[,1])
  point_boot <- lapply(1:n_trans, function(k) wb_log_ranks[[k]]$lr_test_boot[,,1])
  grid_test <- sapply(1:n_trans, function(k) max(point_test[,k]))
  grid_boot <- sapply(1:n_trans, function(k) sapply(1:n_boot, function(x) max(point_boot[[k]][x,])))

  ### pvalue from wb distribution or from assymptotic distribution
  if(isTRUE(wb_point_p_val)){
    point_test_res <- lapply(1:n_trans, function(y){
      rej_rates <- sapply(1:n_time, function(x){
        quantile(point_boot[[y]][,x], probs = 0.95) < point_test[x,y]
      })
      cbind(point_test[,y], rej_rates)
    })
  } else {
    point_test_res <- lapply(1:n_trans, function(y){
      rej_rates <- sapply(1:n_time, function(x){
        print(qchisq(0.95, 1) < point_test[x,y])
        qchisq(0.95, 1) < point_test[x,y]
      })
      cbind(point_test[,y], rej_rates)
    })
  }

  grid_test_res <- lapply(1:n_trans, function(y){
    rej_rates <- quantile(grid_boot[,y], probs = 0.95) < grid_test[y]
    c(grid_test[y], rej_rates)
  })

  list(point_test = point_test, point_boot = point_boot,
       grid_test = grid_test, grid_boot = grid_boot,
       point_test_res = point_test_res, grid_test_res = grid_test_res)


}
