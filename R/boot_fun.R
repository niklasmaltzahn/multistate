boot_fun <- function(k, time_grid, n_boot, grp1_data, grp2_data){

  n_time <- length(time_grid)
  lr_test_boot <- array(NA, dim = c(n_boot, n_time, 2))
  lr_test <- array(NA, dim = c(n_time, 2))
  lr_process <- list()

  ## unlisting times so that indivisduals in multipla landmark samples have same G process value
  time1 <- unique(sort(unlist(sapply(1:n_time, function(x) grp1_data[[x]][[k]]$time))))
  time2 <- unique(sort(unlist(sapply(1:n_time, function(x) grp2_data[[x]][[k]]$time))))

  ## ensuring that we do not devide by zero when first time point is zero
  grp1_time <- c(ifelse(time1[1] == 0, 1, sqrt(time1[1])), time1[-1])
  grp2_time <- c(ifelse(time2[1] == 0, 1, sqrt(time2[1])), time2[-1])

  ## bootstraped log-ranks
  for(i in 1:n_time){
    lr <- log_rank(grp1_data[[i]][[k]], grp2_data[[i]][[k]], wb_list =  NULL, process = T)
    lr_test[i,] <- lr$log_rank_test
    lr_process[[i]] <- lr$log_rank_proc
  }

  ## bootstraped log-ranks
    for(j in 1:n_boot){
      G_grp1 <- sapply(1:length(time1), function(x) (rpois(n = 1, lambda = grp1_time[x]) - grp1_time[x]) / sqrt(grp1_time[x]))
      G_grp2 <- sapply(1:length(time2), function(x) (rpois(n = 1, lambda = grp2_time[x]) - grp2_time[x]) / sqrt(grp2_time[x]))

      for(i in 1:n_time){
        # keeping track of which G process values hould be in which landmark samples
        w1 <- which(time1 %in% grp1_data[[i]][[k]]$time)
        w2 <- which(time2 %in% grp2_data[[i]][[k]]$time)

        wb_list <- list(G_grp1[w1], G_grp2[w2])
        lr_test_boot[j,i,] <- log_rank(grp1_data[[i]][[k]], grp2_data[[i]][[k]], wb_list =  wb_list)
      }
    }




  list(lr_test = lr_test, lr_test_boot = lr_test_boot,
       lr_process = lr_process)
}
