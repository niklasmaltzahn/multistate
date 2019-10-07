wbMarkov <- function(time_grid, data, grp1, grp2, trans, n_boot, wb_point_p_val = T){

  n_trans <- length(trans)
  n_time <- length(time_grid)
  trans_vec <- trans

  wb_fit_list <- wb_fit(data, n_iter, time_grid, trans_vec, grp1, grp2)
  wb_log_ranks <- lapply(1:n_trans, function(k) boot_fun(k, time_grid, n_boot, wb_fit_list$grp1_data, wb_fit_list$grp2_data))
  wb_test_list <- wb_test(wb_log_ranks, n_trans, n_time, n_boot, wb_point_p_val)
  wb_proccess <- lapply(1:n_trans, function(k) wb_log_ranks[[k]]$lr_process)

  point_list <- list(point_test = wb_test_list$point_test, point_boot = wb_test_list$point_boot)
  grid_list <- list(grid_test = wb_test_list$grid_test, grid_boot = wb_test_list$grid_boot)
  test_res <- list(point_test_res = wb_test_list$point_test_res, grid_test_res = wb_test_list$grid_test_res)


  list(point_list = point_list, grid_list = grid_list,
       test_res = test_res, wb_proccess = wb_proccess,
       trans = trans_vec, time_grid = time_grid)

}





