frames_wbMarkov <- function(wb){

  rep_vec <- unlist(lapply(wb$test_res$point_test_res, nrow))
  trans_vec <- rep(paste("trans", wb$trans, sep = ""), times = rep_vec)
  df_point <- as.data.frame(do.call(rbind, wb$test_res$point_test_res))
  df_point$trans <- trans_vec
  df_grid <- as.data.frame(do.call(rbind, wb$test_res$grid_test_res))
  df_grid$trans <- paste("trans", wb$trans, sep = "")

  names(df_point) <- names(df_grid) <- c("test", "rejected", "transition")
  rownames(df_point) <- c()

  list(df_point = df_point, df_grid = df_grid)

}
