plot_wbMarkov <- function(wb, type = "grid", time_pt = NULL){

  if(type == "point"){
    if(is.null(time_pt)){stop("Must specify time_pt argument")}

    n_time <- which(time_pt == wb$time_grid)
    dist_mat <- sapply(1:length(wb$trans), function(x) wb$point_list$point_boot[[x]][, n_time])
    n_df <- nrow(dist_mat)
    df_grid <- data.frame(test = c(dist_mat), transition = rep(paste("trans", wb$trans, sep = ""), each = n_df))

  }

  if(type == "grid"){

    n_df <- nrow(wb$grid_list$grid_boot)
    df_grid <- data.frame(test = c(wb$grid_list$grid_boot), transition = rep(paste("trans", wb$trans, sep = ""), each = n_df))
  }

  ggplot(df_grid) + geom_density(aes(test, fill = I(gray(0.6)))) + facet_wrap(~transition) + theme_bw()


}


