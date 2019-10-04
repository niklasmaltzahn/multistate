
add_cov <- function(data, cov_data){

  # else we ad new time scale
  ids <- unique(data$id)
  expanded_frame <- lapply(1:length(ids), function(x){
    old_Tstop <- data$Tstop[data$id %in% ids[x]]
    new_times <- cov_data[cov_data[,1] %in% ids[x],2]
    new_Tstop <- unique(sort(c(old_Tstop,new_times)))
    new_Tstart <- c(0, new_Tstop[-length(new_Tstop)])
    new_id <- rep(ids[x], times = length(new_Tstop))
    data.frame(new_id = new_id, new_Tstart = new_Tstart, new_Tstop = new_Tstop)
  })
  expanded_frame <- as.data.frame(rbindlist(expanded_frame))

  # augmenting old data frame
  m <- matrix(NA, ncol = ncol(data), nrow = nrow(expanded_frame))
  colnames(m) <- names(data)
  df_new <- as.data.frame(m)

  # placing old data at old Tstop rows and cencoring the agmenting times
  df_new[expanded_frame[,3] %in% data$Tstop, ] <- data
  df_new[,"id"] <- expanded_frame[,1]
  df_new[,"Tstart"] <- expanded_frame[,2]
  df_new[,"Tstop"] <- expanded_frame[,3]
  df_new[,"time"] <- expanded_frame[,3] - expanded_frame[,2]
  df_new[!expanded_frame[,3] %in% data$Tstop, "status"] <- 0

  #ensuring that first values of (id, from, to, trans) are not na
  id_start <- which(c(T, diff(data$id) == 1))
  id_new_start <- which(c(T, diff(df_new$id) == 1))
  df_new[id_new_start,1:4] <- data[id_start,1:4]


  #placing covariate values at covariate times
  df_new[,colnames(cov_data)[3]] <- NA
  df_new[expanded_frame[,3] %in% cov_data[,2], colnames(cov_data)[3]] <- cov_data[,3]

  df_new <- na.locf(df_new)
  df_new
}
