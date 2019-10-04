

format_fun <- function(data, tmat){
  
  
  ids <- unique(data$id)
  
  identifier <- lapply(1:length(ids), function(y){
    sbs1 <- subset(data, id == ids[y])
    lapply(1:nrow(sbs1), function(x) {
      pos_to <- which(!is.na(tmat[sbs1$from[x],]))
      true_to <- as.numeric(pos_to == sbs1$to[x])
      as.data.frame(cbind(pos_to, true_to))
      })
  })
  
  rep_vec <- unlist(lapply(1:length(identifier), function(y){sapply(identifier[[y]], nrow)}))
  identifier <- rbindlist(lapply(1:length(identifier), function(y) rbindlist(identifier[[y]])))
  
  
  r <- rep(1:nrow(data), times = rep_vec)
  to_long <- unlist(identifier[,1])
  from_long <- data[r,]$from
  trans_long <- sapply(1:length(r), function(x) tmat[from_long[x], to_long[x]])
  originals <- unlist(identifier[,2])
  status_long <- data[r,]$status
  status_long[!originals] <- 0
  list_long <- list(from_long, to_long, trans_long, status_long)
  
  long_data <- data[r,]
  long_data[, c("from", "to", "trans", "status")] <- bind_cols(list_long)
  
  list(long_data = long_data, originals = originals)
  
}
