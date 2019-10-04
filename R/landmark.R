library(data.table)

land_mark <- function(data, s, l){
    
    
      lm_start <- which((data$Tstart <= s) & (s < data$Tstop) & (data$from %in% l))
      id_lm <- unique(data$id[lm_start])
      sbs <- subset(data, id %in% id_lm)
      
 
      lm_start <- which((sbs$Tstart <= s) & (s < sbs$Tstop) & (sbs$from %in% l))
      lm_start <- lm_start[which(c(T,diff(sbs$id[lm_start]) != 0))]
      lm_stop <- c(which(diff(sbs$id) != 0), nrow(sbs))
      lm_rows <- unlist(sapply(1:length(lm_start), function(x) lm_start[x]:lm_stop[x]))
    
    as.data.frame(sbs[lm_rows,])
}

