hybrid_fun <- function(data, s, l, non_markov_trans){
  
  lm_start <- which((data$Tstart <= s) & (s < data$Tstop))
  id_lm <- unique(data$id[lm_start])
  sbs <- subset(data, id %in% id_lm)
  
  
  lm_start <- which((sbs$Tstart <= s) & (s < sbs$Tstop))
  lm_start <- lm_start[which(c(T,diff(sbs$id[lm_start]) != 0))]
  lm_stop <- c(which(diff(sbs$id) != 0), nrow(sbs))
  lm_rows <- unlist(sapply(1:length(lm_start), function(x) lm_start[x]:lm_stop[x]))
  
  hyb_df <- as.data.frame(sbs[lm_rows,])
  
  ### landmarking non-markov transitions at start i.e at s
  id_start <- c(T, diff(hyb_df$id) != 0)
  non_mark_trans <- hyb_df$trans %in% non_markov_trans
  non_landmark_start <- !hyb_df$from %in% l
  
  # finding which ids that do not start at landmark
  w_id_start <- which(id_start)
  w_non_landmark <- which(non_landmark_start)
  w_non_landmark_start <- intersect(w_id_start, w_non_landmark)
  w_id_non_landmark_start <- hyb_df$id[w_non_landmark_start]
  
  # ids in non markov trans
  w_id_non_mark_trans <- unique(hyb_df$id[hyb_df$trans %in% non_markov_trans])
  
  #ids in non_markov_trans who do not start at landmark
  id_interscet <- intersect(w_id_non_landmark_start, w_id_non_mark_trans) 

  # throw those ids out of non markov transitions  
  w_non_mark_trans <- which(non_mark_trans)
  w_intersect <- which(hyb_df$id %in% id_interscet)
  w_out <- intersect(w_intersect, w_non_mark_trans)
  hyb_df$id[w_out] <- NA

  na.omit(hyb_df)
}