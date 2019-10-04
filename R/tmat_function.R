
tmat_function <- function(jump_types){
  tmat_vec <- rep(NA, times = nrow(jump_types)^2)
  tmat_vec[!is.na(c(t(jump_types)))] <- 1:sum(!is.na(c(t(jump_types))))
  matrix(tmat_vec, nrow(jump_types), ncol(jump_types), byrow = T)
}
