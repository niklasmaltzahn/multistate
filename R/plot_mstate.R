
plot_mstate <- function(tmat){
  m <- (!is.na(tmat)) + 0
  colnames(m) <- rownames(m) <- 1:ncol(tmat)
  m1 <- graph.adjacency(m)
  plot.igraph(m1, vertex.color="green",
              edge.arrow.size=0.3,
              edge.color = "black")
}


