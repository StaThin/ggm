adj_matrix <- function(dag) {
  nodi <- names(dag)
  n <- length(nodi)
  mat <- matrix(0, nrow = n, ncol = n, dimnames = list(nodi, nodi))
  e <- dagitty::edges(dag)
  
  if(nrow(e) == 0) return(mat)
  
  for(i in 1:nrow(e)) {
    orig <- as.character(e$v[i])
    dest <- as.character(e$w[i])
    tipo <- as.character(e$e[i])
    
    if (tipo == "->") {
      # Arco diretto: solo una direzione
      mat[orig, dest] <- 1
    } else if (tipo == "<->") {
      # Arco bidirezionale: simmetrico
      mat[orig, dest] <- 100
      mat[dest, orig] <- 100
    } else if (tipo == "--") {
      # Arco non orientato: simmetrico
      mat[orig, dest] <- 10
      mat[dest, orig] <- 10
    }
  }
  return(mat)
}
