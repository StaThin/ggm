mixDagitty <- function (dg = NULL, ug = NULL, bg = NULL) 
{
  dg <- adj_matrix(dagitty(paste("mag{", dg, "}")))
  ug <- adj_matrix(dagitty(paste("mag{", ug, "}")))
  bg <- adj_matrix(dagitty(paste("mag{", bg, "}")))
  dg.nodes <- rownames(dg)
  ug.nodes <- rownames(ug)
  bg.nodes <- rownames(bg)
  ver <- unique(c(dg.nodes, ug.nodes, bg.nodes))

  d <- length(ver)
  amat <- matrix(0, d, d)
  dimnames(amat) <- list(ver, ver)
  
  amat.dg <- amat
  amat.ug <- amat
  amat.bg <- amat
  
  if (!is.null(dg)) {
    amat.dg[dg.nodes, dg.nodes] <- dg
  }
  if (!is.null(ug)) {
    amat.ug[ug.nodes, ug.nodes] <- ug 
  }
  if (!is.null(bg)) {
    amat.bg[bg.nodes, bg.nodes] <- bg
  }
  amat.dg + amat.ug + amat.bg
}