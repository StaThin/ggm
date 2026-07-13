#' Plot mixed graphs
#'
#' @description This function takes an adjacency matrix or a graph object 
#' and generates a highly customizable, clean static visual representation of the network.
#'
#' @param a A square adjacency matrix, an \code{igraph} object, a \code{graphNEL} object, or a character vector.
#' @param dashed Logical. If \code{TRUE} (default), edges with specific identifiers will be drawn as dashed lines.
#' @param layout An \code{igraph} layout function or matrix (e.g., \code{layout_nicely}).
#' @param directed Logical. Indicates whether the graph should be treated as directed.
#' @param noframe Logical. If \code{TRUE}, removes vertex frames and sets node backgrounds to white.
#' @param nodesize Numeric. The size of the vertices (nodes). Default is 20.
#' @param vld Numeric. Distance of the vertex labels from the nodes.
#' @param vc Character. The fill color of the nodes.
#' @param vfc Character. The frame (border) color of the nodes.
#' @param colbid Character. Color for bidirectional or special edges.
#' @param coloth Character. Color for standard edges.
#' @param cex Numeric. Text expansion factor for vertex labels.
#' @param ew Numeric. The width (thickness) of the edges.
#' @param eas Numeric. The size of the edge arrowheads.
#' @param ... Additional arguments passed directly to the underlying \code{plot.igraph} function.
#'
#' @return An invisible list containing two elements:
#' \item{tkp.id}{\code{NULL} (retained solely for backward compatibility with older package versions).}
#' \item{igraph}{The processed internal graph object of class \code{igraph}.}
#'
#' @export
#'
#' @examples
#' 
#' exvec<-c("b",1,2,"b",1,14,"a",9,8,"l",9,11,
#'          "a",10,8,"a",11,2,"a",11,9,"a",11,10,
#'          "a",12,1,"b",12,14,"a",13,10,"a",13,12)
#' plotGraph2(exvec)
#' ############################################
#' amat<-matrix(c(0,11,0,0,10,0,100,0,0,100,0,1,0,0,1,0),4,4)
#' plotGraph2(amat)
#' plotGraph2(makeMG(bg = UG(~a*b*c+ c*d), dg = DAG(a ~ x + z, b ~ z )))
#' plotGraph2(makeMG(bg = UG(~a*b*c+ c*d), dg = DAG(a ~ x + z, b ~ z )), dashed = TRUE)
#' # A graph with double and triple edges
#' G <-
#' structure(c(0, 101, 0, 0, 100, 0, 100, 100, 0, 100, 0, 100, 0,
#' 111, 100, 0), .Dim = c(4L, 4L), .Dimnames = list(c("X", "Z",
#' "Y", "W"), c("X", "Z", "Y", "W")))
#' plotGraph2(G)
#' # A regression chain graph with longer labels
#' G <- makeMG(bg = UG(~ Love * Constraints + Constraints * Reversal+ Abuse * Distress), 
#' dg = DAG(Love ~ Abuse + Distress, Constraints ~ Distress, Reversal ~ Distress, Abuse ~ Fstatus, Distress ~ Fstatus), 
#' ug = UG(~Fstatus * Schooling + Schooling * Age))
#' plotGraph2(G, noframe = TRUE)
#' # A graph with 4 edges between two nodes.
#' G4 = matrix(0, 2, 2); G4[1,2] = 111; G4[2,1] = 111
#' plotGraph2(G4)
plotGraph2 <- function(a,
                       dashed = TRUE,
                       layout = igraph::layout_nicely,
                       directed = FALSE,
                       noframe = FALSE,
                       nodesize = 20,
                       vld = 0,
                       vc = "gray80",
                       vfc = NA,
                       colbid = "FireBrick3",
                       coloth = "gray40",
                       cex = 1.5,
                       ew = 1.5,
                       eas = 0.9,
                       ...) {
  
  # 1. Controllo sicuro del tipo di oggetto (Addio class() == ...)
  if (inherits(a, "igraph") || inherits(a, "graphNEL") || is.character(a)) {
    a <- grMAT(a)
  }
  
  if (!is(a, "matrix")) {
    stop("'object' is not in a valid format")
  }
  
  if (nrow(a) != ncol(a)) {
    stop("'object' is not in a valid adjacency matrix form")
  }
  
  # 2. Configurazione nomi righe/colonne se mancanti
  if (length(rownames(a)) != ncol(a)) {
    rownames(a) <- 1:ncol(a)
    colnames(a) <- 1:ncol(a)
  }
  
  if (!directed) {
    if (all(a == t(a)) && all(a[a != 0] == 1)) {
      a <- a * 10
    }
  }
  
  # 3. Estrazione Vettorizzata degli Archi (Ottimizzazione del vecchio doppio ciclo)
  matrice_sup <- a
  matrice_sup[lower.tri(matrice_sup)] <- 0
  
  celle_attive <- which(matrice_sup != 0, arr.ind = TRUE)
  
  l1 <- c()
  l2 <- c()
  
  if (nrow(celle_attive) > 0) {
    valori <- matrice_sup[celle_attive]
    i <- celle_attive[, "row"]
    j <- celle_attive[, "col"]
    
    l1_list <- list()
    l2_list <- list()
    
    # Caso 1
    id1 <- (valori == 1)
    if (any(id1)) {
      l1_list$caso1 <- as.vector(rbind(i[id1], j[id1]))
      l2_list$caso1 <- rep(2, sum(id1))
    }
    
    # Caso %% 10 == 1 (verifica speculare sulla matrice trasposta)
    valori_trasposti <- t(a)[celle_attive]
    id_speciale <- (valori_trasposti %% 10 == 1)
    if (any(id_speciale)) {
      l1_list$caso_sp <- as.vector(rbind(j[id_speciale], i[id_speciale]))
      l2_list$caso_sp <- rep(2, sum(id_speciale))
    }
    
    # Caso 10
    id10 <- (valori == 10)
    if (any(id10)) {
      l1_list$caso10 <- as.vector(rbind(i[id10], j[id10]))
      l2_list$caso10 <- rep(0, sum(id10))
    }
    
    # Caso 11
    id11 <- (valori == 11)
    if (any(id11)) {
      l1_list$caso11 <- as.vector(rbind(i[id11], j[id11], i[id11], j[id11]))
      l2_list$caso11 <- as.vector(rbind(rep(2, sum(id11)), rep(0, sum(id11))))
    }
    
    # Caso 100
    id100 <- (valori == 100)
    if (any(id100)) {
      l1_list$caso100 <- as.vector(rbind(i[id100], j[id100]))
      l2_list$caso100 <- rep(3, sum(id100))
    }
    
    # Caso 101
    id101 <- (valori == 101)
    if (any(id101)) {
      l1_list$caso101 <- as.vector(rbind(i[id101], j[id101], i[id101], j[id101]))
      l2_list$caso101 <- as.vector(rbind(rep(2, sum(id101)), rep(3, sum(id101))))
    }
    
    # Caso 110
    id110 <- (valori == 110)
    if (any(id110)) {
      l1_list$caso110 <- as.vector(rbind(i[id110], j[id110], i[id110], j[id110]))
      l2_list$caso110 <- as.vector(rbind(rep(0, sum(id110)), rep(3, sum(id110))))
    }
    
    # Caso 111
    id111 <- (valori == 111)
    if (any(id111)) {
      l1_list$caso111 <- as.vector(rbind(i[id111], j[id111], i[id111], j[id111], i[id111], j[id111]))
      l2_list$caso111 <- as.vector(rbind(rep(2, sum(id111)), rep(0, sum(id111)), rep(3, sum(id111))))
    }
    
    l1 <- unlist(l1_list, use.names = FALSE)
    l2 <- unlist(l2_list, use.names = FALSE)
  }
  
  # 4. Generazione del grafo igraph
  if (length(l1) == 0) {
    agr <- igraph::make_empty_graph(n = nrow(a), directed = TRUE)
    V(agr)$name <- rownames(a)
    par(mar = c(0, 0, 0, 0) + .1)
    plot(agr, vertex.label = rownames(a))
    return(invisible(list(tkp.id = NULL, igraph = agr)))
  }
  
  agr <- igraph::make_graph(l1, n = nrow(a), directed = TRUE)
  ed0 <- igraph::as_edgelist(agr)
  ne <- nrow(ed0)
  
  # 5. Calcolo curve (Ottimizzato con split/vettorizzazione anziché ciclo For su stringhe)
  ed <- apply(apply(ed0, 1, sort), 2, paste, collapse = "-")
  tb <- table(ed)
  curve <- rep(0, ne)
  
  if (any(tb > 1)) {
    archi_ripetuti <- names(tb[tb > 1])
    indici_archi <- which(ed %in% archi_ripetuti)
    gruppi <- split(indici_archi, ed[indici_archi])
    
    for (g in gruppi) {
      n_ripetizioni <- length(g)
      U <- ed0[g, , drop = FALSE]
      
      if (n_ripetizioni == 2) {
        if (all(is.element(c(0, 3), l2[g]))) {
          curve[g] <- c(0.9, -0.9)
        } else if (all(U[1, ] == U[2, ])) {
          curve[g] <- c(0.6, -0.6)
        } else {
          curve[g] <- c(0.6, 0.6)
        }
      } else if (n_ripetizioni == 3) {
        curve[g[l2[g] == 3]] <- 0.9
        curve[g[l2[g] == 0]] <- -0.9
      } else if (n_ripetizioni == 4) {
        curve[g[l2[g] == 3]] <- 0.3
        curve[g[g[l2[g] == 0]]] <- -0.3
        curve[g[l2[g] == 1]] <- 0.9
        curve[g[l2[g] == 2]] <- 0.9
      }
    }
  }
  
  # 6. Colori e Stili degli Archi
  col <- rep(coloth, ne)
  col[l2 == 3] <- colbid
  
  if (dashed) {
    ety <- rep(1, ne)
    ety[l2 == 3] <- 2
  } else {
    ety <- rep(1, ne)
  }
  
  if (noframe) {
    vfc <- "white"
    vc <- "white"
  }
  
  # 7. Grafico statico standard
  par(mar = c(0, 0, 0, 0) + .1)
  id <- plot(
    agr,
    layout = layout,
    edge.curved = curve,
    vertex.label = rownames(a),
    edge.arrow.mode = l2,
    edge.color = col,
    edge.lty = ety,
    vertex.label.family = "sans",
    edge.width = ew,
    vertex.size = nodesize,
    vertex.frame.color = vfc,
    vertex.color = vc,
    vertex.label.cex = cex * 0.8,
    edge.arrow.size = eas,
    vertex.label.dist = vld,
    margin = c(0, 0, 0, 0),
    ...
  )
  
  igraph::V(agr)$name <- rownames(a)
  agr <- igraph::set_edge_attr(agr, "edge.arrow.mode", index = igraph::E(agr), l2)
  
  return(invisible(list(tkp.id = NULL, igraph = agr)))
}

