#' Plot mixed graphs
#'
#' This function takes an adjacency matrix or a graph object 
#' and generates a highly customizable, clean static visual
#' representation of the network.
#'
#' @param a A square adjacency matrix, an [igraph::igraph] object, 
#' a `graphNEL`  object, or a character vector.
#' @param dashed Logical. If `FALSE` (default), 
#' bidirected edges will be drawn as continuous arcs. Otherwise they will drawn as dashed arcs
#' @param layout An `igraph` layout function or matrix. Defaults to [igraph::layout_nicely].
#' @param directed Logical. Indicates whether the graph should be treated as
#'   directed. Defaults to `FALSE`.
#' @param noframe Logical. If `TRUE`, removes vertex frames and sets node 
#'   backgrounds to white. Defaults to `FALSE`.
#' @param nodesize Numeric. The size of the vertices (nodes). Default is 20.
#' @param vld Numeric. Distance of the vertex labels from the nodes. Defaults to 0.
#' @param vc Character. The fill color of the nodes. Defaults to `"gray80"`.
#' @param vfc Character. The frame (border) color of the nodes. Defaults to `NA`.
#' @param colbid Character. Color for bidirectional or special edges. Defaults to `"FireBrick3"`.
#' @param coloth Character. Color for standard edges. Defaults to `"gray40"`.
#' @param cex Numeric. Text expansion factor for vertex labels. Defaults to 1.
#' @param ew Numeric. The width (thickness) of the edges. Defaults to 1.8.
#' @param eas Numeric. The size of the edge arrowheads. Defaults to 0.7.
#' @param eaw Numeric. The size of the edge arrow width. Defaults to 1.
#' @param ... Additional arguments passed directly to the underlying `plot.igraph` function.
#'
#' @return An invisible list containing two elements:
#' \item{tkp.id}{`NULL` (retained solely for backward compatibility with older package versions).}
#' \item{igraph}{The processed internal graph object of class `igraph`.}
#'
#' @examples
#' ## Generating the adjacency matrix from a vector and plotting
#' exvec <- c(
#'   "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11,
#'   "a", 10, 8, "a", 11, 2, "a", 11, 9, "a", 11, 10,
#'   "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12
#' )
#' plotGraph(exvec)
#' 
#' ## Plotting from a direct adjacency matrix
#' amat <- matrix(c(0, 11, 0, 0, 10, 0, 100, 0, 0, 100, 0, 1, 0, 0, 1, 0), 4, 4)
#' plotGraph(amat)
#' 
#' ## Examples using mixed graphs generation (makeMG)
#' plotGraph(makeMG(bg = UG(~ a * b * c + c * d), dg = DAG(a ~ x + z, b ~ z)))
#' plotGraph(makeMG(bg = UG(~ a * b * c + c * d), dg = DAG(a ~ x + z, b ~ z)), dashed = FALSE)
#' 
#' # A graph with double and triple edges
#' G <- structure(c(
#'     0, 101, 0, 0, 100, 0, 100, 100, 0, 100, 0, 100, 0, 111, 100, 0
#'   ), .Dim = c(4L, 4L), .Dimnames = list(c("X", "Z", "Y", "W"), c("X", "Z", "Y", "W")))
#' plotGraph(G)
#' 
#' # A regression chain graph
#' G_chain <- makeMG(
#'   bg = UG(~ Love * C + C * R + A * D),
#'   dg = DAG(L ~ A + D, C ~ D, R ~ D, A ~ F, D ~ F),
#'   ug = UG(~ F * S + S * Age)
#' )
#' plotGraph(G_chain, noframe = TRUE)
#' 
#' # A graph with 4 edges between two nodes
#' G4 <- matrix(0, 2, 2)
#' G4[1, 2] <- 111
#' G4[2, 1] <- 111
#' plotGraph(G4)
#' 
#' @export

plotGraph <- function(a,
                      dashed = FALSE,
                      layout = igraph::layout_nicely,
                      directed = FALSE,
                      noframe = FALSE,
                      nodesize = 20,
                      vld = 0,
                      vc = "gray80",
                      vfc = NA,
                      colbid = "FireBrick3",
                      coloth = "gray40",
                      cex = 1,
                      ew = 1.8,
                      eas = 0.7,
                      eaw = 1,
                      ...) {
  
  # 1. Controllo sicuro del tipo di oggetto (compatibile anche con graphNEL da Suggests)
  if (inherits(a, "igraph") || inherits(a, "graphNEL") || is.character(a)) {
    a <- grMAT(a)
  }
  
  # Uso di is.matrix al posto del vecchio is()
  if (!is.matrix(a)) {
    stop("'object' is not in a valid format", call. = FALSE)
  }
  
  if (nrow(a) != ncol(a)) {
    stop("'object' is not in a valid adjacency matrix form", call. = FALSE)
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
  
  # 3. Estrazione Vettorizzata degli Archi
  matrice_presenze <- (a != 0) | (t(a) != 0)
  matrice_presenze[lower.tri(matrice_presenze)] <- FALSE
  
  celle_attive <- which(matrice_presenze, arr.ind = TRUE)
  
  l1 <- c()
  l2 <- c()
  stile_originale <- c() 
  
  if (nrow(celle_attive) > 0) {
    i <- celle_attive[, "row"]
    j <- celle_attive[, "col"]
    
    valori <- a[celle_attive]
    valori_trasposti <- t(a)[celle_attive]
    
    # Identifichiamo PRIMA i casi 101 (sia diretti che inversi) per poterli escludere dagli altri filtri
    id101_diretto <- (valori == 101)
    id101_inverso <- (valori_trasposti == 101)
    id101 <- id101_diretto | id101_inverso
    
    l1_list <- list()
    l2_list <- list()
    stile_list <- list()
    
    # Caso 1 (Escludendo i blocchi già gestiti dal 101)
    id1 <- (valori == 1) & !id101
    if (any(id1)) {
      l1_list$caso1 <- as.vector(rbind(i[id1], j[id1]))
      l2_list$caso1 <- rep(2, sum(id1))
      stile_list$caso1 <- rep(1, sum(id1))
    }
    
    # Caso %% 10 == 1 (Escludendo i blocchi già gestiti dal 101)
    id_speciale <- (valori_trasposti %% 10 == 1) & !id101
    if (any(id_speciale)) {
      l1_list$caso_sp <- as.vector(rbind(j[id_speciale], i[id_speciale]))
      l2_list$caso_sp <- rep(2, sum(id_speciale))
      stile_list$caso_sp <- rep(1, sum(id_speciale))
    }
    
    # Caso 10
    id10 <- (valori == 10)
    if (any(id10)) {
      l1_list$caso10 <- as.vector(rbind(i[id10], j[id10]))
      l2_list$caso10 <- rep(0, sum(id10))
      stile_list$caso10 <- rep(10, sum(id10))
    }
    
    # Caso 11
    id11 <- (valori == 11)
    if (any(id11)) {
      l1_list$caso11 <- as.vector(rbind(i[id11], j[id11], i[id11], j[id11]))
      l2_list$caso11 <- as.vector(rbind(rep(2, sum(id11)), rep(0, sum(id11))))
      stile_list$caso11 <- as.vector(rbind(rep(11, sum(id11)), rep(11, sum(id11))))
    }
    
    # Caso 100 (Escludendo i blocchi già gestiti dal 101)
    id100 <- (valori == 100) & !id101
    if (any(id100)) {
      l1_list$caso100 <- as.vector(rbind(i[id100], j[id100]))
      l2_list$caso100 <- rep(3, sum(id100))
      stile_list$caso100 <- rep(100, sum(id100))
    }
    
    # === CASO 101 UNIFICATO ANTI-BUG ===
    if (any(id101)) {
      l1_101 <- c()
      l2_101 <- c()
      stile_101 <- c()
      
      for (k in which(id101)) {
        node_i <- i[k]
        node_j <- j[k]
        
        if (id101_diretto[k]) {
          archi <- as.vector(rbind(
            node_i, node_j,  # 1. Freccia nativa dritta
            node_i, node_j,  # 2. Andata dell'arco
            node_j, node_i   # 3. Ritorno dell'arco
          ))
        } else {
          archi <- as.vector(rbind(
            node_j, node_i,  # 1. Freccia nativa invertita
            node_i, node_j,  # 2. Andata dell'arco
            node_j, node_i   # 3. Ritorno dell'arco
          ))
        }
        
        l1_101 <- c(l1_101, archi)
        l2_101 <- c(l2_101, rep(2, 3)) 
        stile_101 <- c(stile_101, c(1, 100, 100))
      }
      
      l1_list$caso101 <- l1_101
      l2_list$caso101 <- l2_101
      stile_list$caso101 <- stile_101
    }
    
    # === CASO 110 UNIFICATO CON TRUCCO ANTI-BUG A 3 ARCHI ===
    id110 <- (valori == 110)
    if (any(id110)) {
      l1_110 <- c()
      l2_110 <- c()
      stile_110 <- c()
      
      for (k in which(id110)) {
        node_i <- i[k]
        node_j <- j[k]
        archi <- as.vector(rbind(
          node_i, node_j,  # 1. Linea non orientata nativa (mode = 0)
          node_i, node_j,  # 2. Andata dell'arco bidiretto (mode = 2)
          node_j, node_i   # 3. Ritorno dell'arco bidiretto (mode = 2)
        ))
        l1_110 <- c(l1_110, archi)
        l2_110 <- c(l2_110, c(0, 2, 2))
        stile_110 <- c(stile_110, c(10, 100, 100))
      }
      l1_list$caso110 <- l1_110
      l2_list$caso110 <- l2_110
      stile_list$caso110 <- stile_110
    }
    
    # Caso 111
    id111 <- (valori == 111)
    if (any(id111)) {
      l1_list$caso111 <- as.vector(rbind(i[id111], j[id111], i[id111], j[id111], i[id111], j[id111]))
      l2_list$caso111 <- as.vector(rbind(rep(2, sum(id111)), rep(0, sum(id111)), rep(3, sum(id111))))
      stile_list$caso111 <- as.vector(rbind(rep(111, sum(id111)), rep(111, sum(id111)), rep(111, sum(id111))))
    }
    
    l1 <- unlist(l1_list, use.names = FALSE)
    l2 <- unlist(l2_list, use.names = FALSE)
    stile_originale <- unlist(stile_list, use.names = FALSE)
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
  
  # 5. Calcolo curve deterministico
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
      
      # Gestione dei casi sdoppiati a 3 ripetizioni (Caso 101 e Caso 110)
      if (n_ripetizioni == 3) {
        if (any(stile_originale[g] == 1)) {
          # Caso 101: la freccia rimane DRITTA
          idx_freccia_nat <- g[stile_originale[g] == 1]
          idx_archi_bid   <- g[stile_originale[g] == 100]
          curve[idx_freccia_nat] <- 0.0
          curve[idx_archi_bid[1]] <- 0.7
          curve[idx_archi_bid[2]] <- -0.7
        } else if (any(stile_originale[g] == 10)) {
          # Caso 110: la linea non orientata rimane DRITTA, l'arco rosso si curva
          idx_linea_nat <- g[stile_originale[g] == 10]
          idx_archi_bid <- g[stile_originale[g] == 100]
          curve[idx_linea_nat] <- 0.0
          curve[idx_archi_bid[1]] <- 0.7
          curve[idx_archi_bid[2]] <- -0.7
        }
        
      } else if (n_ripetizioni == 2) {
        if (all(is.element(c(0, 3), l2[g]))) {
          curve[g] <- c(0.9, -0.9)
        } else if (all(U[1, ] == U[2, ])) {
          curve[g] <- c(0.6, -0.6)
        } else {
          curve[g] <- c(0.6, 0.6)
        }
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
  col[stile_originale == 100 | l2 == 3] <- colbid
  
  if (dashed) {
    ety <- rep(1, ne)
    ety[stile_originale == 100 | l2 == 3] <- 2
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
    edge.arrow.mode = l2,
    edge.color = col,
    edge.lty = ety,
    edge.width = ew,
    edge.arrow.size = eas,
    edge.arrow.width = eaw + 0.3, 
    vertex.label = rownames(a),
    vertex.label.family = "sans",
    vertex.size = nodesize,
    vertex.frame.color = vfc,
    vertex.color = vc,
    vertex.label.cex = cex * 0.8,
    vertex.label.dist = vld,
    margin = c(0, 0, 0, 0),
    autocurve.edges = FALSE,
    ...
  )
  
  igraph::V(agr)$name <- rownames(a)
  agr <- igraph::set_edge_attr(agr, "edge.arrow.mode", index = igraph::E(agr), l2)
  
  return(invisible(list(tkp.id = NULL, igraph = agr)))
}

#' Graph to adjacency matrix
#'
#' `grMAT` converts graph objects to a mixed adjacency matrix.
#'
#' @details Support for `graphNEL` objects requires the `graph` package 
#'   from Bioconductor, which is a suggested dependency. 
#'   If the package is missing, passing a `graphNEL` object will 
#'   trigger an informative error.
#'
#' @param agr A graph object. This can be:
#' * a `graphNEL` object, 
#' * an [igraph::igraph()] object, or 
#' * a character vector of length \eqn{3e}{3e}, where \eqn{e} 
#'   is the number of edges. If it is a vector, it must 
#'   be a sequence of triples (`type`, `node1label`, `node2label`).
#'   
#'   The type of edge can be:
#'   * `"a"` (arrow from `node1` to `node2`),
#'   * `"b"` (bi-directed arc), 
#'   * `"l"` (undirected line), or 
#'   * `"*"` (for an isolated node with `node1 == node2`).
#'
#' @return A matrix consisting of 4 different integers 
#' representing the \eqn{ij}{ij}-elements:
#' * 0 for a missing edge between \eqn{i} and \eqn{j}, 
#' * 1 for an arrow from \eqn{i} to \eqn{j}, 
#' * 10 for a full line between \eqn{i} and \eqn{j}, and 
#' * 100 for a bi-directed arrow between \eqn{i} and \eqn{j}. 
#' These numbers are added when multiple edges of different types 
#' are present. The matrix is symmetric with respect to full lines 
#' and bi-directed arrows.
#' 
#' @author Kayvan Sadeghi, Giovanni Marchetti 
#' @keywords graphs adjacency matrix mixed-graph vector
#' 
#' @examples
#' ## Generating the adjacency matrix from a vector
#' exvec <- c(
#'   "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11, "a", 10, 8,
#'   "a", 11, 2, "a", 11, 10, "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12)
#' grMAT(exvec)
#'
#' \dontrun{
#' ## Example with graphNEL (requires the 'graph' package from Bioconductor)
#' if (requireNamespace("graph", quietly = TRUE)) {
#'   V <- c("a", "b", "c")
#'   g <- graph::graphNEL(nodes = V, edgemode = "undirected")
#'   grMAT(g)
#'  }
#' }
#' 
#' g <- c("a", 3, 1, "l", 1, 2, "*", 4, 4)
#' grMAT(g)
#' @export
grMAT <- function(agr) {
  # 1. Safe check for graphNEL objects (S4 compatible for Bioconductor)
  if ("graphNEL" %in% class(agr) || inherits(agr, "graph")) {
    if (!requireNamespace("graph", quietly = TRUE)) {
      stop(
        "Package 'graph' (Bioconductor) is required to convert graphNEL objects.\n", 
        "It can be installed using: BiocManager::install('graph')", 
        call. = FALSE
      )
    }
    agr <- methods::as(agr, "matrix")
    return(agr) 
  }
  
  
  # 2. Safe check for igraph objects (using direct namespace call)
  if (inherits(agr, "igraph")) {
    return(igraph::as_adjacency_matrix(agr, sparse = FALSE))
  }
  
  # 3. Processing character vectors representing mixed graphs
  if (inherits(agr, "character")) {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form", call. = FALSE)
    }
    
    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]
    
    # Updated check: Now allowing '*' to declare isolated nodes
    if (!all(b %in% c("a", "l", "b", "*"))) {
      stop("'The numeric object' is not in a valid form", call. = FALSE)
    }
    
    # Sort unique nodes once at the beginning (includes isolated nodes defined via '*')
    Ragr <- sort(unique(agrn)) 
    ma <- length(Ragr)
    mat <- matrix(0, nrow = ma, ncol = ma)
    rownames(mat) <- Ragr
    colnames(mat) <- Ragr
    
    # Vectorized assignment of edge weights (isolated placeholder '*' gets weight 0)
    bn <- numeric(length(b))
    bn[b == "l"] <- 10
    bn[b == "a"] <- 1
    bn[b == "b"] <- 100
    bn[b == "*"] <- 0
    
    # Extract all source and destination nodes at once
    origins <- agrn[seq(1, length(agrn), 2)]
    destinations <- agrn[seq(2, length(agrn), 2)]
    
    # Match node positions instantly without any SPl helper function
    row_indices <- match(origins, Ragr)
    col_indices <- match(destinations, Ragr)
    
    # Loop to accumulate single and multiple edge weights
    for (i in seq_along(bn)) {
      r <- row_indices[i]
      c <- col_indices[i]
      val_bn <- bn[i]
      
      # If it's a designated isolated node marker or an explicit self-loop placeholder,
      # do nothing and skip to the next iteration to keep the diagonal at 0.
      if (b[i] == "*" || r == c) {
        next
      }
      
      current_val <- mat[r, c]
      
      # Historical checks for modulo remainders to accumulate overlapping edges
      if ((val_bn == 1   && current_val %% 10 != 1) || 
          (val_bn == 10  && current_val %% 100 < 10) || 
          (val_bn == 100 && current_val < 100)) {
        
        mat[r, c] <- current_val + val_bn
        
        # Apply symmetry for undirected ('l') and bi-directed ('b') edges
        if (val_bn == 10 || val_bn == 100) {
          mat[c, r] <- mat[c, r] + val_bn
        }
      }
    }
    return(mat)
  }
  
  # 4. Fallback for unsupported object classes
  stop("Input 'agr' must be a graphNEL, igraph, or character object.", call. = FALSE)
}
