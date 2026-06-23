# =======================================================================
# Advanced Graph Layout Editor (Direct Replacement for ggm::plotGraph)
#
# Acknowledgments: Developed in collaboration with Google Gemini AI (2026).
# =======================================================================

#' Advanced Graph Layout Editor for Mixed Graphs
#'
#' @description
#' An interactive replacement for \code{\link{plotGraph}} using Shiny and visNetwork. 
#' Allows dynamic node repositioning, handles multiple edge types, and outputs node coordinates.
#'
#' @param ggm_matrix A square adjacency matrix, a graphNEL object, an igraph object, or a descriptive character vector.
#' @param dashed Logical. If \code{TRUE} (default), bidirected edges (<->) are drawn as dashed lines.
#' @param bidirected_roundness Numeric. Curvature of bidirected edges when multiple edges are present. Default is 0.25.
#'
#' @return A matrix of X and Y coordinates for each node, returned after terminating the Shiny application.
#' 
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel titlePanel p br tags actionButton observeEvent stopApp runApp shinyApp
#' @importFrom visNetwork visNetworkOutput renderVisNetwork visNetwork visNodes visNetworkProxy visGetPositions \%>\%
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ag <- makeMG(ug=UG(~y0*y1), dg=DAG(y4~y2, y2~y1), bg=UG(~y2*y3+y3*y4))
#'   coords <- graphLayout(ag)
#'   print(coords)
#' }
#' }
graphLayout <- function(ggm_matrix, dashed = TRUE, bidirected_roundness = 0.25) {
  
  # 1. CONTROLLO DIPENDENZE (Pacchetti in 'Suggests')
  missing_pkgs <- c()
  if (!requireNamespace("shiny", quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "shiny")
  if (!requireNamespace("visNetwork", quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "visNetwork")
  
  if (length(missing_pkgs) > 0) {
    stop(
      "The following package(s) are required for graphLayout: ", 
      paste(missing_pkgs, collapse = ", "), ".\n",
      "Please install them by running: install.packages(", 
      paste0("'", missing_pkgs, "'", collapse = ", "), ")", 
      call. = FALSE
    )
  }
  
  # 2. NORMALIZZAZIONE DELL'INPUT (Integrazione di grMAT)
  # Qui convertiamo graphNEL, igraph o vettori in una matrice standard di ggm
  ggm_matrix <- grMAT(ggm_matrix)
  
  # 3. LOGICA DI ESTRAZIONE DEL GRAFO (Il tuo codice originale)
  n_nodes <- nrow(ggm_matrix)
  node_names <- rownames(ggm_matrix)
  if (is.null(node_names)) {
    node_names <- paste0("v", 1:n_nodes)
  }
  
  nodes_converted <- data.frame(
    id = 1:n_nodes,
    label = node_names,
    shape = "circle",
    color = "#97C2FC", 
    stringsAsFactors = FALSE
  )

  edges_list <- list()
  edge_counter <- 1

  # -------------------------------------------------------------------
  # PRELIMINARY PHASE: Calculate edge multiplicity per node pair
  # -------------------------------------------------------------------
  edge_count_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)

  for (i in 1:(n_nodes - 1)) {
    for (j in (i + 1):n_nodes) {
      val_str_ij <- sprintf("%03d", as.integer(ggm_matrix[i, j]))
      has_bid_ij <- substr(val_str_ij, 1, 1) == "1"
      has_und_ij <- substr(val_str_ij, 2, 2) == "1"
      has_dir_ij <- substr(val_str_ij, 3, 3) == "1"

      val_str_ji <- sprintf("%03d", as.integer(ggm_matrix[j, i]))
      has_dir_ji <- substr(val_str_ji, 3, 3) == "1"

      total_types <- sum(c(has_bid_ij, has_und_ij, has_dir_ij, has_dir_ji))

      edge_count_matrix[i, j] <- total_types
      edge_count_matrix[j, i] <- total_types
    }
  }

  # -------------------------------------------------------------------
  # PHASE 1: Extract DIRECTED EDGES
  # -------------------------------------------------------------------
  for (i in 1:n_nodes) {
    for (j in 1:n_nodes) {
      if (i == j) next

      cell_value <- ggm_matrix[i, j]
      if (cell_value == 0) next

      val_str_ij <- sprintf("%03d", as.integer(cell_value))
      has_directed_ij <- substr(val_str_ij, 3, 3) == "1"

      if (has_directed_ij) {
        val_str_ji <- sprintf("%03d", as.integer(ggm_matrix[j, i]))
        has_directed_ji <- substr(val_str_ji, 3, 3) == "1"

        if (has_directed_ji) {
          if (i < j) {
            # 1. Forward Edge (i -> j)
            edges_list[[edge_counter]] <- data.frame(
              from = i, to = j, arrows = "to", dashes = FALSE,
              smooth = I(list(list(enabled = TRUE, type = "curvedCW", roundness = 0.25))),
              stringsAsFactors = FALSE
            )
            edge_counter <- edge_counter + 1

            # 2. Backward Edge (j -> i)
            edges_list[[edge_counter]] <- data.frame(
              from = i, to = j, arrows = "from", dashes = FALSE,
              smooth = I(list(list(enabled = TRUE, type = "curvedCCW", roundness = 0.25))),
              stringsAsFactors = FALSE
            )
            edge_counter <- edge_counter + 1
          }
        } else {
          # Standard Single Directed Edge
          smooth_config <- list(enabled = TRUE, type = "continuous", roundness = 0.0)
          edges_list[[edge_counter]] <- data.frame(
            from = i, to = j, arrows = "to", dashes = FALSE,
            smooth = I(list(smooth_config)),
            stringsAsFactors = FALSE
          )
          edge_counter <- edge_counter + 1
        }
      }
    }
  }

  # -------------------------------------------------------------------
  # PHASE 2: Extract SYMMETRIC EDGES (Undirected lines & Bidirected arrows)
  # -------------------------------------------------------------------
  for (i in 1:(n_nodes - 1)) {
    for (j in (i + 1):n_nodes) {
      cell_value <- ggm_matrix[i, j]
      if (cell_value == 0) next

      val_str <- sprintf("%03d", as.integer(cell_value))
      has_bidirectional <- substr(val_str, 1, 1) == "1"
      has_undirected <- substr(val_str, 2, 2) == "1"

      is_multiple <- edge_count_matrix[i, j] > 1

      # --- Undirected Edge (i - j) ---
      if (has_undirected) {
        smooth_config <- list(enabled = TRUE, type = "continuous", roundness = 0.0)
        edges_list[[edge_counter]] <- data.frame(
          from = i, to = j, arrows = "", dashes = FALSE,
          smooth = I(list(smooth_config)),
          stringsAsFactors = FALSE
        )
        edge_counter <- edge_counter + 1
      }

      # --- Bidirectional Edge (i <-> j) ---
      if (has_bidirectional) {
        if (is_multiple) {
          smooth_config <- list(enabled = TRUE, type = "curvedCCW", roundness = bidirected_roundness)
        } else {
          smooth_config <- list(enabled = TRUE, type = "continuous", roundness = 0.0)
        }

        edges_list[[edge_counter]] <- data.frame(
          from = i, to = j,
          arrows = "to, from",
          dashes = dashed,
          smooth = I(list(smooth_config)),
          stringsAsFactors = FALSE
        )
        edge_counter <- edge_counter + 1
      }
    }
  }

  if (length(edges_list) > 0) {
    edges_converted <- do.call(rbind, edges_list)
  } else {
    edges_converted <- data.frame(from = integer(), to = integer(), arrows = character(), dashes = logical(), smooth = list())
  }

  # -------------------------------------------------------------------
  # SHINY INTERFACE WITH DIRECT JAVASCRIPT CANVAS CAPTURE
  # -------------------------------------------------------------------
  ui <- fluidPage(
    titlePanel("GGM Mixed Graph Layout Editor"),
    sidebarLayout(
      sidebarPanel(
        p("1. Drag nodes with your mouse to define the layout."),
        p("2. Multiple edge codes are correctly split and curved."),
        br(),
        # Questo pulsante è gestito direttamente da JavaScript (HTML5 Canvas) ed è visibile a sinistra
        tags$button(
          id = "snap_btn",
          class = "btn btn-success",
          style = "width:100%; font-weight:bold;",
          onclick = "
            var canvas = document.querySelector('#my_graph canvas');
            if(canvas) {
              var link = document.createElement('a');
              link.download = 'ggm_graph_layout.png';
              link.href = canvas.toDataURL('image/png');
              link.click();
            } else {
              alert('Error: Canvas element not found yet. Try again in a second.');
            }
          ",
          "📸 Download PNG Diagram"
        ),
        br(), br(), br(),
        actionButton("close_btn", "Terminate and Save Coordinates to R", class = "btn-danger", style = "width:100%")
      ),
      mainPanel(
        visNetworkOutput("my_graph", height = "550px")
      )
    )
  )

  server <- function(input, output, session) {
    output$my_graph <- renderVisNetwork({
      visNetwork(nodes_converted, edges_converted) %>%
        visNodes(physics = FALSE)
    })

    observeEvent(input$close_btn, {
      visNetworkProxy("my_graph") %>% visGetPositions()
    })

    observeEvent(input$my_graph_positions, {
      positions <- input$my_graph_positions
      ordered_ids <- nodes_converted$id
      coords_matrix <- matrix(0, nrow = length(ordered_ids), ncol = 2)
      rownames(coords_matrix) <- nodes_converted$label
      colnames(coords_matrix) <- c("X", "Y")

      for (i in seq_along(ordered_ids)) {
        current_id <- as.character(ordered_ids[i])
        coords_matrix[i, "X"] <- positions[[current_id]]$x
        coords_matrix[i, "Y"] <- -positions[[current_id]]$y
      }

      stopApp(coords_matrix)
    })
  }

  runApp(shinyApp(ui = ui, server = server), launch.browser = interactive())
}


## Function SPl
SPl <- function(a, b) {
  (seq_along(a))[is.element(sort(a), b)]
}  
## Function grMAT
grMAT <- function(agr) {
  # 1. Se è già una matrice o un data.frame con la codifica ggm, la puliamo e la restituiamo subito
  if (is.matrix(agr) || is.data.frame(agr)) {
    return(as.matrix(agr))
  }
  
  # 2. Gestione oggetti graphNEL (Bioconductor)
  if (inherits(agr, "graphNEL") || inherits(agr, "graph")) {
    if (!requireNamespace("graph", quietly = TRUE)) {
      stop("Package 'graph' (Bioconductor) is required to convert graphNEL objects.\n", 
           "It can be installed as: BiocManager::install('graph')", call. = FALSE)
    }
    return(as.matrix(methods::as(agr, "matrix")))
  }
  
  # 3. Gestione oggetti igraph
  if (inherits(agr, "igraph")) {
    return(as.matrix(igraph::as_adjacency_matrix(agr, sparse = FALSE)))
  }
  
  # 4. Gestione del vettore descrittivo (character vector)
  if (is.character(agr)) {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form")
    }
    
    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]
    bn <- numeric(length(b))
    
    for (i in 1:length(b)) {
      if (b[i] != "a" && b[i] != "l" && b[i] != "b") {
        stop("'The numeric object' is not in a valid form")
      }
      if (b[i] == "l")  bn[i] <- 10
      if (b[i] == "a")  bn[i] <- 1
      if (b[i] == "b")  bn[i] <- 100
    }
    
    Ragr <- unique(agrn)
    ma <- length(Ragr)
    mat <- matrix(0, ma, ma)
    
    for (i in seq(1, length(agrn), 2)) {
      idx_from <- SPl(Ragr, agrn[i])
      idx_to   <- SPl(Ragr, agrn[i + 1])
      
      if ((bn[(i + 1) / 2] == 1   && mat[idx_from, idx_to] %% 10 != 1) || 
          (bn[(i + 1) / 2] == 10  && mat[idx_from, idx_to] %% 100 < 10) ||
          (bn[(i + 1) / 2] == 100 && mat[idx_from, idx_to] < 100)) {
        
        mat[idx_from, idx_to] <- mat[idx_from, idx_to] + bn[(i + 1) / 2]
        if (bn[(i + 1) / 2] == 10 || bn[(i + 1) / 2] == 100) {
          mat[idx_to, idx_from] <- mat[idx_to, idx_from] + bn[(i + 1) / 2]
        }
      }
    }
    rownames(mat) <- sort(Ragr)
    colnames(mat) <- sort(Ragr)
    return(mat)
  }
  
  stop("Input format not recognized by grMAT.")
}

