library(igraph)

# ==============================================================================
# 1. FIXED WRAPPER FUNCTION (IMMUNE TO RESIZING AND ZOOM)
# ==============================================================================
plot_custom_edges <- function(graph, layout, vertex_size = 25, edge_arrow_size = 1, vertex_color = "lightblue") {
  
  normalized_layout <- norm_coords(layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  # Freeze the aspect ratio 1:1 to maintain perfect isosceles shapes when resizing
  plot(NULL, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), type = "n", xlab = "", ylab = "", axes = FALSE, asp = 1)
  
  edge_matrix <- get.edgelist(graph, names = FALSE)
  node_radius <- vertex_size / 300
  
  # Proportion parameters for sharp and sleek arrowheads
  arrow_length <- edge_arrow_size * 0.095  
  arrow_width  <- edge_arrow_size * 0.033  
  
  for (i in 1:ecount(graph)) {
    node1 <- edge_matrix[i, 1]
    node2 <- edge_matrix[i, 2]
    
    x1 <- normalized_layout[node1, 1]; y1 <- normalized_layout[node1, 2]
    x2 <- normalized_layout[node2, 1]; y2 <- normalized_layout[node2, 2]
    
    curvature <- if (!is.null(E(graph)$curved)) E(graph)$curved[i] else 0
    arrow_mode <- if (!is.null(E(graph)$arrow.mode)) E(graph)$arrow.mode[i] else 1
    edge_color <- if (!is.null(E(graph)$color)) E(graph)$color[i] else "gray40"
    
    dx <- x2 - x1; dy <- y2 - y1
    distance <- sqrt(dx^2 + dy^2)
    if (distance == 0) next 
    
    # Generate coordinates (Straight line or Quadratic Bézier curve)
    t_steps <- seq(0, 1, length.out = 100)
    if (curvature != 0) {
      mid_x <- (x1 + x2) / 2; mid_y <- (y1 + y2) / 2
      perp_x <- -dy / distance; perp_y <- dx / distance
      control_x <- mid_x + perp_x * (distance * curvature)
      control_y <- mid_y + perp_y * (distance * curvature)
      
      bx <- (1 - t_steps)^2 * x1 + 2 * (1 - t_steps) * t_steps * control_x + t_steps^2 * x2
      by <- (1 - t_steps)^2 * y1 + 2 * (1 - t_steps) * t_steps * control_y + t_steps^2 * y2
    } else {
      bx <- x1 + t_steps * dx
      by <- y1 + t_steps * dy
    }
    
    # Clip the line segment at the exact arrow base position
    clip_radius_n1 <- node_radius + (if(arrow_mode == 3) arrow_length else 0)
    clip_radius_n2 <- node_radius + (if(arrow_mode %in% c(1,3)) arrow_length else 0)
    
    dist_from_n1 <- sqrt((bx - x1)^2 + (by - y1)^2)
    dist_from_n2 <- sqrt((bx - x2)^2 + (by - y2)^2)
    valid_indices <- which(dist_from_n1 >= clip_radius_n1 & dist_from_n2 >= clip_radius_n2)
    
    if (length(valid_indices) > 2) {
      visible_bx <- bx[valid_indices]
      visible_by <- by[valid_indices]
      
      # Render the main edge line
      lines(visible_bx, visible_by, col = edge_color, lwd = 2)
      
      # --- RADIAL ARROWHEAD RENDERER ---
      draw_perfect_arrowhead <- function(tip_x, tip_y, center_x, center_y) {
        ray_x <- center_x - tip_x
        ray_y <- center_y - tip_y
        ray_dist <- sqrt(ray_x^2 + ray_y^2)
        
        ux <- ray_x / ray_dist
        uy <- ray_y / ray_dist
        nx <- -uy
        ny <- ux
        
        base_center_x <- tip_x - ux * arrow_length
        base_center_y <- tip_y - uy * arrow_length
        
        p1_x <- tip_x
        p1_y <- tip_y
        p2_x <- base_center_x + nx * arrow_width
        p2_y <- base_center_y + ny * arrow_width
        p3_x <- base_center_x - nx * arrow_width
        p3_y <- base_center_y - ny * arrow_width
        
        polygon(c(p1_x, p2_x, p3_x), c(p1_y, p2_y, p3_y), col = edge_color, border = edge_color)
      }
      
      # Target arrowhead (Head / Cima)
      if (arrow_mode %in% c(1, 3)) {
        last_idx <- length(visible_bx)
        
        target_v_x <- x2 - visible_bx[last_idx]
        target_v_y <- y2 - visible_by[last_idx]
        target_v_dist <- sqrt(target_v_x^2 + target_v_y^2)
        
        tip_x <- x2 - (target_v_x / target_v_dist) * node_radius
        tip_y <- y2 - (target_v_y / target_v_dist) * node_radius
        
        draw_perfect_arrowhead(tip_x, tip_y, x2, y2)
      }
      
      # Source arrowhead (Tail / Fondo)
      if (arrow_mode == 3) {
        source_v_x <- x1 - visible_bx[1]
        source_v_y <- y1 - visible_by[1]
        source_v_dist <- sqrt(source_v_x^2 + source_v_y^2)
        
        tip_x <- x1 - (source_v_x / source_v_dist) * node_radius
        tip_y <- y1 - (source_v_y / source_v_dist) * node_radius
        
        draw_perfect_arrowhead(tip_x, tip_y, x1, y1)
      }
    }
  }
  
  # --- 2. RENDER VERTICES ---
  for (i in 1:vcount(graph)) {
    symbols(normalized_layout[i, 1], normalized_layout[i, 2], circles = node_radius, inches = FALSE, 
            add = TRUE, bg = vertex_color, fg = "black", lwd = 1.5)
    label_text <- if (!is.null(V(graph)$name)) V(graph)$name[i] else as.character(i)
    text(normalized_layout[i, 1], normalized_layout[i, 2], labels = label_text, font = 2, cex = 1)
  }
}

# ==============================================================================
# 2. INITIALIZATION & AUTOMATIC ATTRIBUTE MAPPING
# ==============================================================================
# 1. Create the raw directed graph (contains 1->2 and 2->1)
original_graph <- graph(c(1,2, 2,1, 2,3, 3,4, 4,1), directed = TRUE)

# 2. Identify which edges are mutual in the original graph
mutual_edges <- which_mutual(original_graph)

# 3. Collapse mutual pairs into single lines for the final layout
collapsed_graph <- as.undirected(original_graph, mode = "collapse")
final_graph <- as.directed(collapsed_graph, mode = "arbitrary")

# 4. Initialize ALL edges as standard single, straight arrows
E(final_graph)$arrow.mode <- 1          
E(final_graph)$curved     <- 0          
E(final_graph)$color      <- "gray40"

# 5. FIXED LOGIC: Automate selection using graph structure.
# Map mutual information from the original graph to the collapsed graph.
# We check which node pairs in the final graph had a mutual relationship.
edge_matrix <- get.edgelist(final_graph, names = FALSE)
for (i in 1:ecount(final_graph)) {
  u <- edge_matrix[i, 1]
  v <- edge_matrix[i, 2]
  
  # Check if there was an edge going both ways in the original dataset
  if (are_adjacent(original_graph, u, v) && are_adjacent(original_graph, v, u)) {
    E(final_graph)$arrow.mode[i] <- 3    # Turn into bidirected (<->)
    E(final_graph)$curved[i]     <- 0.35 # Apply curvature ONLY to this mutual edge
  }
}

# 6. Execute plotting
graph_layout <- layout_in_circle(final_graph)
plot_custom_edges(final_graph, layout = graph_layout, vertex_size = 25, edge_arrow_size = 1.1)
