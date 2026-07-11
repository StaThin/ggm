# 1. Funzione per calcolare i punti di un arco curvo e le coordinate delle frecce
disegna_arco_bidirezionale <- completeness <- function(x1, y1, x2, y2, curvatura = 0.2, raggio_nodo = 0.08, dim_freccia = 0.04) {
  # Distanza e punto medio tra i due nodi
  dx <- x2 - x1
  dy <- y2 - y1
  dist <- sqrt(dx^2 + dy^2)
  
  mx <- (x1 + x2) / 2
  my <- (y1 + y2) / 2
  
  # Vettore perpendicolare per il punto di controllo della curva
  px <- -dy / dist
  py <-  dx / dist
  
  # Punto di controllo (regola l'altezza della curva)
  cx <- mx + px * (dist * curvatura)
  cy <- my + py * (dist * curvatura)
  
  # Genera i punti della curva di Bézier quadratica
  t <- seq(0, 1, length.out = 100)
  bx <- (1-t)^2 * x1 + 2*(1-t)*t * cx + t^2 * x2
  by <- (1-t)^2 * y1 + 2*(1-t)*t * cy + t^2 * y2
  
  # Trova i punti in cui la curva tocca il BORDO del nodo (accorciamento)
  dist_da_n1 <- sqrt((bx - x1)^2 + (by - y1)^2)
  dist_da_n2 <- sqrt((bx - x2)^2 + (by - y2)^2)
  
  # Indici dei punti della curva che si trovano fuori dal raggio del nodo
  indici_validi <- which(dist_da_n1 >= raggio_nodo & dist_da_n2 >= raggio_nodo)
  
  if(length(indici_validi) > 2) {
    bx_visibile <- bx[indici_validi]
    by_visibile <- by[indici_validi]
    
    # Disegna la linea curva dell'arco
    lines(bx_visibile, by_visibile, col = "gray40", lwd = 2)
    
    # --- FUNZIONE INTERNA PER DISEGNARE LA PUNTA DELLA FRECCIA ---
    disegna_punta <- function(px_punta, py_punta, px_retro, py_retro) {
      tdx <- px_punta - px_retro
      tdy <- py_punta - py_retro
      tdist <- sqrt(tdx^2 + tdy^2)
      ux <- tdx / tdist
      uy <- tdy / tdist
      nx <- -uy
      ny <-  ux
      
      # 3 vertici del triangolo della freccia
      p1_x <- px_punta
      p1_y <- py_punta
      p2_x <- px_retro - ux * dim_freccia + nx * (dim_freccia * 0.5)
      p2_y <- py_retro - uy * dim_freccia + ny * (dim_freccia * 0.5)
      p3_x <- px_retro - ux * dim_freccia - nx * (dim_freccia * 0.5)
      p3_y <- py_retro - uy * dim_freccia - ny * (dim_freccia * 0.5)
      
      polygon(c(p1_x, p2_x, p3_x), c(p1_y, p2_y, p3_y), col = "gray40", border = "gray40")
    }
    
    # Freccia all'estremità del Nodo 2 (Usa gli ultimi punti visibili per la tangente)
    n2_fine <- length(bx_visibile)
    disegna_punta(bx_visibile[n2_fine], by_visibile[n2_fine], bx_visibile[n2_fine-2], by_visibile[n2_fine-2])
    
    # Freccia all'estremità del Nodo 1 (Usa i primi punti visibili per la tangente invertita)
    disegna_punta(bx_visibile[1], by_visibile[1], bx_visibile[3], by_visibile[3])
  }
}

# 2. Funzione per disegnare un arco singolo (linea retta standard)
disegna_arco_singolo <- function(x1, y1, x2, y2, raggio_nodo = 0.08, dim_freccia = 0.04) {
  dx <- x2 - x1
  dy <- y2 - y1
  dist <- sqrt(dx^2 + dy^2)
  ux <- dx / dist
  uy <- dy / dist
  
  # Accorcia la linea per non farla entrare nei nodi
  x1_bordo <- x1 + ux * raggio_nodo
  y1_bordo <- y1 + uy * raggio_nodo
  x2_bordo <- x2 - ux * raggio_nodo
  y2_bordo <- y2 - uy * raggio_nodo
  
  # Disegna la linea retta
  lines(c(x1_bordo, x2_bordo), c(y1_bordo, y2_bordo), col = "gray40", lwd = 2)
  
  # Disegna la freccia solo alla fine (Nodo 2)
  nx <- -uy
  ny <-  ux
  p1_x <- x2_bordo
  p1_y <- y2_bordo
  p2_x <- x2_bordo - ux * dim_freccia + nx * (dim_freccia * 0.5)
  p2_y <- y2_bordo - uy * dim_freccia + ny * (dim_freccia * 0.5)
  p3_x <- x2_bordo - ux * dim_freccia - nx * (dim_freccia * 0.5)
  p3_y <- y2_bordo - uy * dim_freccia - ny * (dim_freccia * 0.5)
  
  polygon(c(p1_x, p2_x, p3_x), c(p1_y, p2_y, p3_y), col = "gray40", border = "gray40")
}


# ==============================================================================
# SCRIPT DI TEST (Disegno del Grafo)
# ==============================================================================

# Definiamo le coordinate X e Y dei nodi (es. 4 nodi disposti a cerchio)
nodi_x <- c(0.2, 0.8, 0.8, 0.2)
nodi_y <- c(0.8, 0.8, 0.2, 0.2)
nomi_nodi <- c("1", "2", "3", "4")

# Prepariamo la finestra di plot vuota
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)

# --- 1. DISEGNO DEGLI ARCHI ---
# Arco bidirezionale curvo tra Nodo 1 e Nodo 2
disegna_arco_bidirezionale(nodi_x[1], nodi_y[1], nodi_x[2], nodi_y[2], curvatura = 0.2)

# Archi singoli dritti
disegna_arco_singolo(nodi_x[2], nodi_y[2], nodi_x[3], nodi_y[3]) # 2 -> 3
disegna_arco_singolo(nodi_x[3], nodi_y[3], nodi_x[4], nodi_y[4]) # 3 -> 4
disegna_arco_singolo(nodi_x[4], nodi_y[4], nodi_x[1], nodi_y[1]) # 4 -> 1

# --- 2. DISEGNO DEI NODI (Sopra gli archi per coprire eventuali imperfezioni)
for(i in 1:4) {
  # Disegna il cerchio del nodo (raggio = 0.08, coerente con le funzioni sopra)
  symbols(nodi_x[i], nodi_y[i], circles = 0.08, inches = FALSE, 
          add = TRUE, bg = "lightblue", fg = "black", lwd = 2)
  # Aggiunge il testo
  text(nodi_x[i], nodi_y[i], labels = nomi_nodi[i], font = 2, cex = 1.2)
}

