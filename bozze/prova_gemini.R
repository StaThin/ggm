# ==============================================================================
# SCRIPT DI TEST COMPLETO PER TUTTI I CASI DI PLOTGRAPH2
# ==============================================================================

# 1. Carica l'ambiente del pacchetto aggiornato
devtools::load_all()

# 2. Configura una griglia grafica 3x3 per vedere tutti i casi insieme
# (Ripristina la configurazione normale alla fine del test)
old_par <- par(no.readonly = TRUE)
par(mfrow = c(3, 3), mar = c(1, 1, 2, 1))

# 3. Funzione di supporto per generare rapidamente matrici di adiacenza 2x2
crea_matrice <- function(valore) {
  m <- matrix(0, 2, 2)
  m[1, 2] <- valore
  rownames(m) <- c("1", "2")
  colnames(m) <- c("1", "2")
  return(m)
}

# ------------------------------------------------------------------------------
# GENERAZIONE ED ESECUZIONE DEI CASI
# ------------------------------------------------------------------------------

# Caso 1: Freccia singola (1 -> 2)
message("Disegno Caso 1...")
plotGraph2(crea_matrice(1), main = "Caso 1: Freccia (1->2)")

# Caso 10: Linea non orientata (1 - 2)
message("Disegno Caso 10...")
plotGraph2(crea_matrice(10), main = "Caso 10: Non orientato (1-2)")

# Caso 11: Freccia + Linea non orientata
message("Disegno Caso 11...")
plotGraph2(crea_matrice(11), main = "Caso 11: Freccia + Non orientato")

# Caso 100: Arco bidirezionale (1 <-> 2)
message("Disegno Caso 100...")
plotGraph2(crea_matrice(100), main = "Caso 100: Bidirezionale (1<->2)")

# Caso 101: Freccia + Arco bidirezionale (Il caso critico sotto esame)
message("Disegno Caso 101...")
plotGraph2(crea_matrice(101), main = "Caso 101: Freccia + Bidirezionale")

# Caso 110: Linea non orientata + Arco bidirezionale
message("Disegno Caso 110...")
plotGraph2(crea_matrice(110), main = "Caso 110: Non orientato + Bidirezionale")

# Caso 111: Tutto insieme (Freccia + Linea + Bidirezionale)
message("Disegno Caso 111...")
plotGraph2(crea_matrice(111), main = "Caso 111: Freccia + Non orient. + Bidirez.")

# Test extra: Il ciclo 1->2->4->3->1 (Quello corretto all'inizio)
message("Disegno Ciclo Orientato...")
d_ciclo <- c('a', 1, 2, 'a', 2, 4, 'a', 4, 3, 'a', 3, 1)
plotGraph2(d_ciclo, main = "Ciclo: 1->2->4->3->1")

# 4. Ripristina i parametri grafici originali di RStudio
par(old_par)
message("=== Test completato! Controlla la scheda 'Plots' di RStudio ===")

