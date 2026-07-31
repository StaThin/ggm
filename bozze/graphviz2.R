library(DiagrammeR)


codice_allineato <- "
digraph G {
  // IMPOSTAZIONE GLOBALE (Metodo 1): Sviluppo da Sinistra a Destra (Left to Right)
  graph [rankdir=LR, splines=true, nodesep=0.8, ranksep=1.2]
  node [shape=box, style=filled, fillcolor=PaleGreen, fontname=Helvetica]

  // Flusso principale dei dati
  Ingresso -> Controllo_1;
  
  // Sdoppiamento del flusso
  Controllo_1 -> Elaborazione_A;
  Controllo_1 -> Elaborazione_B;
  
  // Ricongiungimento
  Elaborazione_A -> Uscita;
  Elaborazione_B -> Uscita;

  // ALLINEAMENTO FORZATO (Metodo 2): 
  // Nonostante il flusso si divida, costringiamo Elaborazione_A ed Elaborazione_B 
  // a stare esattamente sulla stessa linea verticale (colonna).
  subgraph {
    rank=same;
    Elaborazione_A; 
    Elaborazione_B;
  }
}
"

# Genera il grafico e applica lo zoom interattivo
grafo <- grViz(codice_allineato)
