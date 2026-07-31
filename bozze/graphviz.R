library(DiagrammeR)
# library(svgPanZoom)

codice_dot <- "
digraph G {
  graph [splines=true, overlap=false] 
  node [shape=circle, style=filled, fillcolor=LightGray, fontname=Helvetica]

  # Grafo misto precedente
  A -> B [label='Diretto'];
  B -> C [dir=none, label='Non diretto', color=red]; 
  C -> D [dir=both, label='Bidirezionale'];

  # DOPPIA FRECCIA CORRETTA (Senza usare il simbolo <-)
  A -> E [label='Andata (A->E)', color=blue, fontcolor=blue];
  A -> E [dir=back, label='Ritorno (E->A)', color=darkgreen, fontcolor=darkgreen];
}
"

grafo <- grViz(codice_dot)

# svgPanZoom(grafo)
