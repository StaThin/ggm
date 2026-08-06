## Downstream dependencies and architecture changes
* Moved 'graph' from Imports to Suggests to avoid rigid Bioconductor requirements for general users. 
* All functions dealing with 'graphNEL' objects are now guarded with `requireNamespace("graph", quietly = TRUE)`.
