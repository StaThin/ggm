allSubsets <-
  function(n) {
    ## Returns all subsets of n
    p <- length(n)
    H <- data.matrix(expand.grid(rep(list(1:2), p))) - 1
    H <- split(H == 1, row(H))
    lapply(H, function(i) n[i])
  }
