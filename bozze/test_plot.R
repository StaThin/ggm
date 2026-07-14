# Tests for plotGraph2
par(mfrow = c(1,2))
d <- c('a', 1, 3, 'a', 2, 4, 'a', 3, 4, 'a', 3, 5)
plotGraph(d, tcltk = FALSE)
plotGraph2(d)


d <- c('a', 1, 2, 'a', 2, 4, 'a', 4, 3, 'a', 3, 1)
plotGraph(d)
plotGraph2(d)


mix <- c('b',1,2,'b',1,14,'a',9,8,'l',9,11,
          'a',10,8,'a',11,2,'a',11,9,'a',11,10,
          'a',12,1,'b',12,14,'a',13,10,'a',13,12)
plotGraph(mix)
plotGraph2(mix, dashed = FALSE, eas = 0.6, ew = 1.5)



G <- makeMG(
  bg = UG(~ L * C + C * R + A * D),
  dg = DAG(L ~ A + D, C ~ D, R ~ D, D ~ F),
  ug = UG(~ F * S + S * A)
)
plotGraph(G)
plotGraph2(G, dashed = TRUE, nodesize = 25, eas = 0.5)


amat <- matrix(c(0, 11, 0, 0, 10, 0, 100, 0, 0, 100, 0, 1, 0, 0, 1, 0), 4, 4)
plotGraph(amat)
plotGraph2(amat, eas = 0.7, eaw = 1)



ex <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, ##The adjacency matrix of a DAG
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
               0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
               0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,
               1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
               0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0),16,16, byrow = TRUE)

M <- c(3,5,6,15,16)
C <- c(4,7)
RG(ex,M,C,plot=TRUE, plotfun = plotGraph)
RG(ex,M,C,plot=TRUE, plotfun = plotGraph2)
SG(ex,M,C,plot=TRUE, plotfun = plotGraph)
SG(ex,M,C,plot=TRUE, plotfun = plotGraph2)


conf <- makeMG(dg = DAG(y ~ x, x~ z), bg = UG(~ y*x))
plotGraph(conf, dashed = TRUE)
plotGraph2(conf, dashed = TRUE)



v <- c('l', 1,2, 'a', 1,2 )
plotGraph(v)
plotGraph2(v)

v4 <- matrix(c(0, 111, 111, 0), 2, 2)
plotGraph(v4, dashed = TRUE)
plotGraph2(v4, dashed = TRUE)
