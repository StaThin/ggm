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



v <- c('b', 1,2, 'a', 1,2 )
plotGraph(v)
plotGraph2(v, dashed = TRUE)

# v4 <- matrix(c(0, 111, 111, 0), 2, 2)
# plotGraph(v4, dashed = TRUE)
# plotGraph2(v4, dashed = TRUE)

d <- c('a', 2, 1, 'a', 3, 2, 'a', 4, 1, 'a', 4, 3)
plotGraph(d)
plotGraph2(d, dashed = TRUE)

M = 4

SG(d,M,plot=TRUE, plotfun = plotGraph2)
AG(d,M,plot=TRUE, plotfun = plotGraph2)

d <- c('a', 2, 1, 'a', 3, 2, 'a', 4, 1, 'a', 4, 2)
plotGraph(d)
plotGraph2(d, dashed = TRUE)

M = 4

plotGraph2(SG(d,M),dashed = TRUE)
SG(d,M,plot=TRUE, plotfun = plotGraph2)

nanny <- c('a', 5, 4, 'a', 5, 3,'a', 6, 4, 'a', 6, 1, 'a', 4, 2, 'a', 2, 1, 'a', 3, 1)
M <- c(5, 6)
plotGraph2(nanny, layout  = layout_on_grid)
plotGraph2(SG(nanny,M))

#########################


#######

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

SG(ex, M, C, plot = TRUE)


par(mfrow = c(1,1))
plotGraph(mg)
plotGraph2(mg)


exvec <- c("a",1,2,"a",2,3,"a",4,3,
           "a",4,5,"a",4,7,"a",5,6, 
           "a",7,6,"a",8,6,"a",8,7)

M <- c(5,8)
C<- 3 
par(mfrow =c(1,2))
# RG(exvec, M, C, plot = TRUE)
SG(exvec, M, C, plot = TRUE)
AG(exvec, M, C, plot = TRUE)




a <- makeMG(dg= DG(W ~ Z, Z ~ Y + X),
            bg= UG(~ Y*Z))
par(mfrow = c(1,1))
plotGraph2(a)

par(mfrow = c(1,2))
H <- matrix(c(0 ,100, 1, 0,
              100,0 ,100, 0, 
              0 ,100, 0,100, 
              0, 1 ,100, 0), 4, 4)
plotGraph2(H)
plotGraph2(Max(H))

par(mfrow = c(1,3))

H1 <- makeMG(dg = DAG(W ~ X, Q ~ Z),
             bg = UG(~ X*Y + Y*Z + W*Q))
H2 <- makeMG(dg = DAG(W ~ X, Q ~ Z, Y ~ X + Z),bg = UG(~ W*Q))
H3 <- DAG(W ~ X, Q ~ Z + W, Y ~ X + Z)

plotGraph2(H1); plotGraph2(H2); plotGraph2(H3)

A1 <- makeMG(dg = DG(W ~ Y),
             bg = UG(~ X*Y + Y*Z + Z*W))
A2 <- makeMG(dg = DG(W ~ Y, Y ~ X),
               bg = UG(~ Y*Z + Z*W))
A3 <- makeMG(dg = DG(W ~ Y, Y ~ X, Z ~ Y), bg = UG(~ Z*W))

plotGraph2(A1); plotGraph2(A2); plotGraph2(A3)

H <- matrix(c( 0,10, 0, 0,
               10, 0, 0, 0,
               0, 1, 0,100,
               0, 0,100, 0), 4, 4)
 plotGraph2(H)




