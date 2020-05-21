library(ggplot2)

SetScene <- function(n,scale=1) {
  lim = scale*n/2
  x0 = runif(n,min = -lim, max = lim)
  y0 = runif(n,min = -lim, max = lim)
  data.frame(ID=1:n,x0,y0)
}

Move <- function(phi,d,out=NULL) {
  if (!length(phi)==length(d)) stop("Length of phi and d are not equal")
  x = d*cos(phi)
  y = d*sin(phi)
  xy = cbind(x,y)
  if (is.null(out)) {return(xy)} else {
    if(out=="x") return(x)
    if(out=="y") return(y)
  }
}

MakeSteps <- function(n,time,scale=1) {
  t1=data.frame(time = 1:time)
  d1 = merge(SetScene(n,scale),t1)
  ord <- with(d1,order(ID,time))
  d2 = within(d1[ord,],{
    phi=runif(n*time,0,2*pi)
    d = runif(n*time,0,1)
    x1 = ifelse(time==1,x0,Move(phi,d,out="x"))
    y1 = ifelse(time==1,y0,Move(phi,d,out="y"))
  })
  x = aggregate(x1~ID,FUN=cumsum,data=d2)
  y = aggregate(y1~ID,FUN=cumsum,data=d2)
  d2$x = as.vector(t(x$x1))
  d2$y = as.vector(t(y$y1))
  return(d2)
}

dset <- MakeSteps(100,5000,scale = 50)
ggplot(data = dset,mapping = aes(x=x,y=y, color=factor(ID))) + geom_path()





Distance <- function(x_v,y_v) {
  if (!length(x_v)==length(y_v)) stop("Length of x and y are not equal")
  bigX = outer(x_v,x_v,FUN = function (x,y) (x-y)^2)
  bigY = outer(y_v,y_v,FUN = function (x,y) (x-y)^2)
  return(sqrt(bigX + bigY))
}

transProb <- function(D_matrix, P_D0=1) {
  if (!is.matrix(X)) stop("Object for argument X is not a matrix")
  ind = as.vector(upper.tri(X))
  x = as.vector(X)
  x[!ind] = 0
  matrix(x,nrow = nrow(X))
  }
colorspaces