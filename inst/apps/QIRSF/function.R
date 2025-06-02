unitscale <- function(raw.mat,cand.mat=NULL)
{
  raw.mat = as.matrix(raw.mat)
  p <- dim(raw.mat)[2]
  raw.mat.new <- raw.mat
  if(is.null(cand.mat))
  {
    cand.mat=raw.mat
  }
  cand.mat = as.matrix(cand.mat)
  for(i in 1:p)
  {
    raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
  }
  return(raw.mat.new)
}
irsf.cluster <- function(xmat,ymat,N,wt,Method) {
  library(fastcluster, quietly = T)
  Nd <- N
  wt <- wt
  nxq <- dim(xmat)[1]
  nxp <- dim(xmat)[2]
  nyp <- dim(ymat)[2]
  xmat.r <- xmat
  ymat.r <- ymat
  
  xmat.r.1 <- unitscale(xmat.r)
  ymat.r.1 <- unitscale(ymat.r)
  randmat.x <- as.matrix(xmat.r.1[,1:(nxp)])
  randmat.y <- as.matrix(ymat.r.1[,1:(nyp)])
  dx = stats::dist(randmat.x, method = "euclidean")
  dy = stats::dist(randmat.y, method = "euclidean")
  xmax = max(dx)
  xmin = min(dx)
  ymax = max(dy)
  ymin = min(dy)
  d1 = (dx-xmin)/(xmax-xmin)
  d2 = (dy-ymin)/(ymax-ymin)
  dc = wt*d1+(1-wt)*d2
  
  if (Method == "complete") {
    groupid <- stats::cutree(stats::hclust(d = dc, method = "complete"), k = Nd)
  }
  if (Method == "average"){
    groupid <- stats::cutree(stats::hclust(d = dc, method = "average"), k = Nd)
  }
  if (Method == "mcquitty"){
    groupid <- stats::cutree(stats::hclust(d = dc, method = "mcquitty"), k = Nd)
  }
  
  outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
  outmat.y <- matrix(0,nrow = Nd,ncol = nyp)
  for (j in 1:Nd) {
    cluster.x = as.matrix(randmat.x[which(groupid == j),])
    cluster.y = as.matrix(randmat.y[which(groupid == j),])
    cluster.x.original = as.matrix(xmat.r[which(groupid == j),])
    cluster.y.original = as.matrix(ymat.r[which(groupid == j),])
    nd = dim(cluster.y)[1]
    np = dim(cluster.x)[2]
    nq = dim(cluster.y)[2]
    
    cluster1 = cbind(cluster.x,cluster.y)
    
    Dist.mat = apply(cluster1, 1, function(P1){
      max(wt*((sqrt(rowSums((cluster.x-matrix(rep(P1[1:np],nd),ncol=np,byrow = T))^2))-xmin)/(xmax-xmin))+
            (1-wt)*((sqrt(rowSums((cluster.y-matrix(rep(P1[(np+1):(np+nq)],nd),ncol=nq,byrow = T))^2))-ymin)/(ymax-ymin)))
    })
    Minimax.point = which(Dist.mat==min(Dist.mat))
    if (length(Minimax.point)>1) {
      Minimax.point = sample(Minimax.point,1)
    }
    outmat.x[j,] = cluster.x.original[Minimax.point,]
    outmat.y[j,] = cluster.y.original[Minimax.point,]
  }
  
  result = list(X.design = outmat.x, Y.design = outmat.y, X.candidate = xmat.r, Y.candidate = ymat.r,
                Cluster.num = groupid,weight.value = wt)
  
  return(result)
}
Cri_IRSF_cluster <- function(Des) {
  xmat = as.matrix(Des[[1]])
  ymat = as.matrix(Des[[2]])
  Xcand = as.matrix(Des[[3]])
  Ycand = as.matrix(Des[[4]])
  wt = Des[[6]]
  
  xmat.scale = unitscale(xmat,Xcand)
  ymat.scale = unitscale(ymat,Ycand)
  qnx = dim(Xcand)[1]
  px = dim(xmat)[2]
  py = dim(ymat)[2]
  qx = dim(xmat)[1]
  
  xcand.scale = unitscale(Xcand,Xcand)
  ycand.scale = unitscale(Ycand,Ycand)
  X.max = max(as.numeric(lapply(1:qnx, function(i,xmat,xcand){
    min(as.numeric(lapply(1:qx, function(j,xmat,p){
      stats::dist(as.matrix(rbind(p[1:(px)],xmat[j,1:(px)])))
    },xmat = xmat,p = xcand[i,])))
  },xcand= xcand.scale,xmat =xmat.scale)))
  Y.max = max(as.numeric(lapply(1:qnx, function(i,ymat,ycand){
    min(as.numeric(lapply(1:qx, function(j,ymat,p){
      stats::dist(as.matrix(rbind(p[1:(py)],ymat[j,1:(py)])))
    },ymat = ymat,p = ycand[i,])))
  },ycand= ycand.scale,ymat =ymat.scale)))
  
  Xmat1 = as.data.frame(xmat[,1:(px)])
  names(Xmat1) = paste("x",c(1:(px)))
  Ymat1 = as.data.frame(ymat[,1:(py)])
  names(Ymat1) = paste("y",c(1:(py)))
  result = list(X.max=X.max,Y.max=Y.max,X.design=Xmat1,Y.design = Ymat1,weight.value = Des[[6]])
  return(result)
}

PF_max <- function(Criterion) {
  curpf<-as.data.frame(Criterion[c(which(Criterion$X.max==min(Criterion$X.max)),which(Criterion$Y.max==min(Criterion$Y.max))),])
  for (i in 1:length(Criterion$X.max)) {
    newpt<-as.data.frame(Criterion[i,])
    
    g1=newpt[1,1]>curpf[,1]
    g2=newpt[1,2]>curpf[,2]
    
    ge1=newpt[1,1]>=curpf[,1]
    ge2=newpt[1,2]>=curpf[,2]
    
    l1=newpt[1,1]<curpf[,1]
    l2=newpt[1,2]<curpf[,2]
    
    le1=newpt[1,1]<=curpf[,1]
    le2=newpt[1,2]<=curpf[,2]
    
    eq1=newpt[1,1]==curpf[,1]
    eq2=newpt[1,2]==curpf[,2]
    
    cond1=(l1*le2+l2*le1)==0
    cond2=sum(g1*ge2+g2*ge1+eq1*eq2) # cond2=0 means add the point
    cond3=seq(1,dim(curpf)[1])[cond1]
    
    if(length(cond3)==0)
    {
      newpf=NULL
    }else
    {
      newpf=curpf[cond3,]
    }
    if(cond2==0)
    {
      newpf=as.data.frame(rbind(newpf,newpt))
    }
    curpf = newpf
  }
  curpf = as.data.frame(curpf)
  names(curpf) = names(Criterion)
  curpf = curpf[order(curpf$X.max),]
  row.names(curpf) = seq(1,length(curpf$X.max))
  return(curpf)
}
