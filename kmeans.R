library(pdist)
set.seed(133)
kmeans.function<-function(x,g){
  x.row=nrow(x)
  x.column=ncol(x)
  int.mean=x[sample(x.row,g,replace = FALSE),]
  mean.new=c(0,0,0)
  dist.matrix=matrix(0,nrow=x.row,ncol=g)
  label=rep(0,x.row)
  diff.range=rep(0.1,g)
  mean.diff=rep(1000,g)
  iter=0
  tr=NULL
  while(sum(abs(mean.diff)<diff.range)!=g || iter<200){#sum(abs(mean.diff)<diff.range)!=g || 
  for(j in 1:g){
    for(i in 1:(x.row)){
    p.dist=pdist(x[i,],int.mean[j,])
    dist.matrix[i,j]=p.dist[1,1]
    }
    }
  min.dist=apply(dist.matrix,1,min)
    for(i in 1:(x.row)){
     label[i]=which(dist.matrix[i,]==min.dist[i])
    }
  new.data=cbind(x,label)
  mean.new=aggregate(new.data[,1:x.column],list(new.data[,'label']),mean)
  mean.new=mean.new[,2:(1+x.column)]
  trace=rep(0,g)
  for(k in 1:g){
    class.row=nrow(new.data[which(new.data[,'label']==k),])
    a=as.matrix(new.data[which(new.data[,'label']==k),][,1:ncol(x)])
    trace[k]=sum(diag(cov(a))) 
  }
  s=sum(trace)
  mean.diff=apply((mean.new-int.mean),1,mean)
  int.mean=mean.new
  iter=iter+1
  tr=c(tr,s)
  }
  return(list(label=label, trace.list=tr,iteration=iter))
}


x=iris[,1:4]
data(iris)
iris.label=kmeans.function(x,3)
table(iris.label,iris$Species)


