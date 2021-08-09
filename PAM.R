set.seed(123)
pam.function<-function(x,g){
  data.row<-nrow(x)
  data.col<-ncol(x)
  cost<-matrix(0,nrow = data.row,ncol=g)
  total.cost<-rep(0,choose(data.row,g))
  for(c in 1:choose(data.row,g)){
  int.medoid<-x[sample(data.row,g,replace = F),]
  for(j in 1:g){
  for(i in 1:data.row){
    cost[i,j]=sum(abs(x[i,]-int.medoid[j,]))
  }
  }
  min.cost=apply(cost,MARGIN=1,min)
  total.cost[c]=sum(min.cost)
  }
  
  
  label=rep(0,data.row)
  for(q in 1:data.row){
    label[q]=which(cost[q,]==min.cost[q])
  }
  return(list(label=label,min.total.cost=min(total.cost)))
}


data<-matrix(c(1,5,5,5,10,25,25,25,25,29,4,1,2,4,4,4,6,7,8,7),
             nrow=10,ncol = 2)
pam.function(data,2)

data("faithful")
install.packages('cluster')
library(cluster)
pam(faithful,k=2)
