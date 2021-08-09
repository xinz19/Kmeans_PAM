library(pgmm)
library(MixGHD)
library(MASS)
library(factoextra)
library(ggplot2)
library(cluster.datasets)

library(clue)
accuracy <- function(truelabels, clusters) {
  #Hungarian Algorithm
  
  # labels from cluster A will be matched on the labels from cluster B
  
  minWeightBipartiteMatching <- function(clusteringA, clusteringB) {
    require(clue)
    idsA <- unique(clusteringA)  # distinct cluster ids in a
    idsB <- unique(clusteringB)  # distinct cluster ids in b
    nA <- length(clusteringA)  # number of instances in a
    nB <- length(clusteringB)  # number of instances in b
    if (length(idsA) != length(idsB) || nA != nB) {
      stop("number of clusters do not match")
    }
    
    nC <- length(idsA)
    tupel <- c(1:nA)
    
    # computing the assignment matrix
    assignmentMatrix <- matrix(rep(-1, nC * nC), nrow = nC)
    for (i in 1:nC) {
      tupelClusterI <- tupel[clusteringA == i]
      solRowI <- sapply(1:nC, function(i, clusterIDsB, tupelA_I) {
        nA_I <- length(tupelA_I)  # number of elements in cluster I
        tupelB_I <- tupel[clusterIDsB == i]
        nB_I <- length(tupelB_I)
        nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
        return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
      }, clusteringB, tupelClusterI)
      assignmentMatrix[i, ] <- solRowI
    }
    
    # optimization
    result <- solve_LSAP(assignmentMatrix, maximum = FALSE)
    attr(result, "assignmentMatrix") <- assignmentMatrix
    return(result)
  }
  
  test <- minWeightBipartiteMatching(clusters, truelabels)
  
  predicted = NULL; 
  predicted <- rep(NA, length(clusters))
  for(i in 1:length(test)) {
    predicted[which(clusters == i)] <- test[i] 
  }
  
  table <- table(predicted, truelabels)
  accuracy <- (sum(diag(table))/length(truelabels))
  classaccuracy <- vector()
  colsums <- colSums(table)
  for(i in 1:length(test)) {
    classaccuracy[i] <- table[i,i]/colsums[i]
  }
  
  return(list("accuracy" = accuracy, "classaccuracy" = classaccuracy, "table" = table,
              "mapping" = test, "mappedlabels" = predicted))
}


#call for data
data(geyser)
data(wine)
data("bankruptcy")

#Bankruptcy 
bankruptcy
#PAM algrithom

library(cluster)
bank.pam<-pam(bankruptcy,k=2)
bank.pam.label=bank.pam$clustering
bank.pam.label

bank.true.label=bankruptcy$Y+1
bank.true.label

accuracy(bank.true.label,bank.pam.label)$accuracy
#0.9545455

plot(bankruptcy$RE,bankruptcy$EBIT,col=bank.true.label)

which(bank.pam.label != bank.true.label)
pp=rep(1,length(bank.true.label))
pp[which(bank.pam.label != bank.true.label)]=2
pp

plot(bankruptcy$RE,bankruptcy$EBIT,col=bank.pam.label,pch=pp)

#Kmeans 
set.seed(123)
bank.kmean<-kmeans(bankruptcy[,c(2,3)],center=2, nstart = 10)
bank.kmean.label=bank.kmean$cluster

accuracy(bank.true.label,bank.kmean.label)
#0.6666667
bank.kmean.map=accuracy(bank.true.label,bank.kmean.label)$mappedlabels

pk=rep(1,length(bank.true.label))
pk[which(bank.kmean.map !=bank.true.label)]=3
pk

par(mfrow=c(1,3),oma = c(0, 0, 2, 0))
plot(bankruptcy$RE,bankruptcy$EBIT,col=bank.true.label, xlab='RE',ylab='EBIT',main='True')
plot(bankruptcy$RE,bankruptcy$EBIT,col=bank.pam.label,pch=pp,xlab='RE',ylab='EBIT',main='PAM')
mtext(side = 3, paste0("Accuracy = ", round(0.9545455, digits = 2)))
##bank.kmean.map[which(bank.kmean.label==1)]=2
#bank.kmean.map[which(bank.kmean.label==2)]=1
plot(bankruptcy$RE,bankruptcy$EBIT,col=bank.kmean.label,pch=pk,xlab='RE',ylab='EBIT',main='K-means')
mtext(side = 3, paste0("Accuracy = ", round(0.6666667, digits = 2)))
mtext("Comparision for Bankruptcy Data ", outer = TRUE, cex = 1.5)

#wine
 
head(wine,1)
wine.ture.label=wine$Type

par(mfrow=c(1,3))
#True plot 
clusplot(wine,
         wine$Type,
         pch=wine$Type,
         plotchar = FALSE,
         span = TRUE,
         main='True',
         cex=1,
         color = T,
         col.p = wine$Type
)


#PAM
wine.pam=pam(wine,k=3)
wine.pam.label=pam(wine,k=3)$clustering
wine.pam.label

accuracy(wine$Type,wine.pam.label)$accuracy
#0.741573
wine.map.label.pam=accuracy(wine$Type,wine.pam.label)$mappedlabels

cex=rep(1,length(wine$Type))
cex[which(wine.map.label.pam!=wine$Type)]=2

clusplot(wine,
         wine.map.label.pam,
         pch=wine.map.label.pam,
         plotchar = FALSE,
         span = TRUE,
         main='Wine PAM',
         cex=1,
         color = T,
         col.p = wine.map.label.pam
)
mtext(side = 3, paste0("Accuracy = ", round(0.741573, digits = 2)))

#kmeans
wine.kmeans=kmeans(wine,center=3,nstart=10)

accuracy(wine.ture.label,wine.kmeans$cluster)
# 0.741573

wine.kmeans.maplabel=accuracy(wine.ture.label,wine.kmeans$cluster)$mappedlabels

cex=rep(1,length(wine$Type))
cex[which(wine.kmeans.maplabel!=wine$Type)]=2

clusplot(wine,
         wine.kmeans.maplabel,
         pch=wine.kmeans.maplabel,
         plotchar = FALSE,
         span = TRUE,
         main='K-means',
         cex=1,
         col.p = wine.kmeans.maplabel
)
mtext(side = 3, paste0("Accuracy = ", round(0.741573, digits = 2)))
#


#combine graphs for wine
clusplot(wine,
         wine$Type,
         pch=wine$Type,
         plotchar = FALSE,
         span = TRUE,
         main='True',
         cex=1,
         color = T,
         col.p = wine$Type,
         sub=''
)
clusplot(wine,
         wine.map.label.pam,
         pch=wine.map.label.pam,
         plotchar = FALSE,
         span = TRUE,
         main='PAM',
         cex=1,
         color = T,
         col.p = wine.map.label.pam,
         sub=''
)
mtext(side = 3, paste0("Accuracy = ", round(0.741573, digits = 2)))
clusplot(wine,
         wine.kmeans.maplabel,
         pch=wine.kmeans.maplabel,
         plotchar = FALSE,
         span = TRUE,
         main='K-means',
         cex=1,
         color=T,
         col.p = wine.kmeans.maplabel,
         sub = ''
)
mtext(side = 3, paste0("Accuracy = ", round(0.741573, digits = 2)))
mtext("Comparision for Wine Data ", outer = TRUE, cex = 1.5)




library(mlbench)
data(Soybean)
unique(Soybean$Class)
dim(Soybean)


pairs(wine[,2:8],col=wine[,1])

agnes(bankruptcy[,2:3])
plot(agg)
cutree(agg,4)
plot(bankruptcy[,2:3],col=cutree(agg,2),main='2 Clusters')
plot(bankruptcy[,2:3],col=cutree(agg,3),main="3 Clusters")
plot(bankruptcy[,2:3],col=cutree(agg,4),main="4 Clusters")
mtext("2-4 Clusters ", outer = TRUE, cex = 1.5)

plot(bankruptcy[,2:3],col=cutree(agg,5),main="5 Clusters")
plot(bankruptcy[,2:3],col=cutree(agg,6),main="6 Clusters")
plot(bankruptcy[,2:3],col=cutree(agg,7),main="7 Clusters")
mtext("5-7 Clusters ", outer = TRUE, cex = 1.5)


agg=agnes(bankruptcy[,2:3],method="complete")
agg
agg=agnes(bankruptcy[,2:3],method="single")
agg
plot(bankruptcy[,2:3],col=agg)
plot(bankruptcy[,2:3],col=cutree(agg,8))
agn1 <- agnes(bankruptcy, method="complete")
agn1
plot(agn1)



par(mfrow=c(1,3),oma = c(0, 0, 2, 0))
data(animals)
aa.a  <- agnes(animals,method='single') # default method = "average"
aa.ga <- agnes(animals, method = "complete")
aa.wa <- agnes(animals, method = "ward")

plot(aa.a,  which.plot = 2,main="Single Linkage")
plot(aa.ga, which.plot = 2,main="Complete Linkage")
plot(aa.wa, which.plot = 2,main="Ward Linkage")
mtext("Comparision for Animal Data ", outer = TRUE, cex = 1.5)

plot(aa.wa, which.plot = 2)













