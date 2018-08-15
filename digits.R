setwd("~/wentworth/machine-learning/chapter_2")
library(Matrix)
library(DMwR)
library(class)
library(TTR)
acc <- numeric()
err <- numeric()
n <- integer()
datatr <- read.csv(file="pendigits_tra.csv",as.is=TRUE)
datate <- read.csv(file="pendigits_tes.csv",as.is=TRUE)
for (i in 1:10) {
  n[i]=i
  knear <- kNN(digit~.,datatr, datate, k=i)
  #confusion matrix
  t <- table(datate[,'digit'],knear)
  #print(t)
  #get diagonal elements of matrix
  td <- diag(t)
  #sum all the elements of the matrix t which gives 
  #the examples tested
  sumt <- sum(t)
  #some the diagonal to see how many we got correct
  sumtd <- sum(td)
  #calculate accuracy
  acc[i] <- sumtd/sumt
  #calculate realtive error
  err[i] <- (sumt-sumtd)/sumt
  #print(acc[i])
  #print(err[i])
 
}
resultsknn <- cbind(n,acc,err)
colnames(resultsknn) <- c("k","Accuracy","Error")
pdf("kNNaccuracy.pdf")
plot(1:100,acc,xlab="k",ylab="Accuracy")
dev.off()
pdf("kNNerror.pdf")
plot(1:100,err,xlab="k",ylab="Error")
dev.off()
write.table(resultsknn, file = "ResultsDigitKNN.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#iris data
data(iris)
#easy way to split into training set and test set into 70% training
#30% test 
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]
for (i in 1:100) {
  n[i]=i
  knn3 <- kNN(Species~., trainIris,testIris,k=i)
  # The resulting confusion matrix
  t <- table(testIris[,'Species'],knn3)
  #get diagonal elements of matrix
  td <- diag(t)
  #sum all the elements of the matrix t which gives 
  #the examples tested
  sumt <- sum(t)
  #some the diagonal to see how many we got correct
  sumtd <- sum(td)
  #calculate accuracy
  acc[i] <- sumtd/sumt
  #calculate realtive error
  err[i] <- (sumt-sumtd)/sumt
  #print(acc[i])
  #print(err[i])
  
}
resultsknn <- cbind(n,acc,err) # Combine by columbs
colnames(resultsknn) <- c("k","Accuracy","Error")
pdf("kNNaccuracyIris.pdf")
plot(1:100,acc,xlab="k",ylab="Accuracy")
dev.off()
pdf("kNNerrorIris.pdf")
plot(1:100,err,xlab="k",ylab="Error")
dev.off()
write.table(resultsknn, file = "ResultsIrisKNN.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)

