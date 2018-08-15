#kNN with package class need Matrix and DMwR
library(class)
library(caret)
library(ggplot2)
library(Matrix)
library(DMwR)
library(lattice)
#library(TTR)
#function that plots decision boundary by evaluating the classifier at evenly 
#spaced grid points
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
#iris data
data(iris)
#easy way to split into training set and test set into 70% training
#30% test 
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]
#lets do 3NN with package
# 3-nearest neighbours with no normalization
#knn3 <- knn(Species~., trainIris, testIris, k=3)
knn3 <- knn(Species~., trainIris,testIris,k=3)
# The resulting confusion matrix
t3 <- table(testIris[,'Species'],knn3)
#print(t3)
# 5-nearest neighbours model with normalization
knn5 <- kNN(Species ~ .,trainIris, testIris, norm=TRUE, k=5)
# The resulting confusion matrix
t5 <- table(testIris[,'Species'],knn5)
#print(t5)
#another knn package using caret
# Three classes and take rows 1:150 and columns labeled sepal.length
# sepal.width and species
x <- iris[1:150, c("Sepal.Length", "Sepal.Width", "Species")]
acc <- numeric()
err <- numeric()
n <- integer()
#up to how many neighbors do we want to consider
kk=20
for (i in 1:kk){
n[i]=i  
nn3 <- knn3(Species ~ ., data=x, k = i)
name1="kNNboundaries"
num=i
ext=".pdf"
name2=paste(name1,i,ext,sep='')
pdf(file=name2)
decisionplot(nn3, x, class = "Species", main = paste("kNN ",i))
dev.off()
}
#cross validation and fitting model
knn_fit <- train(Species~.,data=trainIris,
                 trControl=trainControl(method='cv',number=5),
                 preProcess=c("center","scale"),method="knn")
print(knn_fit)
pdf("knn_fit.pdf")
plot(knn_fit)
dev.off()
knn_pred <- predict(knn_fit,newdata=testIris)
#confusion matrix
#first ones gives much more info.
#t <- confusionMatrix(knn_pred,testIris$Species)
t <- table(testIris[,'Species'],knn_pred)
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
resultsknn <- cbind(n,acc,err)
colnames(resultsknn) <- c("k","Accuracy","Error")
pdf("kNNaccuracyIris.pdf")
plot(1:kk,acc,xlab="k",ylab="Accuracy")
dev.off()
pdf("kNNerrorIris.pdf")
plot(1:kk,err,xlab="k",ylab="Error")
dev.off()
write.table(resultsknn, file = "ResultsIrisKNN.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)