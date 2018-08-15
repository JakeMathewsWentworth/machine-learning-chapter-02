setwd("~/wentworth/machine-learning/chapter_2")
library(tidyverse)
library(nnet)
insect <- read.csv("insects.csv")
c <- insect$c
n = length(insect$c)
cc <- integer()
for (i in 1:n) {
  if (c[i] == "g") {
    cc[i] = 1
  } else{
    cc[i] = 2
  }
}
x <- insect$x
y <- insect$y
dataint <- data.frame(cc, x, y)
colnames(dataint) <- c("c", "abd", "ant")
#print(dataint)
#stop()
write.table(
  dataint,
  file = "insectclass_number.csv",
  append = F,
  quote = F,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = F,
  col.names = T
)
insect <- read.csv("insectclass_number.csv")
print(insect)
insect <- insect[order(insect$c), ]
#col <- c("blue","red")
#col <- col[as.numeric(insect$c)]
#pdf('dotplotinsect.pdf')
#dotchart(insect$abd,main="Abdomen Length and Class",
#         xlab="Abdomen Length",color=col)
#dev.off()
#stop()
#define two vectors
#x=c(1,2,3)
#y=c(2,4,1)
#nx=length(x)
#ny=length(y)
edistance <- function(x, y, n) {
  d = 0
  for (i in 1:n) {
    d = d + (x[i] - y[i]) ^ 2
  }
  d = sqrt(d)
  return(d)
}
#d=edistance(x,y,nx)
#print(d)
#make test set by randomly picking 10 values
x <- insect$abd
y <- insect$c
n <- length(x)
insectabd <- data.frame(y, x)
colnames(insectabd) <- c("c", "abd")
xx = sample(1:n, testSize, replace = F)
xx = sort(xx)
xt <- numeric()
yt <- numeric()
for (i in 1:testSize) {
  j = xx[i]
  xt[i] = insectabd$abd[j]
  yt[i] = insectabd$c[j]
}
insectest <- data.frame(yt, xt)
colnames(insectest) <- c("c", "abd")
for (i in testSize:1) {
  j = xx[i]
  insectabd <- insectabd[-j, ]
}
nt <- length(insectest$abd)
n <- length(insectabd$abd)
insectest_no_class <- insectest[, -1]
insectabd_no_class <- insectabd[, -1]
#5-NN
kk <- as.integer(readline(prompt = paste("How many Neighbors ")))
testSize = as.integer(readline(prompt = paste("How many tests ")))
#kk=5
d1 <- numeric()
cc <- integer()
c5 <- integer()
for (i in 1:nt) {
  # for (i in 1:1){
  xt <- insectest_no_class[i]
  nn <- length(xt)
  for (j in 1:n) {
    x <- insectabd_no_class[j]
    d1[j] = edistance(xt, x, nn)
  }
  results <- data.frame(d1, insectabd$c)
  #print(results)
  colnames(results) <- c("d1", "c")
  results <- results[order(d1), ]
  #cc[i] <- results$c[1]
  c5 <- results$c[1:kk]
  sum1 = 0
  sum2 = 0
  #print(results)
  for (k in 1:kk) {
    if (c5[k] == 1) {
      sum1 = sum1 + 1
    } else {
      sum2 = sum2 + 1
    }
  }
  print(sum1)
  print(sum2)
  sumall <- cbind(sum1, sum2)
  nm <- which(sumall == max(sumall))
  nm2 <- length(nm)
  cc[i] <- which.is.max(sumall)
}
allresults <- data.frame(cc, insectest$c)
colnames(allresults) <- c("predict", "actual")
write.table(
  allresults,
  file = "resultsinsect_1NN.csv",
  append = F,
  quote = F,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = F,
  col.names = T
)

#confusion table Accuracy
c1 <- allresults$c1
c2 <- allresults$c2
#print(c1)
#print(c2)
n4 = length(allresults$actual)
TN = 0
TP = 0
FN = 0
FP = 0
for (i in 1:n4) {
  c1[i] <- allresults$actual[i]
  c2[i] <- allresults$predict[i]
  print(c1[i])
  print(c2[i])
  if (c1[i] == 1) {
    if (c1[i] == c2[i]) {
      TN = TN + 1
    } else{
      FN = FN + 1
    }
  } else{
    if (c1[i] == c2[i]) {
      TP = TP + 1
    } else{
      FP = FP + 1
    }
  }
}
#calculate overall accuracy
OA = (TN + TP) / n4
#calculate acc. negative samples
AN = TN / (TN + FN)
if ((TN + FN) == 0) {
  AN = 0
}

#calculate acc. positive samples
AP = TP / (TP + FP)
if ((TP + FP) == 0) {
  AP = 0
}
#print(AP)
accresults <- data.frame(OA, AN, AP)
colnames(accresults) <- c("Overall", "Negative", "Positive")
write.table(
  accresults,
  file = "accintsects.csv",
  append = F,
  quote = F,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = F,
  col.names = T
)
print(accresults)