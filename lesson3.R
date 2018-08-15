# Intizialization
library(tidyverse)
setwd("~/wentworth/machine-learning/chapter_2")

# Change class to integer
insect <- read.csv("insects.csv")
c <- insect$c
n = length(insect$c)
cc <- integer()
for (i in 1:n) {
  if (c[i] == 'g') {
    cc[i] = 1
  } else {
    cc[i] = 2
  }
}
x <- insect$x
y <- insect$y

# Reload insect data
dataint <- data.frame(cc, x, y)
colnames(dataint) <- c('class', 'abdomen', 'antenna')
insectclass_filename = 'insectclass_number.csv'
write.table(
  dataint,
  file = insectclass_filename,
  append = FALSE,
  quote = FALSE,
  sep = ',',
  eol = '\n',
  na = "NA",
  dec = '.',
  row.names = FALSE,
  col.names = TRUE
)
insect <- read.csv(insectclass_filename)
insect <- insect[order(insect$class), ]

# Plot points
# color <- c('blue', 'red')
# color <- color[as.numeric(insect$c)]
# pdf('dotplotinsect.pdf')
# dotchart(insect$abdomen,
#          main = 'Abdomen Length and Class',
#          xlab = 'Abdomen Length',
#          color = color)
# dev.off()

#### EXAMPLE ###
# Define two vectors
# x = c(1, 2, 3)
# y = c(2, 4, 1)

# Compute distance
# length = length(x)
# if (length != length(y)) {
#   print.warnings("X and Y not same length")
#   stop()
# }
edistance <- function(x, y, n) {
  d = 0
  for (i in 1:n) {
    d = d + (x[i] - y[i]) ^ 2
  }
  d = sqrt(d)
  return(d)
}
# distance = edistance(x, y, length)
#### END EXAMPLE ####


# Make tests set by randomly picking 10 values
x <- insect$abdomen
y <- insect$class
n <- length(x)
sampleSize = 10
insectAbdomen <- data.frame(y, x)
colnames(insectAbdomen) <- c("class", "abdomen")
sampleIndex = sample(1:n, 10, replace = FALSE)
sampleIndex <- sort(sampleIndex)
xTest <- numeric()
yTest <- numeric()
for (i in 1:sampleSize) {
  j = sampleIndex[i]
  xTest[i] = insectAbdomen$abdomen[j]
  yTest[i] = insectAbdomen$class[j]
}
insectTest <- data.frame(yTest, xTest)
colnames(insectTest) <- c("class", "abdomen")
for (i in sampleSize:1) {
  j = sampleIndex[i]
  insectAbdomen <- insectAbdomen[-j,]
}

#############
insectTestLength = length(insectTest$class)
insectAbdomenLength = length(insectAbdomen$class)
insectTest_noClass = insectTest[,-1]
insectAbdomen_noClass = insectAbdomen[,-1]

#1-NN (First nearest neighbor)
distances <- numeric()
output_cc <- integer()
# for (i in 1:insectTestLength) {
for (i in 1:1) {
  xTest <- insectTest_noClass[i]
  xTestLength <- length(xTest)
  for (j in 1:insectAbdomenLength) {
    xAbdomen <- insectAbdomen_noClass[j]
    distances[j] = edistance(xTest, xAbdomen, xTestLength)
  }
  results <- data.frame(distances, insectAbdomen$class)
  colnames(results) <- c("distances", "class")
  results <- results[order(distances), ]
  output_cc[i] <- results$class[1]
}
allResults <- data.frame(output_cc, insectTest$class)
write.table(
  allResults,
  file = "resultsInsect_1NN.csv",
  sep = ",",
  append = FALSE,
  quote = FALSE,
  eol = '\n',
  na = "NA",
  dec = '.',
  row.names = FALSE,
  col.names = TRUE
)
