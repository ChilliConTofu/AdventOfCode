data <- read.csv("input3.txt", header = F, sep = "")

data2 <- matrix(nrow = 142, ncol = 142)
data2[1,] <- rep(".", 142)
data2[,1] <- rep(".", 142)
data2[142,] <- rep(".", 142)
data2[,142] <- rep(".", 142)

for (i in 2:141){
data2[i,2:141] <- unlist(strsplit(data[i-1,], NULL))
}


collector <- c()

for (i in 2:141){
  j <- 2
  while (j <=140){
    test_n <- is.na(as.numeric(data2[i,j]))
    if (test_n == F){
      test_next1 <- is.na(as.numeric(data2[i, j+1]))
      test_next2 <- is.na(as.numeric(data2[i, j+2]))
      if ((test_next1 == T) & (test_next2 == T)){
        test1 <- (data2[i-1, j-1]!=".") & is.na(as.numeric(data2[i-1, j-1]))
        test2 <- (data2[i, j-1]!=".") & is.na(as.numeric(data2[i, j-1]))
        test3 <- (data2[i+1, j-1]!=".") & is.na(as.numeric(data2[i+1, j-1]))
        test4 <- (data2[i-1, j]!=".") & is.na(as.numeric(data2[i-1, j]))
        test5 <- (data2[i+1, j]!=".") & is.na(as.numeric(data2[i+1, j]))
        test6 <- (data2[i-1, j+1]!=".") & is.na(as.numeric(data2[i-1, j+1]))
        test7 <- (data2[i, j+1]!=".") & is.na(as.numeric(data2[i, j+1]))
        test8 <- (data2[i+1, j+1]!=".") & is.na(as.numeric(data2[i+1, j+1]))
        test <- test1 | test2 | test3 | test4 | test5 | test6 | test7 | test8
        if (test == T){
          collector <- c(collector, as.numeric(data2[i, j]))
        }
      }
      if((test_next1 == F) & (test_next2 == T)){
        test1 <- (data2[i-1, j-1]!=".") & is.na(as.numeric(data2[i-1, j-1]))
        test2 <- (data2[i, j-1]!=".") & is.na(as.numeric(data2[i, j-1]))
        test3 <- (data2[i+1, j-1]!=".") & is.na(as.numeric(data2[i+1, j-1]))
        test4 <- (data2[i-1, j]!=".") & is.na(as.numeric(data2[i-1, j]))
        test5 <- (data2[i+1, j]!=".") & is.na(as.numeric(data2[i+1, j]))
        test6 <- (data2[i-1, j+1]!=".") & is.na(as.numeric(data2[i-1, j+1]))
        test7 <- (data2[i+1, j+1]!=".") & is.na(as.numeric(data2[i+1, j+1]))
        test8 <- (data2[i-1, j+2]!=".") & is.na(as.numeric(data2[i-1, j+2]))
        test9 <- (data2[i, j+2]!=".") & is.na(as.numeric(data2[i, j+2]))
        test10 <- (data2[i+1, j+2]!=".") & is.na(as.numeric(data2[i+1, j+2]))
        test <- test1 | test2 | test3 | test4 | test5 | test6 | test7 | test8 | test9 | test10
        if (test == T){
          number <- paste(data2[i, j], data2[i, j+1], sep = "")
          number <- as.numeric(number)
          collector <- c(collector, number)
          j <- j + 1
        }
      }
      if((test_next1 == F) & (test_next2 == F)){
        test1 <- (data2[i-1, j-1]!=".") & is.na(as.numeric(data2[i-1, j-1]))
        test2 <- (data2[i, j-1]!=".") & is.na(as.numeric(data2[i, j-1]))
        test3 <- (data2[i+1, j-1]!=".") & is.na(as.numeric(data2[i+1, j-1]))
        test4 <- (data2[i-1, j]!=".") & is.na(as.numeric(data2[i-1, j]))
        test5 <- (data2[i+1, j]!=".") & is.na(as.numeric(data2[i+1, j]))
        test6 <- (data2[i-1, j+1]!=".") & is.na(as.numeric(data2[i-1, j+1]))
        test7 <- (data2[i+1, j+1]!=".") & is.na(as.numeric(data2[i+1, j+1]))
        test8 <- (data2[i-1, j+2]!=".") & is.na(as.numeric(data2[i+1, j+2]))
        test9 <- (data2[i+1, j+2]!=".") & is.na(as.numeric(data2[i+1, j+2]))
        test10 <- (data2[i-1, j+3]!=".") & is.na(as.numeric(data2[i-1, j+3]))
        test11 <- (data2[i, j+3]!=".") & is.na(as.numeric(data2[i, j+3]))
        test12 <- (data2[i+1, j+3]!=".") & is.na(as.numeric(data2[i+1, j+3]))
        test <- test1 | test2 | test3 | test4 | test5 | test6 | test7 | test8 | test9 | test10 | test11 | test12
        if (test == T){
          number <- paste(data2[i, j], data2[i, j+1], data2[i, j+2], sep = "")
          print (number)
          number <- as.numeric(number)
          collector <- c(collector, number)
          j <- j + 2
        }
      }
    }
    j <- j + 1
  }
}


data[1,]

indices <- which(data2 == "*", arr.ind = T)
dim(indices)
df <- matrix(nrow = dim(indices)[1], ncol = 4)

for (i in 1:dim(indices)[1]){
  red <- indices[i,1]
  kol <- indices[i,2]
  df[i, 1] <- paste(data2[red-1, (kol-3):(kol+3)], collapse = "")
  df[i, 2] <- paste(data2[red, (kol-3):(kol-1)], collapse = "")
  df[i, 3] <- paste(data2[red, (kol+1):(kol+3)], collapse = "")
  df[i, 4] <- paste(data2[red+2, (kol-3):(kol+3)], collapse = "")
}


