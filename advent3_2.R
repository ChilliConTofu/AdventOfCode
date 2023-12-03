data <- read.csv("input3.txt", header = F, sep = "")

data2 <- matrix(nrow = 142, ncol = 142)
data2[1,] <- rep(".", 142)
data2[,1] <- rep(".", 142)
data2[142,] <- rep(".", 142)
data2[,142] <- rep(".", 142)

for (i in 2:141){
  data2[i,2:141] <- unlist(strsplit(data[i-1,], NULL))
}

indices <- which(data2 == "*", arr.ind = T)
indices <- cbind(indices, 1, 0)

star_logic <- data2 == "*"
star_logic2 <- data2 == "*"

for (i in 2:141){
  j <- 2
  while (j <=140){
    test_n <- is.na(as.numeric(data2[i,j]))
    if (test_n == F){
      test_next1 <- is.na(as.numeric(data2[i, j+1]))
      test_next2 <- is.na(as.numeric(data2[i, j+2]))
      if ((test_next1 == T) & (test_next2 == T)){
        star_logic[(i-1):(i+1), (j-1):(j+1)] <- star_logic[(i-1):(i+1), (j-1):(j+1)]*as.numeric(data2[i,j])
        star_logic2[(i-1):(i+1), (j-1):(j+1)] <- star_logic2[(i-1):(i+1), (j-1):(j+1)]/2
      }
      if((test_next1 == F) & (test_next2 == T)){
        ind1 <- (i-1):(i+1)
        ind2 <- (j-1):(j+2)
        number <- as.numeric(data2[i, j])*10 + as.numeric(data2[i, j+1])
        star_logic[ind1,ind2] <- star_logic[ind1, ind2]*number
        star_logic2[ind1,ind2] <- star_logic2[ind1,ind2]/2
        j <- j+1
      }
      if((test_next1 == F) & (test_next2 == F)){
        number <- as.numeric(data2[i, j])*100 + as.numeric(data2[i, j+1])*10 + as.numeric(data2[i, j+2])
        star_logic[(i-1):(i+1), (j-1):(j+3)] <- star_logic[(i-1):(i+1), (j-1):(j+3)]*number
        star_logic2[(i-1):(i+1), (j-1):(j+3)] <- star_logic2[(i-1):(i+1), (j-1):(j+3)]/2
        j <- j + 2
      }
    }
    j <- j + 1
  }
}

sum((star_logic2 == 0.25)*(star_logic))
