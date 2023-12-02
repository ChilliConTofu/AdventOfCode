data <- read.csv("input (1).txt", header = F, sep = ";")

#View(data)
data$V1 <- gsub(" green", "G", data$V1)
data$V1 <- gsub(" red", "R", data$V1)
data$V1 <- gsub(" blue", "B", data$V1)

data$V2 <- gsub(" green", "G", data$V2)
data$V2 <- gsub(" red", "R", data$V2)
data$V2 <- gsub(" blue", "B", data$V2)

data$V3 <- gsub(" green", "G", data$V3)
data$V3 <- gsub(" red", "R", data$V3)
data$V3 <- gsub(" blue", "B", data$V3)

data$V4 <- gsub(" green", "G", data$V4)
data$V4 <- gsub(" red", "R", data$V4)
data$V4 <- gsub(" blue", "B", data$V4)

data$V5 <- gsub(" green", "G", data$V5)
data$V5 <- gsub(" red", "R", data$V5)
data$V5 <- gsub(" blue", "B", data$V5)

data$V6 <- gsub(" green", "G", data$V6)
data$V6 <- gsub(" red", "R", data$V6)
data$V6 <- gsub(" blue", "B", data$V6)

for (i in 1:100){
  data$V1[i] <- gsub(paste ("Game ", as.character(i), ":", sep = ""), "", data$V1[i])
}

suma <- 0

for (i in 1:100){
temp <- unlist(strsplit(t(data[i,]), split = ", "))

g_max <- max(as.numeric(gsub("G", "", temp)), na.rm = T)
r_max <- max(as.numeric(gsub("R", "", temp)), na.rm = T)
b_max <- max(as.numeric(gsub("B", "", temp)), na.rm = T)

if ((g_max<=13) & (r_max <=12) & (b_max<=14)){
  suma <- suma + i
}
}



suma <- 0

for (i in 1:100){
  temp <- unlist(strsplit(t(data[i,]), split = ", "))
  
  g_max <- max(as.numeric(gsub("G", "", temp)), na.rm = T)
  r_max <- max(as.numeric(gsub("R", "", temp)), na.rm = T)
  b_max <- max(as.numeric(gsub("B", "", temp)), na.rm = T)
  prod <- g_max * r_max * b_max
  suma <- suma + prod
}
suma
