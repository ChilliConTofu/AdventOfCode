data <- read.csv("input4.txt", header = F)

View(data)

for (i in 1:186){
  data$V1[i] <- gsub(paste ("Card ", " ", as.character(i), ":", sep = ""), "", data$V1[i])
}

for (i in 1:186){
  data$V1[i] <- gsub(paste ("Card ", "  ", as.character(i), ":", sep = ""), "", data$V1[i])
}

data$V1
games <- sub("\\|.*", "", data$V1)

results <- c()

for (i in 1:186){
results <- c(results, gsub(c(games[i]), "", data$V1[i]))
}

sum <- 0
M <- c()
for (i in 1:186){
results_t <- as.numeric(unlist(strsplit(results[i], " "))[-1])
games_t <- as.numeric(unlist(strsplit(games[i], " ")))
results_t <- results_t[is.na(results_t)==F]
games_t <- games_t[is.na(games_t)==F]

matches <- 0
for (j in 1:length(results_t)){
  matches <- matches + sum(games_t == results_t[j])
}
M <- c(M, matches)
}

suma <- 0
M
for (i in 1:186){
  if (M[i]>=1){
    suma <- suma + 2^(M[i]-1)
  }
}

suma


for (j in 1:186){
  
}