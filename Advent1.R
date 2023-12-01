
f <- function(){
data <- read.csv("input.txt", header = F)

data1 <- gsub("one", "1", data$V1)
data2 <- gsub("two", "2", data$V1)
data3 <- gsub("three", "3", data$V1)
data4 <- gsub("four", "4", data$V1)
data5 <- gsub("five", "5", data$V1)
data6 <- gsub("six", "6", data$V1)
data7 <- gsub("seven", "7", data$V1)
data8 <- gsub("eight", "8", data$V1)
data9 <- gsub("nine", "9", data$V1)

one_pre <- nchar(((sub("1.*", "", data1))))
two_pre <- nchar(((sub("2.*", "", data2))))
three_pre <- nchar(((sub("3.*", "", data3))))
four_pre <- nchar(((sub("4.*", "", data4))))
five_pre <- nchar(((sub("5.*", "", data5))))
six_pre <- nchar(((sub("6.*", "", data6))))
seven_pre <- nchar(((sub("7.*", "", data7))))
eight_pre <- nchar(((sub("8.*", "", data8))))
nine_pre <- nchar(((sub("9.*", "", data9))))

for (i in 1:1000){
data1[i] <- paste(rev(strsplit(data1[i], "")[[1]]), collapse = "")
data2[i] <- paste(rev(strsplit(data2[i], "")[[1]]), collapse = "")
data3[i] <- paste(rev(strsplit(data3[i], "")[[1]]), collapse = "")
data4[i] <- paste(rev(strsplit(data4[i], "")[[1]]), collapse = "")
data5[i] <- paste(rev(strsplit(data5[i], "")[[1]]), collapse = "")
data6[i] <- paste(rev(strsplit(data6[i], "")[[1]]), collapse = "")
data7[i] <- paste(rev(strsplit(data7[i], "")[[1]]), collapse = "")
data8[i] <- paste(rev(strsplit(data8[i], "")[[1]]), collapse = "")
data9[i] <- paste(rev(strsplit(data9[i], "")[[1]]), collapse = "")
}


one_post <- nchar(((sub("1.*", "", data1))))
two_post <- nchar(((sub("2.*", "", data2))))
three_post <- nchar(((sub("3.*", "", data3))))
four_post <- nchar(((sub("4.*", "", data4))))
five_post <- nchar(((sub("5.*", "", data5))))
six_post <- nchar(((sub("6.*", "", data6))))
seven_post <- nchar(((sub("7.*", "", data7))))
eight_post <- nchar(((sub("8.*", "", data8))))
nine_post <- nchar(((sub("9.*", "", data9))))

PRE <- cbind(one_pre, two_pre, three_pre, four_pre, five_pre, six_pre, seven_pre, eight_pre, nine_pre)
POST <- cbind(one_post, two_post, three_post, four_post, five_post, six_post, seven_post, eight_post, nine_post)



sum(apply(PRE, 1, FUN = which.min)*10 + apply(POST, 1, FUN = which.min))
}

system.time(f())