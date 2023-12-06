distance <- c()

s <- seq(0, 48989083, by = 1)

for (i in s){
  time_c <- 48989083-i
  speed_c <- i
  distance <- c(distance, speed_c*time_c)
}


time_c <- 48989083-s
sum(time_c*s>390110311121360)
