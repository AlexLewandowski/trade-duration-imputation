library(moments)
############################ Constructs Duration From Source Data ######################################
Datum <- read.csv("training.csv",stringsAsFactors = FALSE) len <-
length(Datum[,2]) newDat <- numeric(len)

for(i in 1:len){ newDat[i] <- 3600*as.numeric(substr(Datum[,2][i],10,11)) +
60*as.numeric(substr(Datum[,2][i],13,14)) +
as.numeric(substr(Datum[,2][i],16,200)) }

Diffd <- diff(newDat) Diffd <- sapply(Diffd, function(x) { if(x < 0){ 3600*24
+ x} else{x} })

shorterDat <- newDat[2:len] shorterBid <- Datum[,3][2:len] shorterAsk <-
Datum[,4][2:len] Data <- data.frame(shorterDat, Diffd,shorterBid,shorterAsk)
colnames(Data) <- c("Times","Durations","Bid","Ask")


Datum <- read.csv("training2.csv",stringsAsFactors = FALSE) len <-
length(Datum[,2]) newDat <- numeric(len)

for(i in 1:len){ newDat[i] <- 3600*as.numeric(substr(Datum[,2][i],10,11)) +
60*as.numeric(substr(Datum[,2][i],13,14)) +
as.numeric(substr(Datum[,2][i],16,200)) }

Diffd <- diff(newDat) Diffd <- sapply(Diffd, function(x) { if(x < 0){ 3600*24
+ x} else{x} })

shorterDat <- newDat[2:len] shorterBid <- Datum[,3][2:len] shorterAsk <-
Datum[,4][2:len] Data2 <- data.frame(shorterDat, Diffd,shorterBid,shorterAsk)
colnames(Data2) <- c("Times","Durations","Bid","Ask")

################################################################################################################################################################


Dur01 <- which(Data$Durations == 0)
DurNot1 <- which(Data$Durations != 0)

Dur02 <- which(Data2$Durations == 0)
DurNot2 <- which(Data2$Durations != 0)

Dur1 <- Data$Durations
Dur2 <- Data2$Durations

Den1 <- density(Dur1,from=10^(-10),to=0.1,adjust=1,n=1000000)
Den2 <- density(Dur2,from=10^(-10),to=0.1,adjust=1,n=1000000)
scale1 <- 0.001/max(Den1$x)
scale2 <- 0.001/max(Den2$x)



m = 20
augmean1 <- c(1:m)
augvar1 <- c(1:m)
augskew1 <- c(1:m)
augkur1 <- c(1:m)
augmean2 <- c(1:m)
augvar2 <- c(1:m)
augskew2 <- c(1:m)
augkur2 <- c(1:m)
log1 <- c(1:m)
log2 <- c(1:m)
ratio1 <- c(1:m)
ratio2 <- c(1:m)
for(i in 1:m){
  samp1 <- scale1*sample(x = Den1$x, size = length(Dur01), prob=Den1$y/sum(Den1$y), repl = TRUE)
  samp2 <- scale2*sample(x = Den2$x, size = length(Dur02), prob=Den2$y/sum(Den2$y), repl = TRUE)
  
  AugDur1 <- c(1:length(Dur1))
  AugDur1[Dur01] <- samp1
  AugDur1[DurNot1] <- Dur1[DurNot1]
  
  AugDur2 <- c(1:length(Dur2))
  AugDur2[Dur02] <- samp2
  AugDur2[DurNot2] <- Dur2[DurNot2]
  
  augmean1[i] <- mean(AugDur1)
  augvar1[i] <- var(AugDur1)
  augskew1[i] <- skewness(AugDur1)
  augkur1[i] <- kurtosis(AugDur1)
  
  augmean2[i] <- mean(AugDur2)
  augvar2[i] <- var(AugDur2)
  augskew2[i] <- skewness(AugDur2)
  augkur2[i] <- kurtosis(AugDur2)
  
  log1[i] <- mean(log(AugDur1))
  log2[i] <- mean(log(AugDur2))
  ratio1[i] <- mean(1/(AugDur1))
  ratio2[i] <- mean(1/(AugDur2))
}

mean(augmean1)
mean(augvar1)
mean(augskew1)
mean(augkur1)

mean(augmean2)
mean(augvar2)
mean(augskew2)
mean(augkur2)

mean(log1)
var(log1)
mean(log2)
var(log2)
mean(ratio1)
var(ratio1)
mean(ratio2)
var(ratio2)

mean(Dur1)
var(Dur1)
skewness(Dur1)
kurtosis(Dur1)

mean(Dur2)
var(Dur2)
skewness(Dur2)
kurtosis(Dur2)

filtered1 <- Dur1[Dur1 < 2000]
filtered2 <- Dur2[Dur2 < 2000]

ts.plot(filtered1)
ts.plot(filtered2)

plot(scale1*Den1$x,Den1$y,xlab="",ylab = "density")
plot(scale2*Den2$x,Den2$y,xlab="",ylab = "density")
