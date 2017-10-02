

PB <- read.csv("C:/Users/Miles/Documents/UIUC/Fall 2017 Classes/Quant 1/Quant 1 Homework/Week 5 Homework/PB.csv")
PB$row <- 1:nrow(PB)
PB2 <- within(PB[2:nrow(PB),], powN<-data.frame(do.call('rbind', strsplit(as.character(as.character(Draw.Date...WB1.WB2.WB3.WB4.WB5.PB..PP)), "  ", fixed=TRUE))))
PB2 <- PB2[,-1]
PB2 <- PB2$powN[,-8]
colnames(PB2) <- c("Date","WB1","WB2","WB3","WB4","WB5","PB")
head(PB2)
tail(PB2)
PB2$Date <- as.Date(as.character(PB2$Date),"%m/%d/%Y")
PB2$Era <- NA
PB2$Era[PB2$Date<="2002-10-09"] <- "1"
PB2$Era[PB2$Date>"2002-10-09"] <- "2"
PB2$Era[PB2$Date>"2005-08-31"] <- "3"
PB2$Era[PB2$Date>"2009-01-07"] <- "4"
PB2$Era[PB2$Date>"2012-01-11"] <- "5"
PB2$Era[PB2$Date>"2015-10-07"] <- "6"
PowBal <- PB2 # Rename data to avoid unintential product placement.
Ns <- summary(as.factor(PowBal$Era))
E1 <- matrix(0,nrow=Ns[1],ncol=5,byrow=T)
E2 <- matrix(0,nrow=Ns[2],ncol=5,byrow=T)
E3 <- matrix(0,nrow=Ns[3],ncol=5,byrow=T)
E4 <- matrix(0,nrow=Ns[4],ncol=5,byrow=T)
E5 <- matrix(0,nrow=Ns[5],ncol=5,byrow=T)
E6 <- matrix(0,nrow=Ns[6],ncol=5,byrow=T)
for(i in 1:Ns[1]){
  E1[i,] <- sample(1:49,5,replace=F)
}
for(i in 1:Ns[2]){
  E2[i,] <- sample(1:53,5,replace=F)
}
for(i in 1:Ns[3]){
  E3[i,] <- sample(1:55,5,replace=F)
}
for(i in 1:Ns[4]){
  E4[i,] <- sample(1:59,5,replace=F)
}
for(i in 1:Ns[5]){
  E5[i,] <- sample(1:59,5,replace=F)
}
for(i in 1:Ns[6]){
  E6[i,] <- sample(1:69,5,replace=F)
}
P1 <- c()
P2 <- c()
P3 <- c()
P4 <- c()
P5 <- c()
P6 <- c()
for(i in 1:Ns[1]){
  P1[i] <- sample(1:42,1,replace=F)
}
for(i in 1:Ns[2]){
  P2[i] <- sample(1:42,1,replace=F)
}
for(i in 1:Ns[3]){
  P3[i] <- sample(1:42,1,replace=F)
}
for(i in 1:Ns[4]){
  P4[i] <- sample(1:35,1,replace=F)
}
for(i in 1:Ns[5]){
  P5[i] <- sample(1:39,1,replace=F)
}
for(i in 1:Ns[6]){
  P6[i] <- sample(1:26,1,replace=F)
}

# Era 1
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="1"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/49 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=49)
  abline(h=0.02040816,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="1"),"PB"]),freq=F,
     main="Power Ball (5/49 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=0.02380952,col="red")
for(i in 1:5){
  hist(E1[,i],freq=F,main=paste("Simulation",i,"(5/49 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=49)
  abline(h=0.02040816,col="red")
}
hist(P1,freq=F,main="Simulation (5/49 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=0.02380952,col="red")


# Era 2
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="2"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/53 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=53)
  abline(h=1/53,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="2"),"PB"]),freq=F,
     main="Power Ball (5/53 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=1/42,col="red")
for(i in 1:5){
  hist(E2[,i],freq=F,main=paste("Simulation",i,"(5/53 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=53)
  abline(h=1/53,col="red")
}
hist(P2,freq=F,main="Simulation (5/53 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=1/42,col="red")


# Era 3
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="3"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/55 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=55)
  abline(h=1/55,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="3"),"PB"]),freq=F,
     main="Power Ball (5/55 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=1/42,col="red")
for(i in 1:5){
  hist(E3[,i],freq=F,main=paste("Simulation",i,"(5/55 + 1/42)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=55)
  abline(h=1/55,col="red")
}
hist(P3,freq=F,main="Simulation (5/55 + 1/42)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=42)
abline(h=1/42,col="red")


# Era 4
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="4"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/55 + 1/35)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=55)
  abline(h=1/55,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="4"),"PB"]),freq=F,
     main="Power Ball (5/55 + 1/35)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=35)
abline(h=1/35,col="red")
for(i in 1:5){
  hist(E4[,i],freq=F,main=paste("Simulation",i,"(5/55 + 1/35)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=55)
  abline(h=1/55,col="red")
}
hist(P4,freq=F,main="Simulation (5/55 + 1/35)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=35)
abline(h=1/35,col="red")


# Era 5
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="5"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/59 + 1/39)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=59)
  abline(h=1/59,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="5"),"PB"]),freq=F,
     main="Power Ball (5/59 + 1/39)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=39)
abline(h=1/39,col="red")
for(i in 1:5){
  hist(E5[,i],freq=F,main=paste("Simulation",i,"(5/59 + 1/39)"),
       xlab="",ylab="Probability",ylim=c(0,.06),breaks=59)
  abline(h=1/59,col="red")
}
hist(P5,freq=F,main="Simulation (5/59 + 1/39)",
     xlab="",ylab="Probability",ylim=c(0,.06),col.main="red",
     breaks=39)
abline(h=1/39,col="red")



# Era 6
par(mfrow=c(2,6),family="serif")
for(i in 2:6){
  hist(as.numeric(PowBal[which(PowBal$Era=="6"),i]),freq=F,
       main=paste("White Ball",i-1,"(5/69 + 1/26)"),
       xlab="",ylab="Probability",ylim=c(0,.08),breaks=69)
  abline(h=1/69,col="red")
}
hist(as.numeric(PowBal[which(PowBal$Era=="6"),"PB"]),freq=F,
     main="Power Ball (5/69 + 1/26)",
     xlab="",ylab="Probability",ylim=c(0,.08),col.main="red",
     breaks=26)
abline(h=1/26,col="red")
for(i in 1:5){
  hist(E6[,i],freq=F,main=paste("Simulation",i,"(5/69 + 1/26)"),
       xlab="",ylab="Probability",ylim=c(0,.08),breaks=59)
  abline(h=1/69,col="red")
}
hist(P6,freq=F,main="Simulation (5/69 + 1/26)",
     xlab="",ylab="Probability",ylim=c(0,.08),col.main="red",
     breaks=26)
abline(h=1/26,col="red")
