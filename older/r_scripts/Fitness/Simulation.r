# redoing the fitness simulation... 

setwd("D:/Work/Fitness/")


# K : rivals
# d : disadvantage
# n : number of simulations
n <- 100000
K <- 1:15
D <- seq(0,3, by=0.1)

Output <- expand.grid(k=K, d=D, prob=NA)


for(d in D){
  for (k in K){
    A.win <- vector("numeric", n)
    for (i in 1:n){
      B <- rnorm(k)
      A <- rnorm(1, -d)
      
      A.win[i] <- as.numeric(A > max(B))
    }
    prob.A.win <- sum(A.win) / n
    Output[Output$k==k & Output$d==d, "prob"] <- prob.A.win
  }  
}
####


contourplot(prob ~ k * d, data=Output, cuts=20)
save(Output, file="SimulationOutput.rData")
?contourplot


# new session
setwd("D:/Work/Fitness/")
load("SimulationOutput.rData")

require(lattice)

png("attempts.png", height=800, width=800)

plot((1/prob) ~ d, data=subset(Output, k==1), 
     type="l", ylab="Expected number of attempts", 
     xlab="Fitness disadvantage (Standard deviations)", 
     main="Relationship between relative fitness, number of competitors, \nand probability of getting a job", log="y", lwd=2, ylim=c(1, 200))
lines((1/prob) ~ d, data=subset(Output, k==2), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==3), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==4), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==5), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==6), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==7), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==8), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==8), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==9), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==10), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==11), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==12), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==13), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==14), lty="dashed")
lines((1/prob) ~ d, data=subset(Output, k==15), lwd=2, lty="dashed")

legend("bottomright", 
       legend=c("One competitor", "intermediate numbers (two to 14 competitors)", "15 competitors"), 
       lwd=c(2,1,2), 
       lty=c("solid", "dashed", "dashed"))
 
dev.off()

#####
png("probs.png", height=800, width=800)

plot(prob ~ d, data=subset(Output, k==1), 
     type="l", ylab="Probability of getting a job on any one occasion", 
     xlab="Fitness disadvantage (Standard deviations)", 
     main="Relationship between relative fitness, number of competitors, \nand probability of getting a job", log="y", lwd=2)
lines(prob ~ d, data=subset(Output, k==2), lty="dashed")
lines(prob ~ d, data=subset(Output, k==3), lty="dashed")
lines(prob ~ d, data=subset(Output, k==4), lty="dashed")
lines(prob ~ d, data=subset(Output, k==5), lty="dashed")
lines(prob ~ d, data=subset(Output, k==6), lty="dashed")
lines(prob ~ d, data=subset(Output, k==7), lty="dashed")
lines(prob ~ d, data=subset(Output, k==8), lty="dashed")
lines(prob ~ d, data=subset(Output, k==8), lty="dashed")
lines(prob ~ d, data=subset(Output, k==9), lty="dashed")
lines(prob ~ d, data=subset(Output, k==10), lty="dashed")
lines(prob ~ d, data=subset(Output, k==11), lty="dashed")
lines(prob ~ d, data=subset(Output, k==12), lty="dashed")
lines(prob ~ d, data=subset(Output, k==13), lty="dashed")
lines(prob ~ d, data=subset(Output, k==14), lty="dashed")
lines(prob ~ d, data=subset(Output, k==15), lwd=2, lty="dashed")

legend("bottomright", 
       legend=c("One competitor", "intermediate numbers (two to 14 competitors)", "15 competitors"), 
       lwd=c(2,1,2), 
       lty=c("solid", "dashed", "dashed"))
 
dev.off()

####
png("ContourPlot.png", width=800, height=800)
contourplot(1/prob ~ d * k, data=Output, at=c(2,5,10,25, 50,100,200), xlab="Disadvantage (Standard deviations)", ylab="Number of competitors", main="Contour plot of expected number of attempts required to secure a job")
dev.off()
