setwd("~/Desktop/Evolution/Tasks/Task_02")
dir()
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
head(berenMilk)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
order(Data$age)
beren3 <- beren2[order(beren2$age) ,]
getOption("max.print")
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
beren3
#for hypothesis 1, we aren't given the "amount" beren eats so this is not testable. in hypothesis 2 we are also not given "amounts" so these two are inappropriate. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
#2.36677 oz
#the "value" gives us the literal value of what we are asking for. 
# [Feeds] means we are storing it within an object, in this case within avgMilk. It is important because it allows us to store the feeds as well as see all of the values from 1:322
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
head(avgFeed)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
?par
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
getwd()
source("http://jonsmitchell.com/code/plotFxn02b.R")
# Question 2: The graph is impossible tp interpret because there are 3 axis labels when there are only two axis. The graph representing nap time has no units, so there is no way to interpret it if we really don't know how long the naps were. 
pdf("r02b-cumlativeMilkBytime.pdf", height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#extra credit below
beren4
?tapply
Start <- beren4$start_hour + beren4$start_minute/60
End <- beren4$end_hour + beren4$end_minute/60
duration <- End-Start
plot(beren4$age, duration)
cor.test(beren4$age, duration)