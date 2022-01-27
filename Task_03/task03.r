setwd("~/Desktop/Evolution/Tasks/Task_03")
trueMean1 <- 5
trueSD1 <- 5
population1 <-rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population1, Size)
boxplot(Sample1, Sample2)
#Yes, the two samples and populations are different
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
nrow(MatGrandma)
ncol(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
nrow(MatGrandpa)
ncol(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
head(Focus)
# The number should be 0.5 because Brenda will share 50% of her genes with focus
ToMom <- length(grep("mom", Focus)) /length(Focus)
# The number of genes focus shares with each maternal grandparent should be 0.25
ToMomMom <- length(grep("grandma_mom", Focus)) /length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) /length(Focus)
# My prediction was incorrect, Focus's grandma on moms side shared 0.444 while the grandpa on the moms side shared 0.0556
ToDadMom <- length(grep("grandma_da", Focus)) /length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus)) /length(Focus)
#Focus is not equally related to the maternal grandparents or the paternal grandparents due to different numbers of genes being shared. This is not what I expected, I expected the numbers to be equal within the grandma and grandpa. The average relatedness is 0.25 (25%)
Sibling_01 <- makeBaby(Brenda, Alan)
# I expect focus to share 0.5 (half) of his DNA with his sibling but they actually ended up sharing 0.6052 of their DNA
ToSib <- length(intersect(Focus, Sibling_01)) /length(Focus)
# Focus would share about half (0.5) of his genes with each sibling
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) /length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
# Every time Alan and Brenda make an offspring, the genes are going to differ slightly. This explains why that although we see a lot of them sharing around 0.5 of their genes, it can be more or less. 
HWE <- function(p)  {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
# The frequency of aa increases as the a allele increases, and as a decreases so does the frequency of aa. Both time and geographic space are not shown on this graph.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
??lty
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
# Yes?
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
# Points have been added to the lower aa frequency line and are overall more spread out, the reason for the change is due to the change in sample size
library(learnPopGen)
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=100, nrep=5, pause=0.01)
x <- genetic.drift(Ne=50, nrep=5, pause=0.01)
x <- genetic.drift(Ne=5, nrep=5, pause=0.01)
x <- genetic.drift(Ne=25, nrep=5, pause=0.01)
x <- genetic.drift(Ne=500, nrep=5, pause=0.01)
# As you increase the sample size, the fequences begin to straighten out and very less, but as you decrease the sample size, the frequences begin to jump higher and look more jagged.
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples + 0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
#The first line is slightly higher than Line2 when the sample is smaller. +0 indicates there is no intercept and that there is only samples shown. As the population size increases the points become more scattered. 
install.packages("MASS")
library(MASS)
LineA <- rlm(tExt ~ Samples)
abline(LineA)
LineA$coef
summary(LineA)
# The slope of the line decreased 