library(phytools)
trees <- list()
births <- vector()
Fractions <- vector()
netdiversification <- vector()
speciationrate <- vector()
Avgbranchlength <- vector()
pbtree
for (i in 1:100) {
  births [i] <- runif(1, 0, 1)
  Fractions [i] <- runif(1, 0, 1)
  trees[[i]] <- pbtree(n=100, b=births [i], d=Fractions [i] * births[i])
  netdiversification[i] <- (births[i] - Fractions[i]*births[i])
  speciationrate[i] <- births[i]
  Avgbranchlength [[i]] <- mean(trees[[i]] $edge.length)
}
?pbtree
pbtree
#Q4:
netdiversification <- (births - Fractions*births)
totaltreetips <- log(sapply(trees, Ntip))
#2 tips
plot(netdiversification, totaltreetips)
Q4plot <- plot(netdiversification, totaltreetips)
line <- lm(totaltreetips ~ netdiversification) 
abline(line)
# As net diversification increases, the log of the total tips also increases 
#Q5: 
plot(speciationrate, Avgbranchlength)
#As speciation rate increases, Avg branch length decreases
#Q6: 
cor(speciationrate, Avgbranchlength)
#Q7: 
trees
trees[73]
Tree <- trees[[73]]
rates <- vector()
traits <- list()
plot(Tree)
for (i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(Tree, sig2=rates[i])
}
#Q8: 
head(traits)
length(traits)
#100
mtraitsK <- sapply(traits, mean)
mtraitsratecor <- cor(mtraitsK, rates)
plot(mtraitsK, rates)
dev.off()
# There is no correlation between the mean of traits and rates
#Q9
vtraits <- sapply(traits, var)
vtraitsratecor <- cor(vtraits, rates)
plot(vtraits, rates)
# there is a positive correlation between the variance of traits and the rates
dev.off()
#Q10 
element1 <- sapply(traits, "[[", 1)
element2 <- sapply(traits, "[[", 2)
traitMat <- cbind(element1, element2)
element1and2cor <- cor(element1, element2)
plot(element1, element2)
dev.off()
# there is a positive correlation between element 1 and element 2 
#the slight correlation is due to relatedness in the phylo tree
# We account for the phylogeny because living organisms can't have independent variables. This is not significant because the elements of the traits are randomly generated and each time there is a positive correlation. This shows that all the traits are positively correlated in some way. Element1 and element2 having a small positive correlation doesn't hold any significance 
#Extra credit 
?phylomorphospace
TreeE <- pbtree(n=100)
X <- fastBM(TreeE, nsim=2)
phylomorphospace(TreeE, X, xlab="element1", ylab="element2")