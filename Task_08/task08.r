library(phytools)
library(ape)
library(phytools)
text.string<-
  "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#Question 1: the shark is more closely related to the goldfish than humans
vert.tree
#Question 2: no, there are no branch lengths
str(vert.tree)
tree <- read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths) [which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
#Question 3: plot a tree with no tip labels 
plot(AnolisTree, cex=0.25, show.tip.label = FALSE)
#Question 4: plot a tree that is plotted as a circle
plot(AnolisTree, type="radial", cex=0.25)
#Question 5: plot a tree with the tips colored red
plot(AnolisTree, cex=0.25, tip.color= "red")
#Question 6-8: shortest edge length, drop tip, plot
which.min(AnolisTree$edge.length)
AnolisTree2 <- drop.tip(AnolisTree, 82)
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#Question 9: The line increases. It does not decrease because none of the past lineages have gone extinct thus far. The slope varies depending on the diversity. When the line goes flat for a little bit that represents a steady diversity and then when a new event is introduced it begins to increase again
#Question 10: use function fit.bd and set rho=0.2 to calculate the rate species form and disappear
fit.bd(AnolisTree, rho=0.2)
#ML(b/lambda)= 0.8031 
#ML(d/mu)= 0