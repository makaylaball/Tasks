setwd("~/Desktop/Evolution/Tasks/Project2/Data")
data1 <- read.csv("individual_collection_data.csv")
data2 <- read.csv("individual_field_data.csv")
library(phytools)
treeb <- read.nexus("butterfly_tree.nex")
plot(treeb)
pdf("Phylogenetic Tree")

#pearsons correlation: 
cor(data1$N.family.level, data1$avg.eye.width, use="complete")
cor(data1$N.family.level, data1$Wing.Length, use="complete")

Drops <- setdiff(treeb$tip.label, data1$Species)
treec <- drop.tip(treeb, Drops)

Eye <- tapply(data1$avg.eye.width, data1$Species, mean, na.rm=T)
Eye <- Eye[-which(names(Eye) == "Phyciodes_graphica")]
tree_eye <- drop.tip(treec, "Phyciodes_graphica")
pANOVA_Eye <- phylANOVA(tree_eye, names(Eye), Eye[tree_eye$tip.label])

Nitro <- tapply(data1$N.family.level, data1$Species, mean, na.rm=T)
Nitro <- Nitro[-which(names(Nitro)=="Feniseca_tarquinius")]
tree_nitro <- drop.tip(treec, "Feniseca_tarquinius")
pANOVA_Nitro <- phylANOVA(tree_nitro, names(Nitro), Nitro[tree_nitro$tip.label])

Wing <- tapply(data1$Wing.Length, data1$Species, mean, na.rm=T)
pANOVA_Wing <- phylANOVA(treec, names(Wing), Wing[treec$tip.label])

RStudio.Version()

summary(treeb)
summary(treec)

#x, y scatterplot for nitrogen and eye width
Model <- lm(Eye~Nitro)
plot(Nitro, Eye)
abline(Model)
legend("topleft", legend=paste("R2=", format(summary(Model)$r.squared)))
plot(x=Nitro, y=Eye, xlab="Nitrogen", ylab="Eye Width", main='Nitrogen vs Eye Width')
summary(Model)

#scatterplot for nitrogen and wing length
plot(Nitro, Wing)
Model <- lm(Wing~Nitro)
abline(Model)
plot(x=Nitro, y=Wing, xlab="Nitrogen", ylab="Wing Length", main='Nitrogen vs Wing Length')
legend("topleft", legend=paste("R2=", format(summary(Model)$r.squared)))
summary(Model)

