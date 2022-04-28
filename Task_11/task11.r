setwd("~/Desktop/Evolution/Tasks/Task_11")
#part 1
rnorm(100, mean=5, sd=2)
x <- rnorm(100, mean=5, sd=2)
y <- (x * 5) + 2 + runif(100, min=0, max=0.1)
lm(y~x)
#slope= 4.998, y-int= 2.062. They are dependent on restrictions of the mean and variance 
slope <- c()
yint <- c()
x <- c()
y <- c()
z <- c()
for (i in 1:100) 
  {
  x[i] <- rnorm(100, mean=5, sd=2)
  z[i] <- rnorm(1)
  q[i] <- x * z
  y[i] <- (q * 5) + 2 + runif(100, min=0, max=0.1)
  mod <- lm(y~x)
  cf <- coef(mod)
  slope[i] <- cf["x"]
}
plot(z,slope)
# this reveals a stable slope. This slope is non dependent on z 

#part 2
library(dplyr)
library(ggplot2)
doors <- 1:3
sample_doors <- function() { return(sample(doors, size= 1000, replace= TRUE))}
games <- data.frame(prize= sample_doors(), pick=sample_doors())
games$strategy <- factor(ifelse(games$prize == games$pick, 'stay', 'switch'))
monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))  
  return(ifelse(length(remaining)==1,
                remaining,    
                sample(remaining, 1)))
}
games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick), 
         stay = pick, 
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stay', 'switch')))
print(summary(games$strategy) / nrow(games))
qplot(strategy, data = games, fill = strategy, geom = 'bar') +
  xlab('Winning Strategy') +
  ggtitle('Monty Hall Problem Stimulation')

#part 3
install.packages("meme")
library(meme)
u2 <- "https://i.kym-cdn.com/photos/images/original/001/137/686/d09.jpg"
a <- meme(u2, size=1, "evolution of memes")
plot(a)