## Start with cluster algos with iris dataset
## load dataset and rename columns
require(graphics)
require(utils)
library(ggplot2)

iris <- iris

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

pairs(iris,col=iris$Name )
iris_dat <- iris[, -5]

# perform hierarchical clustering
hc <- hclust(dist(iris_dat),"ave")
# show the dendogram as string output
(dend1 <- as.dendrogram(hc))
str(dend1)          # "str()" method
str(dend1, max = 2, last.str =  "'") # only the first two sub-levels
# plot the dendrogram

plot(hc,hang=-2)

# install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
hclust(dist(iris_dat),"ave")


# different types of plots
# fan
plot(as.phylo(hc), type = "fan")

# Unrooted
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

# Cut the dendrogram into 3 clusters
colors = c("red", "blue", "green", "black")
clus3 = cutree(hc, 3)
plot(as.phylo(hc), type = "unrooted", tip.color = colors[clus3],
     label.offset = .5, cex = 0.7)
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5, tip.color = colors[clus3])
hc

# how many does it get wrong
clusGroup <- cutree(hc, k=3)
sum(clusGroup != as.numeric(iris$Species))

