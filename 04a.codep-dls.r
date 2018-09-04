library(codep)
library(vegan)
source("R/custom.plot.cdp.r")

load("clean data/int.sc.2018.rda")

ts <- decostand(cts, method = "hellinger")
h <- decostand(ch, method = "hellinger")
b <- decostand(cb, method = "hellinger")

ts <- as.matrix(ts)
h <- as.matrix(h)
b <- as.matrix(b)

#------------------------------------------#

# Generate an object with spatial variables
map <- eigenmap(x = 1:nrow(b), weighting = Wf.binary, boundaries = c(0,1))
#plot(map)

#------------------------------------------#

# Co-dependence analysis of relation between beetle community and woody species
mca.ts <- MCA(Y = b, X = ts, emobj = map)
mca.ts.partest <- test.cdp(mca.ts)
summary(mca.ts.partest)

ind <- colSums(ts > 0) > 9

png("figs/Fig7a.png", width = 800, height = 350)
par(mar = c(2,14,.5,3.5), cex = 1.5)
plot.cdp(mca.ts.partest, las = 2, col = rev(terrain.colors(256)), ind = ind)
dev.off()

## Nonparametric testing, similar results, but much longer computation
#mca.ts.pertest <- permute.cdp(mca.ts)
#summary(mca.ts.pertest)
#plot(mca.ts.pertest, las = 2, xlab = "dd")

#mca.ts.pertest$UpYXcb$C[1:10,1L,]

res.ts <- data.frame(mem = names(mca.ts.partest$test$significant$X), 
                  var = colnames(ts)[mca.ts.partest$test$significant$X],
                  c = NA, 
                  phi = mca.ts.partest$test$global[1:length(mca.ts.partest$test$significant$X),2],
                  p = mca.ts.partest$test$global[1:length(mca.ts.partest$test$significant$X),6])
coef <- mca.ts.partest$UpYXcb$CM
for (ii in 1:length(mca.ts.partest$test$significant$X))
{
    res.ts$c[ii] <- coef[names(mca.ts.partest$test$significant$X)[ii], mca.ts.partest$test$significant$X[ii]]
}
res.ts$c <- round(res.ts$c, dig = 3)
res.ts$phi <- round(res.ts$phi, dig = 2)
res.ts$p <- round(res.ts$p, dig = 3)

# Final results table
res.ts

#------------------------------------------#

# Co-dependence analysis of relation between beetle community and herb species
mca.h <- MCA(Y = b, X = h, emobj = map)
mca.h.partest <- test.cdp(mca.h)
summary(mca.h.partest)

ind <- colSums(h > 0) > 25

png("figs/Fig7b.png", width = 800, height = 450)
par(mar = c(2,14,.5,3.5), cex = 1.5)
plot.cdp(mca.h.partest, las = 2, col = rev(terrain.colors(256)), ind = ind)
dev.off()

## Nonparametric testing, similar results, but much longer computation
#mca.h.pertest <- permute.cdp(mca.h)
#summary(mca.h.pertest)
#plot(mca.h.pertest, las = 2)

#mca.h.pertest$UpYXcb$C[,1L,]

res.h <- data.frame(mem = names(mca.h.partest$test$significant$X), 
                     var = colnames(h)[mca.h.partest$test$significant$X],
                     c = NA, 
                     phi = mca.h.partest$test$global[1:length(mca.h.partest$test$significant$X),2],
                     p = mca.h.partest$test$global[1:length(mca.h.partest$test$significant$X),6])
coef <- mca.h.partest$UpYXcb$CM
for (ii in 1:length(mca.h.partest$test$significant$X))
{
    res.h$c[ii] <- coef[names(mca.h.partest$test$significant$X)[ii], mca.h.partest$test$significant$X[ii]]
}

res.h$c <- round(res.h$c, dig = 3)
res.h$phi <- round(res.h$phi, dig = 2)
res.h$p <- round(res.h$p, dig = 3)

#------------------------------------------#

# Constructing a predictor matrix of PCs from woody and herb communities

ts.pca <- rda(ts)
barplot(ts.pca$CA$eig, col = "tomato")
abline(h = mean(ts.pca$CA$eig))
ts.npc <- sum(ts.pca$CA$eig > mean(ts.pca$CA$eig))
ts.sc <- scores(ts.pca, choices = 1:ts.npc)$sites

h.pca <- rda(h)
barplot(h.pca$CA$eig, col = "limegreen")
abline(h = mean(h.pca$CA$eig))
h.npc <- sum(h.pca$CA$eig > mean(h.pca$CA$eig))
h.sc <- scores(h.pca, choices = 1:h.npc)$sites

expl <- cbind(ts.sc, h.sc)
colnames(expl) <- c(paste0("ts", 1:ts.npc), paste0("h", 1:h.npc))

#------------------------------------------#

# Final analysis. Multiscale co-dependence analysis between insect community and phytocoenosis
mca <- MCA(Y = b, X = expl, emobj = map)
mca.partest <- test.cdp(mca)
summary(mca.partest)

png("figs/Fig7c.png", width = 800, height = 500)
par(mar = c(2,14,.5,3.5), cex = 1.5)
plot.cdp(mca.partest, las = 2, col = rev(terrain.colors(256)))
dev.off()

## Nonparametric testing, similar results, but much longer computation
#mca.pertest <- permute.mca(mca)
#summary(mca.pertest)
#plot(mca.pertest, las = 2)

res <- data.frame(mem = names(mca.partest$test$significant$X), 
                  var = colnames(expl)[mca.partest$test$significant$X],
                  c = NA, phi = mca.partest$test$global[1:length(mca.partest$test$significant$X),2],
                  p = mca.partest$test$global[1:length(mca.partest$test$significant$X),6])
coef <- mca.partest$UpYXcb$CM
for (ii in 1:length(mca.partest$test$significant$X))
    res$c[ii] <- coef[names(mca.partest$test$significant$X)[ii], mca.partest$test$significant$X[ii]]


res$c <- round(res$c, dig = 3)
res$phi <- round(res$phi, dig = 2)
res$p <- round(res$p, dig = 3)

# Final results table
res