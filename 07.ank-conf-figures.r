library(vegan)

load("clean data/int.sc.2018.rda")

int <- names(rh) %in% names(rts)
rh <- rh[, !int]
rb <- rb[, -47]

rts <- rts[, order(colSums(rts))]
rh <- rh[, order(colSums(rh))]
rb <- rb[, order(colSums(rb))]

#------------------------------------------#

max(rts)

ts.col <- rts
ts.col[] <- "white"
ts.col[rts > 0 & rts < 15] <- "tomato"
ts.col[rts >= 15 & rts < 30] <- "darkred"
ts.col[rts >= 30] <- "black"

ab <- colSums(rts)
ab <- ab / max(ab) * 40


png("figs/CFig1a.png", width = 1200, height = 600, bg = "transparent")

op <- par(mar = c(0.25, 10, 0.25, 0.25), cex = 2)

plot(1, 1, type = "n", ylim = c(-1, 13.5), xlim = c(0, 151), axes = F, ann = F)
for (ii in 1:13) points(1:100, rep(ii, 100), pch = 22, cex = 1, bg = ts.col[,ii])

rect(xleft = 111, ybottom = (1:13) - 0.2, xright = 111 + ab, ytop = (1:13) + 0.2, col = "tomato")

axis(side = 2, at = c(1:13), labels = sub("\\." , " ", colnames(rts)), las = 2, tick = F)
legend("bottom", c("0", "1-15", "16-30", "> 30"), horiz = TRUE, bty = "n", pch = 22, 
       pt.bg = c("white", "tomato", "darkred", "black"), pt.cex = 2)

dev.off()

#---#

max(rh)

h.col <- rh
h.col[] <- "white"
h.col[rh > 0 & rh < 25] <- "lightgreen"
h.col[rh >= 25 & rh < 50] <- "darkgreen"
h.col[rh >= 50] <- "black"

ab <- colSums(rh)
ab <- ab / max(ab) * 40


png("figs/CFig1b.png", width = 1200, height = 600, bg = "transparent")

op <- par(mar = c(0.25, 12, 0.25, 0.25), cex = 2)

plot(1, 1, type = "n", ylim = c(-1, 13.5), xlim = c(0, 151), axes = F, ann = F)
for (ii in 1:13) points(1:100, rep(ii, 100), pch = 22, cex = 1, bg = h.col[,ii+20])

rect(xleft = 111, ybottom = (1:13) - 0.2, xright = 111 + ab[21:33], ytop = (1:13) + 0.2, col = "limegreen")

axis(side = 2, at = c(1:13), labels = sub("\\." , " ", colnames(rh)[21:33]), las = 2, tick = F)
legend("bottom", c("0", "1-5", "5-10", "> 10"), horiz = TRUE, bty = "n", pch = 22, 
       pt.bg = c("white", "lightgreen", "darkgreen", "black"), pt.cex = 2)

dev.off()

#---#

max(rb)

b.col <- rb
b.col[] <- "white"
b.col[rb > 0 & rb < 25] <- "skyblue"
b.col[rb >= 25 & rb < 50] <- "darkblue"
b.col[rb >= 50] <- "black"

ab <- colSums(rb)
ab <- ab / max(ab) * 40

png("figs/CFig1c.png", width = 1200, height = 600, bg = "transparent")

op <- par(mar = c(0.25, 14, 0.25, 0.25), cex = 2)

plot(1, 1, type = "n", ylim = c(-1, 13.5), xlim = c(0, 151), axes = F, ann = F)
for (ii in 1:13) points(1:100, rep(ii, 100), pch = 22, cex = 1, bg = b.col[,ii+33])

rect(xleft = 111, ybottom = (1:13) - 0.2, xright = 111 + ab[34:46], ytop = (1:13) + 0.2, col = "steelblue")

axis(side = 2, at = c(1:13), labels = sub("\\." , " ", colnames(rb)[34:46]), las = 2, tick = F)
legend("bottom", c("0", "1-25", "26-50", "> 50"), horiz = TRUE, bty = "n", pch = 22, 
       pt.bg = c("white", "skyblue", "darkblue", "black"), pt.cex = 2)

dev.off()

#------------------------------------------#

png("figs/CFig2a.png", width = 900, height = 900, bg = "transparent")

op <- par(mfrow = c(3, 3), mar = c(2,2,.25,.25), cex = 1.5)

for (ii in 1:3)
{
  for (jj in 1:3)
  {
    y <- rb[, 46-ii+1]
    x <-  rts[, 13-jj+1]
    
    plot(y ~ x, pch = 21, bg = "steelblue", xlab = "", ylab = "", axes = F)
    if (jj == 1) axis(2)
    if (ii == 3) axis(1)
    if (summary(lm(y ~ x))$coefficients[2, 4] < 0.05) abline(lm(y ~ x))
    box()
  }
}

dev.off()


png("figs/CFig2b.png", width = 900, height = 900, bg = "transparent")

op <- par(mfrow = c(3, 3), mar = c(2,2,.25,.25), cex = 1.5)

for (ii in 1:3)
{
  for (jj in 1:3)
  {
    y <- rb[, 23-ii+1]
    x <-  rts[, 8-jj+1]
    
    plot(y ~ x, pch = 21, bg = "steelblue", xlab = "", ylab = "", axes = F)
    if (jj == 1) axis(2)
    if (ii == 3) axis(1)
    if (summary(lm(y ~ x))$coefficients[2, 4] < 0.05) abline(lm(y ~ x))
    box()
  }
}

dev.off()



#------------------------------------------#

ts <- decostand(rts, method = "hellinger")
h <- decostand(rh, method = "hellinger")
b <- decostand(rb, method = "hellinger")

#------------------------------------------#

library(ade4)

# PCA
dudi.b <- dudi.pca(b, scale = T, scan = F, nf = 2)
dudi.h <- dudi.pca(h, scale = T, scan = F, nf = 2)
dudi.ts <- dudi.pca(ts, scale = T, scan = F, nf = 2)

# Co-inertia analysis
coia.h <- coinertia(dudi.b, dudi.h, scan = F, nf = 2)
coia.ts <- coinertia(dudi.b, dudi.ts, scan = F, nf = 2)

# Permutation test
test.h <- randtest(coia.h, nrepet = 999)
test.ts <- randtest(coia.ts, nrepet = 999)

png("figs/CFig3a.png", width = 600, height = 600)
par(mar = c(4,4,.5,.5), cex = 2)
plot(test.h$plot$hist, xlim = test.h$plot$xlim, col = "limegreen", main = "", xlab = "RV", ylab = "Частота")
arrows(x0 = test.h$obs, y0 = max(test.h$plot$hist$counts)/2, y1 = 10, angle = 25, lwd = 2)
text(test.h$obs, max(test.h$plot$hist$counts)/2, labels = bquote(RV[obs] == .(round(test.h$obs, digits = 3))), pos = 3 )
dev.off()

png("figs/CFig3b.png", width = 600, height = 600)
par(mar = c(4,4,.5,.5), cex = 2)
plot(test.ts$plot$hist, xlim = test.ts$plot$xlim, col = "tomato", main = "", xlab = "RV", ylab = "Частота")
arrows(x0 = test.ts$obs, y0 = max(test.ts$plot$hist$counts)/2, y1 = 10, angle = 25, lwd = 2)
text(test.ts$obs-0.02, max(test.ts$plot$hist$counts)/2, labels = bquote(RV[obs] == .(round(test.ts$obs, digits = 3))), pos = 3 )
dev.off()

rv <- c(test.h$obs, test.ts$obs)
p <- c(test.h$pvalue, test.ts$pvalue)

#------------------------------------------#

# Aggregation of data and performing the same analysis

for (ii in 2:20)
{
    b <- t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rb[x:(x+ii-1),])))
    b <- decostand(b, method = "hellinger")
    
    h <- t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rh[x:(x+ii-1),])))
    h <- decostand(h, method = "hellinger")
    
    ts <- t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rts[x:(x+ii-1),])))
    ts <- decostand(ts, method = "hellinger")
    
    # PCA
    dudi.b <- dudi.pca(b, scale = T, scan = F, nf = 2)
    dudi.h <- dudi.pca(h, scale = TRUE, scan = F, nf = 2)
    dudi.ts <- dudi.pca(ts, scale = TRUE, scan = F, nf = 2)

    # Co-inertia analysis
    coia.h <- coinertia(dudi.b, dudi.h, scan = F, nf = 2)
    coia.ts <- coinertia(dudi.b, dudi.ts, scan = F, nf = 2)

    # Permutation test
    test.h <- randtest(coia.h, nrepet = 999)
    test.ts <- randtest(coia.ts, nrepet = 999)

    rv <- rbind(rv, c(test.h$obs, test.ts$obs))
    p <- rbind(p, c(test.h$pvalue, test.ts$pvalue))
}


png("figs/CFig4.png", width = 1200, height = 600)
par(mar = c(4,4,.5,.5), cex = 2)

plot((1:20)*10, rv[,1], ylim = c(0,1), type = "o", pch = 21, bg = "white", xlab = "масштаб", ylab = "RV", col = "darkgreen")
lines((1:20)*10, rv[,2], type = "o", pch = 21, bg = "white", col = "darkred")

sig <- p[, 1] < 0.05
points(((1:20)*10)[sig], rv[sig, 1],pch = 21, bg = "limegreen")
sig <- p[, 2] < 0.05
points(((1:20)*10)[sig], rv[sig, 2],pch = 21, bg = "tomato")

legend("bottomright", legend = c("травянистый ярус", "древесно-кустарниковый ярус", "не значимо"), 
       pch = 21, lwd = 1, pt.bg = c("limegreen", "tomato", "white"), col = c("darkgreen", "darkred", "black"))

dev.off()


#------------------------------------------#

library(codep)
source("R/custom.plot.cdp.r")

ts <- decostand(rts, method = "hellinger")
h <- decostand(rh, method = "hellinger")
b <- decostand(rb, method = "hellinger")

ts <- as.matrix(ts)
h <- as.matrix(h)
b <- as.matrix(b)

map <- eigenmap(x = 1:nrow(b), weighting = Wf.binary, boundaries = c(0,1))

# Co-dependence analysis of relation between beetle community and separate species
mca.tsh <- MCA(Y = b, X = cbind(ts[, 9:13], h[, c(15:19, 28:33)]), emobj = map)
mca.tsh.partest <- test.cdp(mca.tsh)
summary(mca.tsh.partest)

png("figs/CFig5a.png", width = 800, height = 400)
par(mar = c(3,10,.5,3.5), cex = 1.5)
plot.cdp(mca.tsh.partest, las = 2, col = rev(terrain.colors(256)), scale.labels = round(c(1000, 2*1010/(3:21))))
dev.off()


res.tsh <- data.frame(mem = names(mca.tsh.partest$test$significant$X), 
                     var = c(colnames(ts[, 9:13]), colnames(h[, c(15:19, 28:33)]))[mca.tsh.partest$test$significant$X],
                     c = NA, 
                     phi = mca.tsh.partest$test$global[1:length(mca.tsh.partest$test$significant$X),2],
                     p = mca.tsh.partest$test$global[1:length(mca.tsh.partest$test$significant$X),6])
coef <- mca.tsh.partest$UpYXcb$CM
for (ii in 1:length(mca.tsh.partest$test$significant$X))
{
  res.tsh$c[ii] <- coef[names(mca.tsh.partest$test$significant$X)[ii], mca.tsh.partest$test$significant$X[ii]]
}
res.tsh$c <- round(res.tsh$c, dig = 3)
res.tsh$phi <- round(res.tsh$phi, dig = 2)
res.tsh$p <- round(res.tsh$p, dig = 3)

# Final results table
res.tsh

#---#

# Constructing a predictor matrix of PCs from woody and herb communities

ts.pca <- rda(ts)
ts.npc <- sum(ts.pca$CA$eig > mean(ts.pca$CA$eig))
ts.sc <- scores(ts.pca, choices = 1:ts.npc)$sites

h.pca <- rda(h)
h.npc <- sum(h.pca$CA$eig > mean(h.pca$CA$eig))
h.sc <- scores(h.pca, choices = 1:h.npc)$sites

expl <- cbind(ts.sc, h.sc)
colnames(expl) <- c(paste0("др-к-", 1:ts.npc), paste0("трав-", 1:h.npc))

#------------------------------------------#

# Final analysis. Multiscale co-dependence analysis between insect community and phytocoenosis
mca <- MCA(Y = b, X = expl, emobj = map)
mca.partest <- test.cdp(mca)
summary(mca.partest)

png("figs/CFig5b.png", width = 800, height = 300)
par(mar = c(3,10,.5,3.5), cex = 1.5)
plot.cdp(mca.partest, las = 2, col = rev(terrain.colors(256)), scale.labels = round(c(1000, 2*1010/(3:21))))
dev.off()

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


#------------------------------------------#


png("figs/CFig6a.png", width = 600, height = 300)
par(mar = c(4,.5,.5,.5), cex = 1.5)

plot(map$U[, 1], type = "o", pch = 19, cex = 0.5, xlab = "Площадки", ylab = "", col = "grey", axes = F)
axis(1)

dev.off()

png("figs/CFig6b.png", width = 600, height = 300)
par(mar = c(4,.5,.5,.5), cex = 1.5)

plot(map$U[, 7], type = "o", pch = 19, cex = 0.5, xlab = "Площадки", ylab = "", col = "grey", axes = F)
axis(1)

dev.off()