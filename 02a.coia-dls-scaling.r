library(vegan)

load("clean data/int.sc.2018.rda")

ts <- decostand(cts, method = "hellinger")
h <- decostand(ch, method = "hellinger")
b <- decostand(cb, method = "hellinger")

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

png("figs/Fig1b.png", width = 1200, height = 600)
par(mar = c(4,4,.5,.5), mfrow = c(1,2), cex = 2)
plot(test.h, main = "", xlab = "RV")
plot(test.ts, main = "", xlab = "RV")
dev.off()

rv <- c(test.h$obs, test.ts$obs)
p <- c(test.h$pvalue, test.ts$pvalue)

#------------------------------------------#

# Aggregation of data and performing the same analysis

for (ii in 2:20)
{
    b <- t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cb[x:(x+ii-1),])))
    b <- decostand(b, method = "hellinger")
    
    h <- t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(ch[x:(x+ii-1),])))
    h <- decostand(h, method = "hellinger")
    
    ts <- t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cts[x:(x+ii-1),])))
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


png("figs/Fig2b.png", width = 600, height = 600)
par(mar = c(4,4,.5,.5), cex = 2)

plot(rv[,1], ylim = c(0,1), type = "o", pch = 21, bg = "white", xlab = "scale", ylab = "RV")
lines(rv[,2], type = "o", pch = 21, bg = "white")

sig <- p[, 1] < 0.05
points((1:20)[sig], rv[sig, 1],pch = 21, bg = "limegreen")
sig <- p[, 2] < 0.05
points((1:20)[sig], rv[sig, 2],pch = 21, bg = "tomato")

legend("bottomright", legend = c("herb", "wood", "ns"), 
       pch = 21, lwd = 1, pt.bg = c("limegreen", "tomato", "white"))

dev.off()