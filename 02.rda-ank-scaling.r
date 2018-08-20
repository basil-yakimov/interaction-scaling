library(vegan)

load("clean data/int.sc.2018.rda")
source("R/interaction.rda.r")

#------------------------------------------#



ts <- cts
h <- ch
b <- cb

res.c <- vector("list", 5)

res.c[[1]] <- interaction.rda(ts, h, b, 5000)

for (ii in 2:5)
{
  b <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cb[x:(x+ii-1),]))))
  h <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(ch[x:(x+ii-1),]))))
  ts <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cts[x:(x+ii-1),]))))
  
  res.c[[ii]] <- interaction.rda(ts, h, b, 5000)
}

#-----#

ts <- rts
h <- rh
b <- rb[, 1:22]

res.r <- vector("list", 5)

res.r[[1]] <- interaction.rda(ts, h, b, 5000)

for (ii in 2:5)
{
  b <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rb[x:(x+ii-1), 1:22]))))
  h <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rh[x:(x+ii-1),]))))
  ts <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rts[x:(x+ii-1),]))))

  res.r[[ii]] <- interaction.rda(ts, h, b, 5000)
}

#-----#

save(res.r, res.c, file = "clean data/rda.scaling.rda")




op <- par(mfcol = c(2,5), mar = c(4, 0.5, 2, 0.5))

for (ii in 1:5)
{
  plot(res.r[[ii]]$part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))
  title(main = paste0("scale = ", ii))
  plot(res.c[[ii]]$part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))
}

par(op)


r.h.r <- c.h.r <- r.ts.r <- c.ts.r <- matrix(0, nrow = 2, ncol = 5)

for (ii in 1:5)
{
  r.ts.r[1, ii] <- res.r[[ii]]$part$part$indfract[6, 3]
  r.ts.r[2, ii] <- res.r[[ii]]$part$part$indfract[1, 3]
  
  c.ts.r[1, ii] <- res.c[[ii]]$part$part$indfract[6, 3]
  c.ts.r[2, ii] <- res.c[[ii]]$part$part$indfract[1, 3]
  
  r.h.r[1, ii] <- res.r[[ii]]$part$part$indfract[5, 3]
  r.h.r[2, ii] <- res.r[[ii]]$part$part$indfract[2, 3]
  
  c.h.r[1, ii] <- res.c[[ii]]$part$part$indfract[5, 3]
  c.h.r[2, ii] <- res.c[[ii]]$part$part$indfract[2, 3]
}

r.ts.r[r.ts.r < 0] <- 0
r.h.r[r.h.r < 0] <- 0
c.ts.r[c.ts.r < 0] <- 0
c.h.r[c.h.r < 0] <- 0


op <- par(mfrow = c(2,2))
barplot(r.ts.r, col = c("darkred", "tomato"), ylim = c(0, 0.17))
barplot(r.h.r, col = c("darkgreen", "limegreen"), ylim = c(0, 0.17))
barplot(c.ts.r, col = c("darkred", "tomato"), ylim = c(0, 0.31))
barplot(c.h.r, col = c("darkgreen", "limegreen"), ylim = c(0, 0.31))
par(op)




RsquareAdj(res[[4]]$h.rda.pars)$adj.r.squared
anova(res[[5]]$h.result, permutations = 1000)
anova(res[[5]]$ts.result, permutations = 1000)
plot(res[[4]]$part, digits = 5, bg = 2:4, Xnames = c("wood", "herb", "MEM"))



res.r[[4]]$part$part$indfract

showvarparts((3))

