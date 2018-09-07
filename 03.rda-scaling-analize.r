library(vegan)

load("clean data/rda.scaling.rda")
source("R/plot.final.rda.r")

#------------------------------------------#

png("figs/Fig3.png", width = 800, height = 1600)
par(mfrow = c(4, 2), mar = c(0.5, 2, 1.5, 0.5), cex = 1.15)

for (ii in 1:4)
{
  plot(res.r[[ii]]$part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))
  mtext(paste0("scale = ", ii), side = 2, line = 1, cex = 1.15)
  if (ii == 1) title(main = "Plain forest")
  plot(res.c[[ii]]$part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))
  if (ii == 1) title(main = "Mountain forest")
}

dev.off()


r.h.r2 <- c.h.r2 <- r.ts.r2 <- c.ts.r2 <- matrix(0, nrow = 2, ncol = 4)
r.pcnm.r2 <- c.pcnm.r2 <- matrix(0, nrow = 4, ncol = 4)

for (ii in 1:4)
{
  r.ts.r2[1, ii] <- res.r[[ii]]$part$part$indfract[6, 3]
  r.ts.r2[2, ii] <- res.r[[ii]]$part$part$indfract[1, 3]
  
  c.ts.r2[1, ii] <- res.c[[ii]]$part$part$indfract[6, 3]
  c.ts.r2[2, ii] <- res.c[[ii]]$part$part$indfract[1, 3]
  
  r.pcnm.r2[1, ii] <- res.r[[ii]]$part$part$indfract[3, 3]
  r.pcnm.r2[2, ii] <- res.r[[ii]]$part$part$indfract[6, 3]
  r.pcnm.r2[3, ii] <- res.r[[ii]]$part$part$indfract[7, 3]
  r.pcnm.r2[4, ii] <- res.r[[ii]]$part$part$indfract[5, 3]
  
  r.h.r2[1, ii] <- res.r[[ii]]$part$part$indfract[5, 3]
  r.h.r2[2, ii] <- res.r[[ii]]$part$part$indfract[2, 3]
  
  c.h.r2[1, ii] <- res.c[[ii]]$part$part$indfract[5, 3]
  c.h.r2[2, ii] <- res.c[[ii]]$part$part$indfract[2, 3]
  
  c.pcnm.r2[1, ii] <- res.c[[ii]]$part$part$indfract[3, 3]
  c.pcnm.r2[2, ii] <- res.c[[ii]]$part$part$indfract[6, 3]
  c.pcnm.r2[3, ii] <- res.c[[ii]]$part$part$indfract[7, 3]
  c.pcnm.r2[4, ii] <- res.c[[ii]]$part$part$indfract[5, 3]
}

r.ts.r2[r.ts.r2 < 0] <- 0
r.h.r2[r.h.r2 < 0] <- 0
r.pcnm.r2[r.pcnm.r2 < 0] <- 0
c.ts.r2[c.ts.r2 < 0] <- 0
c.h.r2[c.h.r2 < 0] <- 0
c.pcnm.r2[c.pcnm.r2 < 0] <- 0


png("figs/Fig4.png", width = 1800, height = 1200)
par(mfrow = c(2, 3), mar = c(5, 4, 2, 0.1), cex = 1)

barplot(r.ts.r2, col = c("darkred", "tomato"), ylim = c(0, 0.17), main = "wood", sub = "(a)")
mtext("Plain forest", side = 2, line = 2.5, cex = 1.5)
barplot(r.h.r2, col = c("darkgreen", "limegreen"), ylim = c(0, 0.17), main = "herb", sub = "(b)")
barplot(r.pcnm.r2, col = c("grey", "tomato", "wheat", "limegreen"), main = "MEM", sub = "(c)")

barplot(c.ts.r2, col = c("darkred", "tomato"), ylim = c(0, 0.31), sub = "(d)")
mtext("Mountain forest", side = 2, line = 2.5, cex = 1.5)
legend("topleft", legend = rev(c("wood * MEM", "wood")), fill = rev(c("darkred", "tomato")), bty = "n", cex = 2)
barplot(c.h.r2, col = c("darkgreen", "limegreen"), ylim = c(0, 0.31), sub = "(e)")
legend("topleft", legend = rev(c("herb * MEM", "herb")), fill = rev(c("darkgreen", "limegreen")), bty = "n", cex = 2)
barplot(c.pcnm.r2, col = c("grey", "tomato", "wheat", "limegreen"), sub = "(f)")
legend("topleft", legend = rev(c("MEM", "MEM * wood", "MEM * wood * herb", "MEM * herb")), 
       fill = rev(c("grey", "tomato", "wheat", "limegreen")), bty = "n", cex = 2)

dev.off()

#---#

terms <- matrix("", nrow = 4, ncol = 2*3)
termsnum <- matrix(0, nrow = 4, ncol = 2*3)

for (ii in 1:4)
{
  terms[ii, 1] <- paste(names(res.r[[ii]]$ts.rda.pars$terminfo$ordered), collapse = ", ")
  terms[ii, 2] <- paste(names(res.r[[ii]]$h.rda.pars$terminfo$ordered), collapse = ", ")
  terms[ii, 3] <- paste(names(res.r[[ii]]$pcnm.rda.pars$terminfo$ordered), collapse = ", ")
  terms[ii, 4] <- paste(names(res.c[[ii]]$ts.rda.pars$terminfo$ordered), collapse = ", ")
  terms[ii, 5] <- paste(names(res.c[[ii]]$h.rda.pars$terminfo$ordered), collapse = ", ")
  terms[ii, 6] <- paste(names(res.c[[ii]]$pcnm.rda.pars$terminfo$ordered), collapse = ", ")
  
  termsnum[ii, 1] <- length(res.r[[ii]]$ts.rda.pars$terminfo$ordered)
  termsnum[ii, 2] <- length(res.r[[ii]]$h.rda.pars$terminfo$ordered)
  termsnum[ii, 3] <- length(res.r[[ii]]$pcnm.rda.pars$terminfo$ordered)
  termsnum[ii, 4] <- length(res.c[[ii]]$ts.rda.pars$terminfo$ordered)
  termsnum[ii, 5] <- length(res.c[[ii]]$h.rda.pars$terminfo$ordered)
  termsnum[ii, 6] <- length(res.c[[ii]]$pcnm.rda.pars$terminfo$ordered)
}


png("figs/Fig5.png", width = 1200, height = 2400)
par(mfrow = c(4, 2), c(4, 0.5, 2, 0.5), cex = 1)

for (ii in 1:4)
{
  plot.final.rda(res.r[[ii]])
  title(main = paste0("scale = ", ii))
  plot.final.rda(res.c[[ii]])
}

dev.off()

