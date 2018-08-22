library(vegan)

load("clean data/int.sc.2018.rda")
source("R/interaction.rda.r")

#------------------------------------------#

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

save(res.r, res.c, file = "clean data/rda.scaling.rda")

#-----#


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


# a <- res.r[[1]]$ts.rda.pars
# str(a)
# a$call
# paste(names(a$terminfo$ordered), collapse = ", ")

terms <- matrix("", nrow = 5, ncol = 2*3)
termsnum <- matrix(0, nrow = 5, ncol = 2*3)

for (ii in 1:5)
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