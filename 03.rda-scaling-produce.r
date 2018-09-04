library(vegan)

load("clean data/int.sc.2018.rda")
source("R/interaction.rda.r")

#------------------------------------------#

ts <- rts
int <- names(rh) %in% names(rts)
h <- rh[, !int]
b <- rb[, 1:22]

res.r <- vector("list", 4)

res.r[[1]] <- interaction.rda(ts, h, b, 5000)

for (ii in 2:4)
{
  print(ii)
  b <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rb[x:(x+ii-1), 1:22]))))
  h <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rh[x:(x+ii-1), !int]))))
  ts <- data.frame(t(sapply(seq(1,ii*(100 %/% ii), by = ii), function(x) colSums(rts[x:(x+ii-1),]))))
  
  res.r[[ii]] <- interaction.rda(ts, h, b, 5000)
}

#-----#

ts <- cts
h <- ch
b <- cb

res.c <- vector("list", 4)

res.c[[1]] <- interaction.rda(ts, h, b, 5000)

for (ii in 2:4)
{
  print(ii)
  b <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cb[x:(x+ii-1),]))))
  h <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(ch[x:(x+ii-1),]))))
  ts <- data.frame(t(sapply(seq(1,ii*(96 %/% ii), by = ii), function(x) colSums(cts[x:(x+ii-1),]))))
  
  res.c[[ii]] <- interaction.rda(ts, h, b, 5000)
}

#-----#

save(res.r, res.c, file = "clean data/rda.scaling.rda")