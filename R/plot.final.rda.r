plot.final.rda <- function(a)
{
  col <- rep(c("tomato", "limegreen", "skyblue"), c(
    length(a$ts.rda.pars$terminfo$ordered),
    length(a$h.rda.pars$terminfo$ordered),
    length(a$pcnm.rda.pars$terminfo$ordered)))
  
  
  plot(a$full.rda.pars, scaling = 1, display = c("species", "sites"), type = "none")
  sc.sp <- scores(a$full.rda.pars, scaling = 1, choices = 1:2, display = "sp")
  arrows(0, 0, sc.sp[, 1], sc.sp[, 2], col = "grey", length = 0)
  sc.fac <- scores(a$full.rda.pars, scaling = 1, choices = 1:2, display = "bp")
  sc.fac <- sc.fac * ordiArrowMul(sc.fac)
  arrows(0, 0, sc.fac[, 1], sc.fac[, 2], col = col, length = 0, lwd = 2)
  lab <- ordiArrowTextXY(sc.fac, rescale = FALSE, labels = rownames(sc.fac))
  text(sc.fac*1.1, labels = lab.convert(rownames(sc.fac)), col = col)
  
}


lab.convert <- function(a)
{
  split <- strsplit(a, split = "[.]")
  for (ii in 1:length(a))
  {
    if (length(split[[ii]]) > 1)
    {
      a[ii] <- paste(substr(split[[ii]][1], 1, 1), substr(split[[ii]][2], 1, 3), sep = ".")
    }
  }
  a
}