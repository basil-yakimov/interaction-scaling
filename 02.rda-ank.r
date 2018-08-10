library(vegan)

load("clean data/int.sc.2018.rda")

rts <- decostand(rts, method = "hellinger")
rh <- decostand(rh, method = "hellinger")
rb <- decostand(rb, method = "hellinger")

#------------------------------------------#

# 1st preliminary step. rda for selection of tree and shrub species into parsimonious model

rts.rda <- rda(rb ~ ., rts)

RsquareAdj(rts.rda)$adj.r.squared

sf <- ordistep(rda(rb ~ 1, data = rts), scope = formula(rts.rda), 
               direction = "forward", pstep = 5000, trace = 0)

rts.pars <- rts[, attr(sf$terminfo$terms, "term.labels")]

rts.rda.pars <- rda(rb ~ ., rts.pars)

RsquareAdj(rts.rda.pars)$adj.r.squared
anova(rts.rda.pars, step = 1000)

#------------------------------------------#

# 2nd preliminary step. rda for selection of herb species into parsimonious model

rh.rda <- rda(rb ~ ., rh)

RsquareAdj(rh.rda)$adj.r.squared

sf <- ordistep(rda(rb ~ 1, data = rh), scope = formula(rh.rda), 
               direction = "forward", pstep = 1000, trace = 0)

rh.pars <- rh[, attr(sf$terminfo$terms, "term.labels")]

rh.rda.pars <- rda(rb ~ ., rh.pars)

RsquareAdj(rh.rda.pars)$adj.r.squared
anova(rh.rda.pars, step = 1000)

#------------------------------------------#

# 3rd preliminary step. construction and selection of spatial variables

transect <- seq(1:100)
transect.d1 <- dist(transect)
# truncation distance set to 1
thresh <- 1
# Truncation to threshold 1
transect.d1[transect.d1 > thresh] <- 4*thresh
# PCoA of truncated matrix 
transect.PCoA <- cmdscale(transect.d1, eig = TRUE, k = length(transect)-1)
# Count the positive eigenvalues
(nb.ev <- length(which(transect.PCoA$eig > 0.0000001)))
# Matrix of PCNM variables
transect.pcnm <- transect.PCoA$points[,1:nb.ev]

pcnm <- data.frame(transect.pcnm)

#------------------------------------------#

pcnm.rda <- rda(rb ~ ., data = pcnm)

RsquareAdj(pcnm.rda)$adj.r.squared

sf <- ordiR2step(rda(rb ~ 1, data = pcnm), scope = formula(pcnm.rda), 
                 direction = "forward", pstep = 1000, trace = 0)

pcnm.pars <- pcnm[,attr(sf$terminfo$terms, "term.labels")]

pcnm.rda.pars <- rda(rb ~ ., pcnm.pars)

RsquareAdj(pcnm.rda.pars)$adj.r.squared
anova(pcnm.rda.pars, step = 1000)

#------------------------------------------#

# Final analysis. Variance partition

part <- varpart(rb, rts.pars, rh.pars, pcnm.pars)

plot(part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))

png("figs/Fig1a.png", width = 600, height = 600)
par(mar = c(.5,.5,.5,.5))
plot(part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"), id.size = 2)
dev.off()

# Testing individual fractions of variance (herb fraction insignificant)

rts.result <- rda(rb ~ as.matrix(rts.pars) + Condition(as.matrix(rh.pars)) +
                   Condition(as.matrix(pcnm.pars)))
anova(rts.result, step=200, perm.max=200)

rh.result <- rda(rb ~ as.matrix(rh.pars) + Condition(as.matrix(rts.pars)) +
                   Condition(as.matrix(pcnm.pars)))
anova(rh.result, step=200, perm.max=200)

pcnm.result <- rda(rb ~ as.matrix(pcnm.pars) + Condition(as.matrix(rts.pars)) +
                     Condition(as.matrix(rh.pars)))
anova(pcnm.result, step=200, perm.max=200)