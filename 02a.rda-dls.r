library(vegan)

load("clean data/int.sc.2018.rda")

cts <- decostand(cts, method = "hellinger")
ch <- decostand(ch, method = "hellinger")
cb <- decostand(cb, method = "hellinger")

#------------------------------------------#

# 1st preliminary step. rda for selection of tree and shrub species into parsimonious model

cts.rda <- rda(cb ~ ., cts)

RsquareAdj(cts.rda)$adj.r.squared

sf <- ordistep(rda(cb ~ 1, data = cts), scope = formula(cts.rda), 
               direction = "forward", pstep = 5000, trace = 0)

cts.pars <- cts[, attr(sf$terminfo$terms, "term.labels")]

cts.rda.pars <- rda(cb ~ ., cts.pars)

RsquareAdj(cts.rda.pars)$adj.r.squared
anova(cts.rda.pars, step = 1000)

#------------------------------------------#

# 2nd preliminary step. rda for selection of herb species into parsimonious model

ch.rda <- rda(cb ~ ., ch)

RsquareAdj(ch.rda)$adj.r.squared

sf <- ordistep(rda(cb ~ 1, data = ch), scope = formula(ch.rda), 
               direction = "forward", pstep = 1000, trace = 0)

ch.pars <- ch[, attr(sf$terminfo$terms, "term.labels")]

ch.rda.pars <- rda(cb ~ ., ch.pars)

RsquareAdj(ch.rda.pars)$adj.r.squared
anova(ch.rda.pars, step = 1000)

#------------------------------------------#

# 3rd preliminary step. construction and selection of spatial variables

transect <- seq(1:96)
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

pcnm.rda <- rda(cb ~ ., data = pcnm)

RsquareAdj(pcnm.rda)$adj.r.squared

sf <- ordiR2step(rda(cb ~ 1, data = pcnm), scope = formula(pcnm.rda), 
                 direction = "forward", pstep = 1000, trace = 0)

pcnm.pars <- pcnm[,attr(sf$terminfo$terms, "term.labels")]

pcnm.rda.pars <- rda(cb ~ ., pcnm.pars)

RsquareAdj(pcnm.rda.pars)$adj.r.squared
anova(pcnm.rda.pars, step = 1000)

#------------------------------------------#

# Final analysis. Variance partition

part <- varpart(cb, cts.pars, ch.pars, pcnm.pars)

plot(part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"))

png("figs/Fig1b.png", width = 600, height = 600)
par(mar = c(.5,.5,.5,.5))
plot(part, digits = 2, bg = 2:4, Xnames = c("wood", "herb", "MEM"), id.size = 2)
dev.off()

# Testing individual fractions of variance (tree fraction insignificant)

cts.result <- rda(cb ~ as.matrix(cts.pars) + Condition(as.matrix(ch.pars)) +
                   Condition(as.matrix(pcnm.pars)))
anova(cts.result, step=200, perm.max=200)

ch.result <- rda(cb ~ as.matrix(ch.pars) + Condition(as.matrix(cts.pars)) +
                   Condition(as.matrix(pcnm.pars)))
anova(ch.result, step=200, perm.max=200)

pcnm.result <- rda(cb ~ as.matrix(pcnm.pars) + Condition(as.matrix(cts.pars)) +
                     Condition(as.matrix(ch.pars)))
anova(pcnm.result, step=200, perm.max=200)
