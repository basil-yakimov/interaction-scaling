interaction.rda <- function(ts, h, b, nperm = 1000)
{
  #require(caret)
  
  if (nrow(h) != nrow(ts) | nrow(h) != nrow(b))
  {
    error("input data should have the same number od rows")
  }
  
  ts <- decostand(ts, method = "hellinger")
  h <- decostand(h, method = "hellinger")
  b <- decostand(b, method = "hellinger")
  
  # 1st preliminary step. rda for selection of tree and shrub species into parsimonious model
  
  ts.rda <- rda(b ~ ., ts)
  
  sf <- ordistep(rda(b ~ 1, data = ts), scope = formula(ts.rda), 
                 direction = "forward", trace = F, permutations = nperm)
  
  ts.pars <- ts[, attr(sf$terminfo$terms, "term.labels")]
  
  ts.rda.pars <- rda(b ~ ., ts.pars)
  
  #RsquareAdj(ts.rda.pars)$adj.r.squared
  #anova(ts.rda.pars, step = 1000)
  
  #------------------------------------------#
  
  # 2nd preliminary step. rda for selection of herb species into parsimonious model
  
  h.rda <- rda(b ~ ., h)
  
  sf <- ordistep(rda(b ~ 1, data = h), scope = formula(h.rda), 
                 direction = "forward", trace = F, permutations = nperm)
  
  h.pars <- h[, attr(sf$terminfo$terms, "term.labels")]
  
  h.rda.pars <- rda(b ~ ., h.pars)
  
  #RsquareAdj(h.rda.pars)$adj.r.squared
  #anova(h.rda.pars, step = 1000)
  
  #------------------------------------------#
  
  # 3rd preliminary step. construction and selection of spatial variables
  
  transect <- seq(1:nrow(b))
  transect.d1 <- dist(transect)
  # truncation distance set to 1
  thresh <- 1
  # Truncation to threshold 1
  transect.d1[transect.d1 > thresh] <- 4*thresh
  # PCoA of truncated matrix 
  transect.PCoA <- cmdscale(transect.d1, eig = TRUE, k = length(transect)-1)
  # Count the positive eigenvalues
  nb.ev <- length(which(transect.PCoA$eig > 0.0000001))
  # Matrix of PCNM variables
  transect.pcnm <- transect.PCoA$points[,1:nb.ev]
  
  pcnm <- data.frame(transect.pcnm)
  
  #------------------------------------------#
  
  pcnm.rda <- rda(b ~ ., data = pcnm)
  
  sf <- ordistep(rda(b ~ 1, data = pcnm), scope = formula(pcnm.rda), 
                 direction = "forward", trace = F, permutations = nperm)
  
  pcnm.pars <- pcnm[,attr(sf$terminfo$terms, "term.labels")]
  
  pcnm.rda.pars <- rda(b ~ ., pcnm.pars)
  
  #RsquareAdj(pcnm.rda.pars)$adj.r.squared
  #anova(pcnm.rda.pars, step = 1000)
  
  #------------------------------------------#
  
  # int <- names(h.pars) %in% names(ts.pars)
  # if (sum(int) > 0)
  # {
  #   names(h.pars)[int] <- paste0(names(h.pars)[int], ".h")
  # }
  
  # Final analysis. Variance partition
  
  n <- c(ncol(ts.pars), ncol(h.pars), ncol(pcnm.pars))
  
  while (sum(n)+1 >= nrow(b))
  {
    print(paste0("nrow = ", nrow(b), "; nts = ", n[1], ", n h = ", n[2], ", n pcnm = ", n[3]))
    if (n[2] >= n[3])
    {
      h.pars <- h.pars[, -n[2]]
      n[2] <- n[2] - 1
    } else
    {
      pcnm.pars <- pcnm.pars[, -n[3]]
      n[3] <- n[3] - 1
    }
  }
  
  full.pars <- cbind(ts.pars, h.pars, pcnm.pars)
  
  # comboInfo <- findLinearCombos(full.pars)
  # 
  # if (!is.null(comboInfo$remove))
  # {
  #   ind1 <- comboInfo$remove <= n[1]
  #   if (sum(ind1) > 0)
  #   {
  #     out1 <- comboInfo$remove[ind1]
  #     ts.pars <- ts.pars[, -out1]
  #   }
  #   ind2 <- comboInfo$remove > n[1] & comboInfo$remove <= n[2]
  #   if (sum(ind2) > 0)
  #   {
  #     out2 <- comboInfo$remove[ind2]
  #     h.pars <- h.pars[, -(out2-n[1])]
  #   }
  #   ind3 <- comboInfo$remove > n[2]
  #   if (sum(ind3) > 0)
  #   {
  #     out3 <- comboInfo$remove[ind3]
  #     pcnm.pars <- pcnm.pars[, -(out3-n[2]-n[1])]
  #   }
  # }
  
  full.pars <- cbind(ts.pars, h.pars, pcnm.pars)
  
  full.rda.pars <- rda(b ~ ., full.pars)
  
  # sf <- ordistep(rda(b ~ 1, data = full.pars), scope = formula(full.rda.pars), 
  #                direction = "forward", trace = F, permutations = nperm)
  
  
  part <- varpart(b, ts.pars, h.pars, pcnm.pars)
  
  # Testing individual fractions of variance (all fraction are significant)
  
  ts.result <- rda(b ~ as.matrix(ts.pars) + Condition(as.matrix(h.pars)) +
                     Condition(as.matrix(pcnm.pars)))
  #anova(ts.result, step=200, perm.max=200)
  
  h.result <- rda(b ~ as.matrix(h.pars) + Condition(as.matrix(ts.pars)) +
                    Condition(as.matrix(pcnm.pars)))
  #anova(h.result, step=200, perm.max=200)
  
  pcnm.result <- rda(b ~ as.matrix(pcnm.pars) + Condition(as.matrix(ts.pars)) +
                       Condition(as.matrix(h.pars)))
  #anova(pcnm.result, step=200, perm.max=200)
  
  return(list(
    ts.rda.pars = ts.rda.pars, h.rda.pars = h.rda.pars, pcnm.rda.pars = pcnm.rda.pars, 
    full.rda.pars = full.rda.pars, part = part, 
    ts.result = ts.result, h.result = h.result, pcnm.result = pcnm.result 
  ))
}