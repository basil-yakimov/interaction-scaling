
# loading plain forest data matrices

library(readxl)
# r is for russian plain forest, tsh stands for tree, shrub and herb layers respectively
rt <- t(read_excel("raw data/Forest_data.xlsx", sheet = 1, range = "B2:CW12", col_names = F))
rs <- t(read_excel("raw data/Forest_data.xlsx", sheet = 2, range = "B2:CW11", col_names = F))
rh <- t(read_excel("raw data/Forest_data.xlsx", sheet = 3, range = "B2:CW43", col_names = F))

# loading and assigning latin to columns
r.tree.list <- read_excel("raw data/Forest_data.xlsx", sheet = 1, range = "A2:A12", col_names = F)[[1]]
colnames(rt) <- r.tree.list

r.shrub.list <- read_excel("raw data/Forest_data.xlsx", sheet = 2, range = "A2:A11", col_names = F)[[1]]
colnames(rs) <- r.shrub.list

r.herb.list <- read_excel("raw data/Forest_data.xlsx", sheet = 3, range = "A2:A43", col_names = F)[[1]]
colnames(rh) <- r.herb.list

# joined species list for trees and shrubs
r.ts.list <- unique(c(r.tree.list, r.shrub.list))

# merging tree and shrub data
rts <- matrix(0, nrow = 100, ncol = length(r.ts.list))

for (ii in 1:length(r.ts.list))
{
  if (r.ts.list[ii] %in% colnames(rt))
  {
    rts[ , ii] <- rts[ , ii] + rt[, r.ts.list[ii]]
  }
  if (r.ts.list[ii] %in% colnames(rs))
  {
    rts[ , ii] <- rts[ , ii] + rs[, r.ts.list[ii]]
  }
}

colnames(rts) <- r.ts.list

rm(rt, rs, r.herb.list, r.tree.list, r.shrub.list, r.ts.list)

#---#

rb <- t(read_excel("raw data/Материал_ловушки_2.xlsx", sheet = 1, range = "C8:CX59", col_names = F))
rb[is.na(rb)] <- 0

r.beetle.list <- read_excel("raw data/Материал_ловушки_2.xlsx", sheet = 1, range = "B8:B59", col_names = F)[[1]]
colnames(rb) <- r.beetle.list

rb <- rb[, colSums(rb) > 0]

rm(r.beetle.list)

#---#