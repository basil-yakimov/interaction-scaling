
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

rts <- data.frame(rts)
rh <- data.frame(rh)

rm(rt, rs, r.herb.list, r.tree.list, r.shrub.list, r.ts.list)

#---#

# load data matrix, b is for beetles
rb <- t(read_excel("raw data/Материал_ловушки_2.xlsx", sheet = 1, range = "C8:CX59", col_names = F))
rb[is.na(rb)] <- 0

# load taxon list
r.beetle.list <- read_excel("raw data/Материал_ловушки_2.xlsx", sheet = 1, range = "B8:B59", col_names = F)[[1]]

# remove genus equivalents
r.beetle.list <- sub('\\(.*?\\)', "", r.beetle.list)

# remove authors and years
r.beetle.list <- sub("^(\\S*\\s+\\S+).*", "\\1", r.beetle.list)

# remove extra spaces
r.beetle.list <- sub("  ", " ", r.beetle.list)

# attribute latin names
colnames(rb) <- r.beetle.list

# remove composite taxons
rb <- rb[, colSums(rb) > 0]

rb <- data.frame(rb)

rm(r.beetle.list)

#--------------------------------------Chinese mount DongLin data----------------------------------------------#

# load mountain forest data
# c is for chinese mountain forest, tsh stands for tree, shrub and herb layers respectively
ct <- t(read_excel("raw data/Forest_data.xlsx", sheet = 4, range = "B2:CS21", col_names = F))
cs <- t(read_excel("raw data/Forest_data.xlsx", sheet = 5, range = "B2:CS42", col_names = F))
ch <- t(read_excel("raw data/Forest_data.xlsx", sheet = 6, range = "B2:CS194", col_names = F))

# loading and assigning latin to columns
c.tree.list <- read_excel("raw data/Forest_data.xlsx", sheet = 4, range = "A2:A21", col_names = F)[[1]]
c.shrub.list <- read_excel("raw data/Forest_data.xlsx", sheet = 5, range = "A2:A42", col_names = F)[[1]]
c.herb.list <- read_excel("raw data/Forest_data.xlsx", sheet = 6, range = "A2:A194", col_names = F)[[1]]

# remove authors and years
c.tree.list <- sub("^(\\S*\\s+\\S+).*", "\\1", c.tree.list)
c.shrub.list <- sub("^(\\S*\\s+\\S+).*", "\\1", c.shrub.list)
c.herb.list <- sub("^(\\S*\\s+\\S+).*", "\\1", c.herb.list)

# manual update
c.herb.list[40] <- "Bupleurum chinense_f._pekinense"
c.herb.list[54:58] <- paste0("Compositae Sp", 1:5)
c.herb.list[82:87] <- paste0("Gramineae Sp", 1:6)
c.herb.list[179] <- "Thalictrum hypoleucum"


colnames(ct) <- c.tree.list
colnames(cs) <- c.shrub.list
colnames(ch) <- c.herb.list

# joined species list for trees and shrubs
c.ts.list <- unique(c(c.tree.list, c.shrub.list))

# merging tree and shrub data
cts <- matrix(0, nrow = 96, ncol = length(c.ts.list))

for (ii in 1:length(c.ts.list))
{
  if (c.ts.list[ii] %in% colnames(ct))
  {
    cts[ , ii] <- cts[ , ii] + ct[, c.ts.list[ii]]
  }
  if (c.ts.list[ii] %in% colnames(cs))
  {
    cts[ , ii] <- cts[ , ii] + cs[, c.ts.list[ii]]
  }
}

colnames(cts) <- c.ts.list

cts <- cts[, colSums(cts) > 0]

cts <- data.frame(cts)
ch <- data.frame(ch)

rm(ct, cs, c.herb.list, c.tree.list, c.shrub.list, c.ts.list)

#---#

cb <- read_excel("raw data/dls_ele_beetles.xlsx", sheet = 1, range = "B1:U97", col_names = T)
  
cb[is.na(cb)] <- 0

sum(colSums(cb) > 0)

cb <- cb[, colSums(cb) > 0]

#---#

rm(ii)

save.image("clean data/int.sc.2018.rda")
