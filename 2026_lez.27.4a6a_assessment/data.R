## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(ggplot2)
taf.bootstrap()

mkdir("data")

# load("boot/initial/data/lez.4a6a_Index.RData")
# write.csv(ind_spring, "boot/initial/data/lez.4a6a_Index_Spring.csv")
# write.csv(ind_autumn, "boot/initial/data/lez.4a6a_Index_Autumn.csv")

#### Load data ####

megC <- read.taf("boot/initial/data/lez4a6a_catch.csv")
megI1 <- read.taf("boot/initial/data/lez.4a6a_Index_Spring.csv")
megI2 <- read.taf("boot/initial/data/lez.4a6a_Index_Autumn.csv")


# set up DATA.bib
draft.data(
  data.files = "lez4a6a_catch.csv",
  originator = "WGCSE",
  year = 2026,
  title = "lez4a6a_Catch",
  period = "1991-2025",
  file = TRUE
)

draft.data(
  data.files = "lez.4a6a_Index_Spring.csv",
  originator = "WGCSE",
  year = 2026,
  title = "lez.4a6a Spring Survey Index",
  period = "2005-2025",
  file = TRUE,
  append = T
)

draft.data(
  data.files = "lez.4a6a_Index_Autumn.csv",
  originator = "WGCSE",
  year = 2026,
  title = "lez.4a6a Spring Autumn Index",
  period = "2005-2025",
  file = TRUE,
  append = T
)


write.taf(megC, dir = "data")
write.taf(megI1, dir = "data")
write.taf(megI2, dir = "data")
