files <- c("chapter01.Rnw", "chapter02.Rnw", "chapter03.Rnw", "chapter04.Rnw", "chapter05.Rnw")
lapply(files, knitr::knit)
