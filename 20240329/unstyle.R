a <- readLines("assessment2.Rmd")
a <- a[grep("^:::",a,invert=TRUE)]
writeLines(a,"assessment2~.Rmd")
