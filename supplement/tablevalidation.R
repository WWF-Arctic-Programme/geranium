wd <- setwd("..")
source("resources/header.R")
ursa:::.elapsedTime("ready")
use <- unlist(industries)
activity <- unique(gsub("\\d+$","",names(use)))[6]
activity
industry <- use[sample(grep(activity,names(use)),1)]
da <- human_use(industry)
str(da)
