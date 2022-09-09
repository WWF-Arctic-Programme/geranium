wd <- setwd("..")
source("resources/process.R")
ursa:::.elapsedTime("ready")
use <- unlist(industries)
activity <- unique(gsub("\\d+$","",names(use)))[7]
activity
industry <- use[sample(grep(activity,names(use)),1)]
da <- human_use(industry)
str(da)
