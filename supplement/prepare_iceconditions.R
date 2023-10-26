.wd <- setwd("..")
source("resources/header.R")
setwd(.wd)
ursa:::.elapsedTime("ready")
stopifnot(!file.exists("../quickload/iceCover.rds"))
toTest <- staffOnly
if (F) {
   iceConcCover(CF=4069,industry="IOT")$concern$available
   ursa:::.elapsedTime("atonce")
   q()
}
season <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
res <- expand.grid(industry=industryCode(),CF=CFCode()
                  ,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
m <- array(NA_real_,dim=c(nrow(res),length(season)),dimnames=list(NULL,season))
if (toTest)
   m[] <- runif(prod(dim(m)),min=0,max=1)
res <- cbind(res,m)
series(res)
pb <- ursaProgressBar(nrow(res))
for (i in seq(nrow(res)) |> sample()) {
   setUrsaProgressBar(pb)
   if (toTest & !res$CF[i] %in% c(2046))
      next
   res[i,season] <- iceConcCover(CF=res$CF[i],industry=res$industry[i])$concern$available
   print(res[i,],digits=2)
  # a <- cvr$concern$available
  # print(a)
}
close(pb)
saveRDS(res,"../quickload/iceCover.rds")
#str(a)
