.wd <- setwd("..")
source("resources/header.R")
setwd(.wd)
ursa:::.elapsedTime("ready")
if (F) {
   iceConcCover(CF=4069,industry="IOT")$concern$available
   ursa:::.elapsedTime("atonce")
   q()
}
dst <- "../quickload/iceCover.rds"
toTest <- F & staffOnly
season <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
if (T & file.exists(dst)) {
   res <- readRDS(dst)
} else {
  # stop("rebuild")
   res <- expand.grid(industry=industryCode(),CF=CFCode()
                     ,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
   m <- array(NA_real_,dim=c(nrow(res),length(season)),dimnames=list(NULL,season))
   if (toTest)
      m[] <- runif(prod(dim(m)),min=0,max=1)
   res <- cbind(res,m)
}
series(res)
pb <- ursaProgressBar(nrow(res))
for (i in seq(nrow(res)) |> sample()) {
   setUrsaProgressBar(pb)
   if (toTest & !res$CF[i] %in% c(2046))
      next
   if (!res$industry[i] %in% c("AM","AC"))
      next
   cat("----------------------\n")
   print(res[i,],digits=2)
   res[i,season] <- iceConcCover(CF=res$CF[i],industry=res$industry[i])$concern$available
   print(res[i,],digits=2)
  # a <- cvr$concern$available
  # print(a)
}
close(pb)
saveRDS(res,"../quickload/iceCover.rds")
#str(a)
