require(ursa)
'f5_rasterizeCF' <- function() {
   pu <- ursa_read("../requisite/dist2land-f.tif")
   names(pu) <- c("id","dist")
   print(pu)
   puvspr <- read.csv("../requisite/puvspr.dat.gz")
   str(puvspr)
   bname <- as.character(sort(unique(puvspr$species)))
   str(bname)
   amount <- ursa(NA_real_,bandname=bname)
  # coverage <- ursa(NA_real_,bandname=bname)
   pb <- ursaProgressBar(bname)
   for (i in sample(seq(amount))) {
      setUrsaProgressBar(pb)
      cf <- puvspr[puvspr$species %in% bname[i],]
      ind <- which(ursa_value(pu["id"]) %in% cf$pu)
      a <- ursa()
      ursa_value(a)[ind] <- cf$amount/max(cf$amount)
      amount[i] <- a
     # a <- ursa()
     # ursa_value(a)[ind] <- cf$coverage
     # coverage[i] <- a
   }
   close(pb)
   write_envi(amount,"../requisite/amount")
  # write_envi(coverage,"coverage")
  # sheets <- readxl::excel_sheets(mdname)
  # print(sheets)
   0L
}
invisible(f5_rasterizeCF())