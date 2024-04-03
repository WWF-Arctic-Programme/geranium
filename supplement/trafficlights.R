'interimMapDev' <- function(industry="Mass tourism",group="\\d",season="max") {
  # activity <- names(industries)[sapply(industries,function(a) industry %in% a)]
  # industry <- paste0(activity,sepRules,industry)
  # v <- v[grep("^Tourism",v$industry),]
  # v <- v[v$industry %in% "Tourism » Mass tourism",]
   v <- vulner[which(!is.na(match(gsub(pattRules,"\\2",vulner$industry),industry))),]
  # v <- vulner[grep(industry,vulner$industry,ignore.case=TRUE),]
   str(v)
   q()
   print(table(v$value))
   q()
   v <- aggregate(list(value=v$value)
                 ,by=list(CF_code=v$CF_code,industry=v$industry),max)
   s <- aggregate(list(count=v$CF_code),by=list(CF_code=v$CF_code,value=v$value),length)
   ursa:::.elapsedTime("incompatibility -- start")
   res <- by(puvspr,puvspr$pu,function(x) {
      ind <- match(x$species,s$CF_code)
      s2 <- s[ind,]
      s2$amount <- x$amount
      y <- by(s2,s2$value,simplify=!FALSE,function(x) sum(x$count*x$amount))
      if (F)
         ret <- data.frame('0'=0,'1'=0,'2'=0,check.names=F)
      else
         ret <- c('0'=0,'1'=0,'2'=0)
      ret[names(y)] <- y
      if (is.data.frame(ret))
         ret$pu <- x$pu[1]
      ret
   }) |> do.call(rbind,args=_) |> data.frame(check.names=FALSE)
   ursa:::.elapsedTime("incompatibility -- finish")
  # session_grid("requisite/amount")
  # pu <- spatial_read("requisite/pulayer")
   if (F) {
      res2 <- pu
      spatial_data(res2) <- data.frame('0'=0,'1'=0,'2'=0,check.names=FALSE)
      ind1 <- match(rownames(res),pu$ID)
      print(summary(ind1))
      ind2 <- match(pu$ID,rownames(res))
      print(summary(ind2))
      spatial_data(res2[ind1,]) <- res
   }
   else {
      res2 <- pu[which(!is.na(match(as.character(pu$ID),rownames(res)))),]
      spatial_data(res2) <- res
   }
   session_grid("requisite/amount")
   allocate(spatial_centroid(res2))
}
.wd <- setwd("..")
if (T) {
   source("resources/header.R")
} else {
   mdname <- "requisite/compatibility assessment_all_2021-05-24-seasons.xlsx"
   rules <- jsonlite::fromJSON("requisite/buffer-rules.json")
   sepRules <- " » "
   sepRules <- iconv(sepRules,to="UTF-8")
   pattRules <- paste0("^(.+\\S)",gsub("\\s","\\\\s",sepRules),"(\\S.+)$")
   vulner <- vector("list",nrow(rules))
   names(vulner) <- rules$activity
   activity <- readxl::excel_sheets(mdname)
   industries <- vector("list",length(activity))
   names(industries) <- activity
   patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
   for (sheet in activity |> sample()) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      v <- as.data.frame(v)
      cname <- colnames(v)
      vname <- paste0(sheet,sepRules,cname)
      ind <- vname %in% rules$activity
      v2 <- v[,!ind]
      industries[[sheet]] <- cname[ind]
      v2$industry <- NA
      v2$value <- NA
      ind <- which(ind)
      for (i in seq_along(ind) |> sample()) {
         iname <- vname[ind[i]]
         v2$industry <- iname
         v2$value <- v[,ind[i]][[1]]
         val <- v[,ind[i]]
         val[val>9] <- NA
         v2$value <- as.integer(val)
         vulner[[match(vname[ind[i]],rules$activity)]] <- v2
      }
   }
   vulner <- do.call(rbind,vulner)
   vulner$CF_code <- as.integer(vulner$CF_code)
   plutil::ursula()
}
ilist <- unname(unlist(industries))
md5 <- sapply(ilist,digest::digest,"crc32")
fname <- file.path("trafficlights",paste0("t",md5))
if (length(ind <- which(as.logical(sapply(fname,envi_exists))))) {
   ilist <- ilist[-ind]
   fname <- fname[-ind]
}
#iname <- "Mass tourism"
pb <- ursaProgressBar(ilist)
for (i in seq_along(ilist) |> sample()) {
   setUrsaProgressBar(pb)
   print(ilist[i])
   a <- try(interimMap(industry=ilist[i]))
   if (inherits(a,"try-error"))
      next
   ignorevalue(a) <- -99
   write_envi(a,fname[i])
  # ursa_write(a,paste0(fname,".tif"))
}
close(pb)
setwd(.wd)
