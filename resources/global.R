invisible(Sys.setlocale("LC_TIME","C"))
sessionFile <- "quickload/session.Rdata"
if (quickStart <- (file.exists(sessionFile))&&(file.size(sessionFile)>1024))
   load(sessionFile)
##~ if (!file.exists("requisite/inventory.rds")) {
   ##~ list1 <- dir(path="output-overlap",pattern="interim\\.tif"
               ##~ ,full.names=TRUE,recursive=TRUE)
   ##~ md5 <- sapply(list1,digest::digest,"crc32")
   ##~ da <- data.frame(ref=md5,src=list1,dst=file.path("requisite",paste0(md5,".tif")))
   ##~ saveRDS(da,"requisite/inventory.rds")
   ##~ file.copy(da$src,da$dst,copy.date=TRUE,overwrite=FALSE)
##~ }
##~ if (check <- !TRUE) {
   ##~ inventory <- readRDS("requisite/inventory.rds")
   ##~ str(inventory)
   ##~ src <- unique(lapply(strsplit(inventory$src,split="/"),function(x) x[2]))
   ##~ str(src)
   ##~ q()
##~ }
require(ursa)
requireNamespace("sf")
#plutil::ursula(3)
isShiny <- ursa:::.isShiny()
# mdname <- "./compatibility assessment_all_2021-04-05-fixed.xlsx"
# mdname <- "requisite/compatibility assessment_all_2021-05-24-seasons.xlsx"
# mdname <- "requisite/compatibility assessment_all_2022-08-23.xlsx" 
mdname <- "requisite/compatibility assessment_all_2022-11-18.xlsx"
pGrYl <- cubehelix(5,light=91,dark=221,weak=220,rich=165,hue=2)
pYlRd <- cubehelix(5,light=91,dark=221,weak=110,rich=165,hue=2,inv=TRUE)
pRd <- cubehelix(5,light=233,dark=91,weak=110,rotate=0,hue=2)
pBl <- cubehelix(5,light=233,dark=91,weak=45,rotate=0,hue=0.2)
p4 <- cubehelix(5,light=221,dark=91,weak=110,rotate=0,hue=2)
p6 <- cubehelix(5,light=221,dark=91,weak=45,rotate=0,hue=2)
pRdT <- paste0(p4,format(as.hexmode(round(seq(15,255,length=length(p4)))),width=2,upper.case=TRUE))
pBlT <- paste0(p6,format(as.hexmode(round(seq(15,255,length=length(p6)))),width=2,upper.case=TRUE))
p5 <- cubehelix(5,light=221,dark=91,weak=165,rich=165,hue=2)
pYlT <- paste0(p5,format(as.hexmode(round(seq(15,255,length=length(p5)))),width=2,upper.case=TRUE))
#pGrYlRd <- c(pGrYl,tail(pYlRd,-1))
pBase <- c('green'="#068400",'yellow'="#fdfc00",'red'="#af0401",'extra'=c("#362978","#000000")[2])
pGrYlRd <- colorRampPalette(pBase[1:3])(9)
pRd <- paste0(pBase[3],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pBl <- paste0(pBase[4],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pGr <- paste0(pBase[1],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pRd2 <- paste0(pBase[3],format(as.hexmode(round(seq(15,151,length=5))),width=2,upper.case=TRUE))
pBl2 <- paste0(pBase[4],format(as.hexmode(round(seq(15,151,length=5))),width=2,upper.case=TRUE))
pYlBase <- colorRampPalette(c(pBase[2],"#af9f00"))(5)
pYl <- paste0(pYlBase,format(as.hexmode(round(seq(15,255,length=length(pYlBase))))
                            ,width=2,upper.case=TRUE))
pYlRd <- pGrYlRd[c(5,8)]
height <- 600
retina <- 2
kwdRed <- "Incompatible"
kwdYellow <- c("Concessional","Conditional","Compatible under certain conditions")[2]
kwdGreen <- c("Compatible","Compatible/not applicable")[2]
kwdGray <- "Not applicable"
allActivity <- "All human use"
noneActivity <- "No human use"
nameAllCF <- "All conservation features"
groupList <- c('\\d'=nameAllCF
              ,'1'="Walrus"
              ,'2'="Pinnipeds"
             # ,'3'="Sea ice"
              ,'4'="Fishes"
              ,'5'="Cetaceans"
              ,'6'="Birds"
              ,'7'="Benthos"
              ,'8'="Coastal"
              ,'9'="Polar bears"
              )
##~ kwdRed <- "'2'"
##~ kwdYellow <- "'1'"
##~ kwdGreen <- "'0'"
kwdLUT <- c('0'=kwdGreen,'1'=kwdYellow,'2'=kwdRed,'9'=kwdGray)
listPD <- list.dirs(path="predefined",recursive=FALSE,full.names=TRUE)
mulNAO <- 3
basename(listPD)
if (!quickStart) {
   regionSF <- vector("list",length(listPD))
   names(regionSF) <- basename(listPD)
   regionU <- regionSF
   for (i in seq_along(listPD)) {
      reg <- ursa:::spatialize(spatial_dir(path=listPD[i]),engine="sf",crs=4326)
      if (length(ind <- which(sapply(as.list(spatial_data(reg)),class) %in% "character"))) {
         reg <- reg[spatial_fields(reg)[ind[1]]]
      }
      else {
         reg <- reg[spatial_fields(reg)[1]]
         if (length(ind <- grep("^PAC",colnames(reg)))) {
            reg[[ind]] <- paste0("PAC ",reg[[ind]])
         }
      }
      spatial_fields(reg) <- "region"
      reg <- reg[!is.na(reg$region),]
      regionSF[[i]] <- reg
   }
}
session_grid(NULL)
if (!quickStart) {
   dist2land <- ursa_read("requisite/dist2land-f.tif")
   rules <- jsonlite::fromJSON("requisite/buffer-rules.json")
   blank <- (!is.na(dist2land["ID"]))-1L
}
#rules <- jsonlite::fromJSON("buffer-rules.json")
#dist2land <- ursa_read("dist2land-f.tif")
#blank <- (!is.na(dist2land["ID"]))-1L
ref <- polygonize(blank,engine="sf")
ref$ID <- seq(spatial_count(ref))
cell <- ursa(dist2land["dist"],"cell")*1e-3
nameInit <- "---"
seasonList <- c("Annual maximum"
               ,format(seq(as.Date("2020-01-15"),length.out=12,by="1 month"),"%B"))[]
methodList <- c('overlap'=paste(sQuote(kwdRed),"colors overwrite",sQuote(kwdYellow),"colors")
               ,'threat'=paste("Accentuated",sQuote(kwdRed),"palette")
               ,'mix'=paste(sQuote(kwdRed),"and",sQuote(kwdYellow),"mixed colors")
               ,'source'=paste(sQuote(kwdRed),"and","source")
               )
nameAllHuman <- "All human use"
sepRules <- " » "
sepRules <- iconv(sepRules,to="UTF-8")
pattRules <- paste0("^(.+\\S)",gsub("\\s","\\\\s",sepRules),"(\\S.+)$")
#activity <- unique(gsub("(.+\\S)\\s*»\\s*(\\S.+)","\\1",rules$activity)) ## deprecated
activity <- unique(gsub(pattRules,"\\1",rules$activity))
activity <- c(noneActivity,allActivity,activity)
options(spinner.color="#ECF0F5")
nameSelector <- "Selector"
nameEditor <- "Editor"
nameClick <- "Click region(s) on map"
cell <- ursa(dist2land["dist"],"cell")*1e-3
if (F)
   clf <- compose_coastline(spatial_transform(spatial_read("crop-coast.geojson"),6931)
                           ,fill="grey90",col="grey70")
# pac0 <- ursa:::.fasterize(pacSF) #"requisite/PACs33_Gridded_IDs.shp")
# ursa:::.elapsedTime("fasterize -- start")
if (!quickStart) {
   for (i in seq_along(listPD)) {
      fileout <- file.path("predefined",names(regionSF)[i],"region")
      if (!envi_exists(fileout)) {
         regionU[[i]] <- ursa:::.fasterize(regionSF[[i]])
         ursa_write(regionU[[i]],fileout)
      }
      else
         regionU[[i]] <- read_envi(fileout)
   }
}
# ursa:::.elapsedTime("fasterize -- finish")
#pacname
#regionSF[["PACs"]][[1]]
ongoing <- "This is verbatim info"
amountFile <- "requisite/amount"
if (!quickStart) {
   cfmeta <- lapply(readxl::excel_sheets(mdname),function(sheet) {
      readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")[,1:2]
   }) |> do.call(rbind,args=_) |> unique()
   cfmeta <- cfmeta[with(cfmeta,order(CF_code,CF_name)),]
   cfmeta$label <- paste0(cfmeta[["CF_code"]]," - ",cfmeta[["CF_name"]])
   ursa:::.elapsedTime("marxan inputs reading -- start")
   puvspr <- read.csv("requisite/puvspr.dat.gz")
   puvspr <- by(puvspr,puvspr$species,simplify=FALSE,function(cf) {
      minv <- min(cf$amount)
      maxv <- max(cf$amount)
      if (maxv+minv==0)
         cf$amount <- 0
      else if (minv-maxv==0)
         cf$amount <- 1
      else
         cf$amount <- (cf$amount-minv)/(maxv-minv)
      cf
   }) |> do.call(rbind,args=_)
   spec <- by(puvspr,puvspr$species,function(x) {
      data.frame(cf=x$species[1],amount=sum(x$amount))
   }) |> do.call(rbind,args=_)
   pu <- spatial_read("requisite/pulayer")
   sf::st_agr(pu) <- "constant"
   PAs <- spatial_read("requisite/PAs_union1.shp") |> spatial_transform(4326)
   sf::st_agr(PAs) <- "constant"
   if (length(ind <- which(!spatial_valid(PAs,each=TRUE))))
      PAs <- sf::st_make_valid(PAs[ind,])
   ursa:::.elapsedTime("marxan inputs reading -- finish")
   half <- median(spatial_area(pu))/2
}
industryAbbr <- read.csv("requisite/industry.csv")
industryAbbr <- industryAbbr[order(industryAbbr$abbr),]
rownames(industryAbbr) <- NULL
patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
if (!quickStart) {
   vulner <- vector("list",nrow(rules))
   names(vulner) <- rules$activity
   comments <- vulner
   activity <- readxl::excel_sheets(mdname)
   industries <- vector("list",length(activity))
   names(industries) <- activity
   for (sheet in activity |> sample()) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      v <- as.data.frame(v)
      cname <- colnames(v)
      vname <- paste0(sheet,sepRules,cname)
      ind <- vname %in% rules$activity
      v2 <- v[,!ind]
      ind2 <- grep("^comment",colnames(v2),ignore.case=TRUE,invert=!TRUE)
      if (hasComment <- length(ind2)>0) {
         v4 <- v2[,ind2]
         v4name <- colnames(v4)
         v4abbr <- gsub(".*_(.+$)","\\1",v4name)
         v2 <- v2[,-ind2]
      }
      industries[[sheet]] <- cname[ind]
      v2$industry <- NA
      v2$value <- NA
      ind <- which(ind)
      for (i in seq_along(ind) |> sample()) {
         iname <- vname[ind[i]]
         v2$industry <- iname
         if (hasComment) {
            v3 <- v2
            v3$value <- v4[,v4name[i]]
            comments[[match(vname[ind[i]],rules$activity)]] <- v3
         }
        # v2$value <- v[,ind[i]][[1]]
         val <- v[,ind[i]]
         val[val>9] <- NA
         v2$value <- as.integer(val)
         vulner[[match(vname[ind[i]],rules$activity)]] <- v2
      }
   }
   vulner <- do.call(rbind,vulner)
   vulner <- vulner[which(!is.na(vulner$'Limitations')),]
   comments <- do.call(rbind,comments)
   colnames(comments)[grep("^value",colnames(comments),ignore.case=TRUE)] <- "Comment"
   vulner$CF_code <- as.integer(vulner$CF_code)
   comments$CF_code <- as.integer(comments$CF_code)
}
scenarioCF <- read.csv(dir(path="requisite",pattern="scenario.*\\.csv$"
                          ,ignore.case=TRUE,full.names=TRUE),check.names=FALSE)
rownames(scenarioCF) <- NULL
if (!quickStart) {
   ursa:::.elapsedTime("concern prepare -- start")
   concern <- lapply(seq_len(nrow(vulner)),function(i) {
      m <- monthList(vulner$'Limitations'[i])
      v <- vulner[rep(i,length(m)),c("CF_code","industry","value"),drop=FALSE]
      v$month <- m
      v
   }) |> do.call(rbind,args=_)
   concern$industry <- factor(industryCode(concern$industry))
   concernNAO <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% "2"))) |>
                 unclass() |> data.frame()
   concernNAC <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% "1"))) |>
                 unclass() |> data.frame()
   concernNAC <- concernNAC+mulNAO*concernNAO
   ursa:::.elapsedTime("concern prepare -- finish")
}
if (!quickStart) {
   if (!dir.exists(dirname(sessionFile)))
      dir.create(dirname(sessionFile))
   save(regionSF,regionU,dist2land,blank,cfmeta,rules,puvspr,pu,spec
       ,PAs,half,vulner,industries,comments,concern,concernNAO,concernNAC
       ,file=sessionFile)
}
meanNAO <- sum(colSums(concernNAO,na.rm=TRUE))/spatial_count(pu)
meanNAC <- sum(colSums(concernNAC,na.rm=TRUE))/spatial_count(pu)
configFile <- "quickload/config.json"
commentFile <- "quickload/comments.json"
if (!file.exists(configFile)) {
  config <- list(comment=TRUE,sleepValue=500)
  writeLines(jsonlite::toJSON(config),configFile)
}
config <- jsonlite::fromJSON(configFile)
if (isShiny) {
  # removeModal()
}
options(warn=2)
##~ if (F)
   ##~ save(pu,puvspr,concern,concernNAO,concernNAC,industryAbbr,industries
       ##~ ,file="C:/tmp/session-soiga.Rdata")
if ((F)&&(quickload())) {
# if (T) {
   load("quickload/session.Rdata")
   require(ursa)
  # loadNamespace("ursa")
   loadNamespace("sf")
}
