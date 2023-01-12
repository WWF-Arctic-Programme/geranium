invisible(Sys.setlocale("LC_TIME","C"))
staffOnly <- F & nchar(Sys.getenv("MSOFFICE"))>0
sessionFile <- "quickload/session.Rdata"
rdstoken <- file.path("quickload/dropbox-token.rds")
isRemote <- ((!staffOnly)&&(file.exists(rdstoken)))
# isRemote <- file.exists(rdstoken)
quickStart <- FALSE
if ((!file.exists(sessionFile))||(file.size(sessionFile)<1024))
   prm_download(sessionFile)
if (file.exists(sessionFile)) {
   a <- load(sessionFile)
   if (("dummy" %in% a)&&(dummy=="rebuild")) {
      quickStart <- FALSE
   } else {
      quickStart <- file.size(sessionFile)>=1024
   }
}
# mdname <- "./compatibility assessment_all_2021-04-05-fixed.xlsx"
# mdname <- "requisite/compatibility assessment_all_2021-05-24-seasons.xlsx"
# mdname <- "requisite/compatibility assessment_all_2022-08-23.xlsx" 
mdname <- "requisite/compatibility assessment_all_2022-11-18.xlsx"
require(ursa)
requireNamespace("sf")
#plutil::ursula(3)
isShiny <- ursa:::.isShiny()
if (dropboxBoris <- F & isShiny) {
   if (file.exists(mdname)) {
     # md5a <- unname(tools::md5sum(mdname))
      md5a <- digest::digest(readBin(mdname,"raw",10e6))
   } else {
      md5a <- digest::digest(Sys.time())
   }
   if (isRemote | !file.exists(mdname)) {
      mdtemp <- tempfile() # "c:/tmp/mdfile.xlsx" # tempfile()
      download.file("https://www.dropbox.com/scl/fi/v2tg3cfhyoyermj0ebcdq/compatibility-assessment_all_2022-11-18.xlsx?dl=1&rlkey=gtq0rcg9b458v5hapsilkzu00"
                   ,mdtemp,mode="wb")
     # md5b <- unname(tools::md5sum(mdtemp))
      md5b <- digest::digest(readBin(mdtemp,"raw",10e6))
      if (FALSE)
         print(data.frame(md5a=md5a,md5b=md5b,eq=md5a==md5b))
      if (md5a!=md5b) {
         file.rename(mdtemp,mdname)
         if (quickStart)
            quickStart <- FALSE
      }
   }
}
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
kwdLUT <- c('0'=kwdGray,'1'=kwdGreen,'2'=kwdYellow,'3'=kwdRed,'9'=kwdGray)
# clrLUT <- c('0'="grey70",'1'="palegreen",'2'="LemonChiffon",'3'="lightsalmon",'9'="grey70")
# clrLUT <- c('0'="grey70",'1'="LemonChiffon",'2'="BurlyWood",'3'="Salmon",'9'="grey70")
#clrLUT <- c('0'="grey70",'1'="PaleGoldenrod",'2'="BurlyWood",'3'="Salmon",'9'="grey70")
#clrLUT <- c('0'="grey70",'1'="yellow",'2'="orange",'3'="red",'9'="grey70")
clrLUT <- c('0'="grey70",'1'="Khaki",'2'="BurlyWood",'3'="Salmon",'9'="grey70")[c('1','2','3')]
listPD <- list.dirs(path="predefined",recursive=FALSE,full.names=TRUE)
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
configFile <- "quickload/config.json"
commentFile <- "quickload/comments.json"
configCloud <- FALSE
config <- config_init()
mulNA <- config$concern
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
   concernNAR <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '3'))) |>
                 unclass() |> data.frame()
   concernNAY <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '2'))) |>
                 unclass() |> data.frame()
   concernNAG <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '1'))) |>
                 unclass() |> data.frame()
   if (T) {
      concernMissed <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                                 ,function(x) {
         y <- length(which(!is.na(x$value)))
         y[y==0] <- NA
         y
      }) |> unclass() |> data.frame()
      concernNAR <- concernNAR/concernMissed*12
      concernNAY <- concernNAY/concernMissed*12
      concernNAG <- concernNAG/concernMissed*12
   }
   concernNAO <- mulNA[1]*concernNAR
   concernNAC <- mulNA[1]*concernNAR+mulNA[2]*concernNAY+mulNA[3]*concernNAG
   ursa:::.elapsedTime("concern prepare -- finish")
}
if (!quickStart) {
   if (!dir.exists(dirname(sessionFile)))
      dir.create(dirname(sessionFile))
   save(regionSF,regionU,dist2land,blank,cfmeta,rules,puvspr,pu,spec
       ,PAs,half,vulner,industries,comments,concern,concernNAO,concernNAC
       ,file=sessionFile)
   if (isRemote)
      prm_upload(sessionFile)
}
meanNAO <- sum(colSums(concernNAO,na.rm=TRUE))/spatial_count(pu)
meanNAC <- sum(colSums(concernNAC,na.rm=TRUE))/spatial_count(pu)
if (F) {
   sumNAC <- rowSums(concernNAC,na.rm=TRUE)/ncol(concernNAC)/max(concernNAC,na.rm=TRUE)
   sumNAO <- rowSums(concernNAO,na.rm=TRUE)/ncol(concernNAO)/max(concernNAO,na.rm=TRUE)
} else {
   d <- max(config$concern)*12
   sumNAC <- apply(concernNAC,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
   sumNAO <- apply(concernNAO,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
   rm(d)
}
sname <- unique(substr(industryAbbr$abbr,1,1))
ctIndustry <- ursa::cubehelix(length(sname),bright=167,weak=100,hue=1,rotate=270)
names(ctIndustry) <- sname
economyList <- spatial_dir(path="humanuse",recursive=TRUE)
hu <- readxl::read_excel("requisite/Gaps in Human Use data from WWF SIGHT.xlsx"
                        ,skip=2)
colnames(hu)[grep("human.*use.*index",colnames(hu),ignore.case=TRUE)] <- "abbr"
colnames(hu)[grep("human.*use.*name",colnames(hu),ignore.case=TRUE)] <- "industry"
colnames(hu)[grep("sight.*dataset",colnames(hu),ignore.case=TRUE)] <- "economy"
sight <- strsplit(hu$economy,split="\\s*,\\s*")
names(sight) <- hu$abbr
if (isShiny) {
  # removeModal()
}
options(warn=2)
##~ if (F)
   ##~ save(pu,puvspr,concern,concernNAO,concernNAC,industryAbbr,industries
       ##~ ,file="C:/tmp/session-soiga.Rdata")
if ((F)&&(quickload())) { ## DEPRECATED
# if (T) {
   load(sessionFile)
   require(ursa)
  # loadNamespace("ursa")
   loadNamespace("sf")
}
