if (ursa:::.argv0name()=="global")
   stop("direct run is forbidden")
invisible(Sys.setlocale("LC_TIME","C"))
root <- here::here()
staffOnly <- T & nchar(Sys.getenv("MSOFFICE"))>0
sessionFile <- "quickload/session.Rdata"
vulnerFile <- paste0("quickload/vulner",seq(4),".rds")
rdstoken <- file.path(root,"quickload/dropbox-token.rds")

iceCover <- if (file.exists(iceCoverFile <- file.path(root 
                           ,"quickload/iceCover.rds"))) readRDS(iceCoverFile) else NULL
useNACR <- FALSE
ignoreMissing <- TRUE
quickStart <- FALSE
count <- 0L
showHist <- T | !staffOnly
showComment <- TRUE
if ((!staffOnly)&&((!file.exists(sessionFile))||(file.size(sessionFile)<1024))) {
   isRemote <- TRUE ## only for 'sessionFile' download 
   prm_download(sessionFile)
}
if (file.exists(sessionFile)) {
   a <- load(sessionFile)
  # str(concern)
  # str(vulner)
  # str(comments)
   if (("dummy" %in% a)&&(dummy=="rebuild")) {
      quickStart <- FALSE
   } else {
      quickStart <- file.size(sessionFile)>=1024
   }
}
isRemote <- ((!staffOnly & T)&&(file.exists(rdstoken)))
# isRemote <- file.exists(rdstoken)
# mdname <- "./compatibility assessment_all_2021-04-05-fixed.xlsx"
# mdname <- "requisite/compatibility assessment_all_2021-05-24-seasons.xlsx"
# mdname <- "requisite/compatibility assessment_all_2022-08-23.xlsx" 
# mdname <- "requisite/compatibility assessment_all_CURRENT_WORKING.xlsx"
mdname <- file.path(root,"requisite/compatibility assessment_all_2022-11-18.xlsx")
require(ursa)
requireNamespace("sf")
#plutil::ursula(3)
isShiny <- ursa:::.isShiny()
if (dropboxBoris <- T & isShiny) {
   if (file.exists(mdname)) {
      ntu <- as.numeric(difftime(Sys.time(),file.mtime(mdname)),units="hours")>12
     # md5a <- unname(tools::md5sum(mdname))
      md5a <- digest::digest(readBin(mdname,"raw",10e6))
   } else {
      ntu <- TRUE
      md5a <- digest::digest(Sys.time())
   }
   if ((ntu)&&(isRemote | !file.exists(mdname))) {
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
      Sys.setFileTime(mdname,Sys.time())
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
configFile <- file.path(root,"quickload/config.json")
commentFile <- file.path(root,"quickload/comments.json")
configCloud <- FALSE
config <- config_init()
mulNA <- config$concern
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
p2YlRd <- pGrYlRd[c(5,8)]
height <- 600
retina <- 2
kwdRed <- "Incompatible"
kwdYellow <- c("Concessional","Conditional","Compatible under certain conditions")[2]
kwdGreen <- c("Compatible","Compatible/not applicable")[2]
kwdGray <- "Not applicable"
allActivity <- "All groups"
noneActivity <- "No human use"
nameAllCF <- "All conservation features"
nameAllSeason <- c("Annual","Annual maximum")[1]
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
names(clrLUT) <- trafficValue(names(clrLUT))
listPD <- list.dirs(path="predefined",recursive=FALSE,full.names=TRUE)
#basename(listPD)
sepRules <- " » "
sepRules <- iconv(sepRules,to="UTF-8")
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
   dist2land <- ursa_read(file.path(root,"requisite/dist2land-f.tif"))
  # rules <- jsonlite::fromJSON("requisite/buffer-rules.json")
   rules <- read.csv(file.path(root,"requisite/industry_conditions.csv"))
   rules$unique <- with(rules,paste0(activity,sepRules,industry))
   blank <- (!is.na(dist2land["ID"]))-1L
}
#rules <- jsonlite::fromJSON("buffer-rules.json")
#dist2land <- ursa_read("dist2land-f.tif")
#blank <- (!is.na(dist2land["ID"]))-1L
ref <- polygonize(blank,engine="sf")
ref$ID <- seq(spatial_count(ref))
cell <- ursa(dist2land["dist"],"cell")*1e-3
nameInit <- "---"
seasonList <- c(nameAllSeason
               ,format(seq(as.Date("2020-01-15"),length.out=12,by="1 month"),"%B"))[]
methodList <- c('overlap'=paste(sQuote(kwdRed),"colors overwrite",sQuote(kwdYellow),"colors")
               ,'threat'=paste("Accentuated",sQuote(kwdRed),"palette")
               ,'mix'=paste(sQuote(kwdRed),"and",sQuote(kwdYellow),"mixed colors")
               ,'source'=paste(sQuote(kwdRed),"and","source")
               )
nameAllHuman <- allActivity
pattRules <- paste0("^(.+\\S)",gsub("\\s","\\\\s",sepRules),"(\\S.+)$")
# activity <- unique(gsub("(.+\\S)\\s*»\\s*(\\S.+)","\\1",rules$activity)) ## deprecated
# activity <- unique(gsub(pattRules,"\\1",rules$activity))
activity <- c(noneActivity,allActivity,rules$activity)
options(spinner.color="#ECF0F5")
nameSelector <- "Selector"
nameEditor <- "Editor"
nameClick <- "Click region(s) on map"
choiceMap <- c('Do not show'="none"
              ,'CAPR'="capr"
              ,'MNSR'="nacr"
              ,'SR'="naor"
              ,'Industrial activities acceptability'="trafficlight"
              ,'Industrial activities amount'="humanuse"
              )

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
   puvspr <- read.csv(file.path(root,"requisite/puvspr.dat.gz"))
   puvspr <- by(puvspr,puvspr$species,simplify=FALSE,function(cf) {
      minv <- min(cf$amount)
      maxv <- max(cf$amount)
      if (maxv+minv==0)
         cf$amount <- 0
      else if (minv-maxv==0)
         cf$amount <- 1
      else {
         if (T)
            minv <- 0
         cf$amount <- (cf$amount-minv)/(maxv-minv)
      }
      cf
   }) |> do.call(rbind,args=_)
   spec <- by(puvspr,puvspr$species,function(x) {
      data.frame(cf=x$species[1],amount=sum(x$amount))
   }) |> do.call(rbind,args=_)
   session_grid(NULL)
   pu <- spatial_read(file.path(root,"requisite/pulayer"))
   sf::st_agr(pu) <- "constant"
   PAs <- spatial_read(file.path(root,"requisite/PAs_union1.shp")) |> spatial_transform(4326)
   sf::st_agr(PAs) <- "constant"
   if (length(ind <- which(!spatial_valid(PAs,each=TRUE))))
      PAs <- sf::st_make_valid(PAs[ind,])
   ursa:::.elapsedTime("marxan inputs reading -- finish")
   half <- median(spatial_area(pu))/2
}
industryAbbr <- read.csv(file.path(root,"requisite/industry_conditions.csv"))
industryAbbr <- industryAbbr[order(industryAbbr$abbr),]
rownames(industryAbbr) <- NULL
industries <- by(industryAbbr$industry,industryAbbr$activity,function(x) x)
iname <- names(industries)
attributes(industries) <- NULL
names(industries) <- iname
patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
if (F & !quickStart) { ## vulner is deprecated
   vulner <- vector("list",nrow(rules))
   names(vulner) <- rules$unique
   comments <- vulner
   activity <- readxl::excel_sheets(mdname)
   industries <- vector("list",length(activity))
   names(industries) <- activity
   for (sheet in activity |> sample()) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      v <- as.data.frame(v)
      cname <- colnames(v)
      vname <- paste0(sheet,sepRules,cname)
      ind <- vname %in% rules$unique
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
            comments[[match(vname[ind[i]],rules$unique)]] <- v3
         }
        # v2$value <- v[,ind[i]][[1]]
         val <- v[,ind[i]]
         val[val>9] <- NA
         v2$value <- as.integer(val)
         vulner[[match(vname[ind[i]],rules$unique)]] <- v2
      }
   }
   if (F) {
      a <- lapply(vulner,dim)
      names(a) <- substr(names(a),1,48)
      str(a)
      q()
   }
   vulner <- do.call(rbind,vulner)
   vulner <- vulner[which(!is.na(vulner$'Limitations')),]
   comments <- do.call(rbind,comments)
   colnames(comments)[grep("^value",colnames(comments),ignore.case=TRUE)] <- "Comment"
   vulner$CF_code <- as.integer(vulner$CF_code)
   comments$CF_code <- as.integer(comments$CF_code)
}
#scenarioCF <- read.csv(dir(path="requisite",pattern="scenario.*\\.csv$"
#                          ,ignore.case=TRUE,full.names=TRUE),check.names=FALSE)
scenarioCF <- readxl::read_excel(dir(path=file.path(root,"requisite")
                                    ,pattern="Table.*ArcNet.*CFs.*\\.xlsx$"
                                    ,ignore.case=TRUE,full.names=TRUE)
                                ,.name_repair="minimal")
rownames(scenarioCF) <- NULL
scenarioCF <- scenarioCF[,nchar(colnames(scenarioCF))>0]
scenarioCF$CF_name <- gsub("\\s\\("," (<em>",scenarioCF$CF_name)
scenarioCF$CF_name <- gsub("\\)","</em>)",scenarioCF$CF_name)
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
if (F & !quickStart) { ## vulner is deprecated
   ursa:::.elapsedTime("concern prepare -- start")
   stop("HERE")
   concern <- lapply(seq_len(nrow(vulner)),function(i) {
      m <- monthList(vulner$'Limitations'[i])
      v <- vulner[rep(i,length(m)),c("CF_code","industry","value"),drop=FALSE]
      v$month <- m
      v
   }) |> do.call(rbind,args=_)
   concern$value[concern$value==0] <- NA
   concern$industry <- factor(industryCode(concern$industry))
   ursa:::.elapsedTime("concern prepare -- finish")
   rHU <- huAmount()
}
if (T & !quickStart) {
   ursa:::.elapsedTime("concern prepare -- start")
   sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%B")
   list1 <- dir(path="requisite",pattern="^concernAssessment-.+\\.xlsx$"
               ,full.names=TRUE)
   a4 <- lapply(list1 |> sample(),\(fname) {
      list2 <- readxl::excel_sheets(fname)
      a3 <- lapply(list2 |> sample(),\(sheet) {
         if (!isShiny)
            cat(sheet,"")
         a <- readxl::read_excel(fname,sheet=sheet,.name_repair="minimal")
         a2 <- lapply(sname |> sample(),\(m) {
            cname <- grep(m,colnames(a),value=TRUE,ignore.case=TRUE)
            nsymbol <- nchar(cname)
            if (!length(nsymbol))
               return(NULL)
            res <- data.frame(CF_code=a$CF_code
                             ,industry=sheet
                             ,value=a[[cname[which.min(nsymbol)]]]
                             ,month=match(m,sname)
                             ,comment=a[[cname[which.max(nsymbol)]]]
                             )
            res
         }) |> do.call(rbind,args=_)
         a2
      }) |> do.call(rbind,args=_)
   }) |> do.call(rbind,args=_)
   if (!isShiny)
      cat("\n")
   a4$CF_code <- as.integer(a4$CF_code)
  # str(a4$value)
  # print(table(a4$value))
   a4$value <- as.integer(a4$value)
  # str(a4$value)
  # print(table(a4$value))
   a4$value[!(a4$value %in% c(1,2,3))] <- NA
   a4$industry <- factor(a4$industry)
  # a4$comment <- paste0(substr(a4$comment,1,5),"...")
   a4 <- a4[with(a4,order(CF_code,industry,month)),]
   if (!isShiny) {
      if (exists("concern"))
         str(concern)
      str(a4)
      if (exists("concern"))
         print(table(concern$value))
      print(table(a4$value))
   }
   concern <- a4
   ursa:::.elapsedTime("concern prepare -- finish")
  # saveRDS(concern,"C:/tmp/concern.rds")
   rm(a4)
   rHU <- huAmount()
   vulner <- NULL
   if (T) {
      verbose <- T
      rule <- read.csv(file.path(root,"requisite/industry_conditions.csv"))
     # rule <- rules[rules$abbr %in% industry,]
      rule$manual <- rule$unique <- rule$activity <- rule$industry <- NULL
     # rule$industry <- substr(rule$industry,1,16)
      r <- rule
      colnames(r)[grep("^abbr",colnames(r))] <- "industry"
      if (verbose) {
         cat("r:\n")
         str(r)
      }
      v <- concern
     # v <- concernSubset(concern,activity=prm$industry,group=prm$group,season=prm$season)
      v <- v[!is.na(v$value),]
      v <- aggregate(list(value=v$value)
                    ,by=list(CF_code=v$CF_code
                            ,industry=v$industry
                           # ,value=v$value
                           # ,month=v$month
                            )
                    ,function(x) {
         x <- na.omit(x)
         if (!length(x))
            return(NA)
         max(x)
      })
      colnames(v)[grep("^CF",colnames(v))] <- "species"
      v$industry <- factor(v$industry,levels=levels(concern$industry))
      if (verbose) {
         cat("v:\n")
         str(v)
      }
      w <- spatial_data(pu)
      colnames(w)[grep("^ID",colnames(w))] <- "pu"
      w <- w[,c("pu","coast","depth")]
      if (verbose) {
         cat("w:\n")
         str(w)
      }
      u <- puvspr[,c("species","pu","amount")]
      if (verbose) {
         cat("u:\n")
         str(u)
      }
      ursa:::.elapsedTime("w+r")
      res1 <- merge(w,r)
      rm(w,r)
      ursa:::.gc(TRUE)
      print("0701a")
     # str(res1)
     # print(object.size(res1))
      if (TRUE) ## `FALSE` should be identical to `interimMap()`
         res1 <- res1[res1$depth<=res1$maxDepth & res1$depth>=res1$minDepth &
                      res1$coast>=res1$minCoast*cell & res1$coast<=res1$maxCoast*cell,]
      print("0701b")
     # str(res1)
     # print(object.size(res1))
     # res1 <- res1[,grep("^(min|max)",colnames(res1),invert=TRUE)]
      print("0701c")
      str(res1)
      res1 <- res1[,c("pu","industry")]
      print("0701d")
      res1$industry <- factor(res1$industry,levels=levels(concern$industry))
      print("0701e")
     # cat("res1:\n")
     # str(res1)
     # print(object.size(res1))
      ursa:::.elapsedTime("u+v")
      print("0701f")
      res2 <- merge(u,v)
      rm(u,v)
      ursa:::.gc(TRUE)
      ursa:::.elapsedTime("u+v merged")
      str(res2)
      res2 <- res2[with(res2,paste0(industry,pu)) %in% with(res1,paste0(industry,pu)),]
      str(res2)
      print("0701g 'merge(u,v)'")
      rm(res1)
      ursa:::.elapsedTime("u+w+v+r -- done")
      res2$value <- NULL
      vulner <- res2
      if (T) {
         upu <- sort(unique(vulner$pu))
         group <- seq(length(vulnerFile))
         ind <- sample(group,length(upu),replace=TRUE)
         ret <- lapply(group,\(i) {
            j <- which(vulner$pu %in% upu[ind==i])
            saveRDS(vulner[j,],vulnerFile[i])
         })
         vulner <- NULL
      }
   }
}
if (!quickStart) {
   if (!dir.exists(dirname(sessionFile)))
      dir.create(dirname(sessionFile))
   save(regionSF,regionU,dist2land,blank,cfmeta,rules,puvspr,pu,spec
       ,PAs,half
      # ,vulner
     # ,comments ## deprecated
       ,concern,rHU
      # ,concernNAO,concernNAC
       ,file=sessionFile)
   if ((isRemote)&&(T | !staffOnly))
      prm_upload(sessionFile)
}
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
