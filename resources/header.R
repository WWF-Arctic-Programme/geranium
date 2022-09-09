devel <- !grepl(file.path(gsub("^\\w:/","",Sys.getenv("RAS")),"shiny"),gsub("^\\w:/","",getwd()))
hlink <- FALSE
staffOnly <- nchar(Sys.getenv("MSOFFICE"))>0
isScroll <- TRUE
useExchange <- FALSE
# invisible(stats::runif(352))
seed <- sample(100:999,1)
set.seed(seed)
# require(ursa)
if (!("shiny" %in% loadedNamespaces()))
   require(shiny)
if (!("ursa" %in% loadedNamespaces()))
   require(ursa)
if (isShiny <- ursa:::.isShiny()) {
  # showModal(modalDialog(title="Initialization"
  #                      ,"Data preparing",size="s",easyClose = TRUE,footer = NULL))
   ursa:::.elapsedTime("require packages -- start")
   require(leaflet)
   require(leaflet.extras)
   require(leafpm)
   require(fasterize)
   require(leafem)
   require(leafpop)
  # require(mapview)
   require(mapedit)
   require(DT)
   require(formattable)
   require(xml2)
  # require(shinycssloaders)
   ursa:::.elapsedTime("require packages -- end")
}
#md <- rmarkdown::metadata
#isShiny <- isTRUE(md$runtime=="shiny")
isReactive <- isShiny & F
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
#activity <- unique(gsub("(.+\\S)\\s*»\\s*(\\S.+)","\\1",rules$activity)) ## deprecated
activity <- unique(gsub(pattRules,"\\1",rules$activity))
activity <- c(noneActivity,allActivity,activity)
options(spinner.color="#ECF0F5")
nameSelector <- "Selector"
nameEditor <- "Editor"
nameClick <- "Click region(s) on map"
