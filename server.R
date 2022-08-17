devel <- !grepl(file.path(gsub("^\\w:/","",Sys.getenv("RAS")),"shiny"),gsub("^\\w:/","",getwd()))
hlink <- !FALSE
isScroll <- TRUE
invisible(stats::runif(352))
seed <- sample(100:999,1)
set.seed(seed)
if (!("shiny" %in% loadedNamespaces()))
   require(shiny)
# require(sf)
require(leaflet)
require(shinycssloaders)
# require(ursa)
source("process.R",encoding="UTF-8")
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
groupList <- c('\\d'=c("All Conservation Features","All Data")[1]
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

exchange <- reactiveValues(editor=NULL,overlay=NULL,selection=NULL
                          ,prev=integer(),curr=integer() ## need for removing forgiven clean
                          ,cd=character()
                          )
if (T) observe({ ## update 'input$sheet'
   req(!is.null(input$sheet))
   cat("observe: update input$sheet:\n")
   if (isFALSE(input$sheet %in% activity)) {
      cat("   updated\n")
      updateSelectInput(session,"sheet",choices=c(nameInit,activity)[-1]
                      # ,selected="Aquaculture"
                      # ,selected=ifelse(devel,sample(activity,1),c("Fishery","Shipping")[1])
                       ,selected=activity[1]
                      # ,selected="Tourism"
                       )
   }
})
if (T) observe({ ## update 'input$column'
   req(!is.null(input$sheet))
   req(input$sheet %in% activity)
   cat("observe: update input$column:\n")
   forcing <- grep(paste0("^",input$sheet),rules$activity,value=TRUE)
  # forcing <- gsub("(.+\\S)\\s*»\\s*(\\S.+)","\\2",forcing) ## deprecated
   forcing <- gsub(pattRules,"\\2",forcing)
   if (length(forcing))
      updateSelectInput(session,"column",choices=c(nameInit,forcing)
                      # ,selected=ifelse(devel,sample(forcing,1),forcing[1])
                      # ,selected=grep("Mussels",forcing,value=TRUE)
                       ,selected=nameInit
                       )
   else if (input$sheet==allActivity)
      updateSelectInput(session,"column",choices="All industries"
                      # ,selected="All industries"
                       )
   else if (input$sheet==noneActivity)
      updateSelectInput(session,"column",choices="None industries"
                      # ,selected="All industries"
                       )
})
if (F) observe({ ## update input$method
   req(!is.null(input$sheet))
   req(input$sheet %in% activity)
   cat("observe: update input$coloring:\n")
   ch <- unname(methodList)
   if (!(input$sheet %in% "Shipping"))
      ch <- head(ch,-1)
   updateSelectInput(session,"coloring",choices=ch)
})
if (T) observe({ ## update input$region
  # req(input$sheet %in% activity)
   req(input$region)
   ##~ cat("observe: update input$predefined:\n")
   ##~ print("1")
   ##~ print(input$region)
   ##~ str(regionSF)
   ##~ print(input$region %in% names(regionSF))
   ##~ print("2")
   if (input$region %in% names(regionSF))
      updateSelectInput(session,"predefined"
                       ,label="Select region of interest"
                       ,choices=c(nameClick,regionSF[[input$region]][[1]])
                       )
   else
      updateSelectInput(session,"predefined"
                       ,label="Select instrument for drawing"
                       ,choices=c("leaflet.extras","leafpm"))
   print("this observe was passed successful")
})
if (T) observe({ ## update 'exchange$selection'
   cat("observe 'exchange$selection'\n")
   selection <- rvCustomer()
   cl <- class(selection)
   if ("character" %in% cl) {
      cat("   PAC\n")
     # if (!is.null(selection))
     #    saveRDS(selection,"c:/tmp/selection.rds")
      exchange$selection <- zoomToAOI(selection)
      exchange$overlay <- NULL
      exchange$prev <- exchange$curr
   }
   else {
      cat("   MANUAL\n")
     # str(exchange$selection)
      req(selection)
      cat("      found\n")
      gs <- selection()
      cat("----------\n")
      str(gs)
      cat("----------\n")
      if (input$region %in% nameEditor) {
         if (is.null(gs$finished)) {
            s <- NULL
         }
         else {
            print("0731h")
           # s <- gs$finished["X_leaflet_id"]
            s <- gs$finished["_leaflet_id"]
            print("0731i")
            cat("         finished\n")
            if (length(ind <- which(s$'_leaflet_id' %in% exchange$prev))) {
               str(ind)
               s <- s[-ind,]
            }
            exchange$curr <- s$'_leaflet_id'
            if (F)
               spatial_write(s,"c:/tmp/curr_aoi.sqlite")
         }
      }
      else if (input$predefined %in% nameClick) {
         cat("         selected\nstr(gs):\n")
         str(gs)
         ind <- as.numeric(gs[which(gs$selected==TRUE),"id"])
         cat("str(ind):\n")
         str(ind)
         if (length(ind)) {
            s <- regionSF[[input$region]][ind,] ## class 'st_sf'
            spatial_data(s) <- data.frame(id=as.integer(ind))
            str(s)
         }
         else
            s <- NULL
      }
      else {
         cat("otherwise value of 'input$predefined' is",sQuote(input$predefined),"\n")
         warning("Undefined 's'. This branch is false.")
         s <- NULL
      }
      if (is.null(s))
         exchange$selection <- NULL
      else if (spatial_count(s)==0)
         exchange$selection <- NULL
      else if (F) {
         cat("0731b\n")
        # writeLines(as.character(spatial_count(s)),"tmp_finished.Rout")
        # spatial_write(s,"tmp_editor.geojson")
        # str(s)
         session_grid(blank)
         a <- c(id=ursa:::.fasterize(s))
         a <- ursa_crop(a,border=1)
         print(c('fasterized'=a))
         exchange$selection <- a
        # exchange$selection <- (!is.na(a))*99L
         ##~ if (length(ind <- sf::st_within(spatial_transform(s,ref),ref)[[1]])) {
            ##~ session_grid(blank)
            ##~ spatial_write(ref[ind,],"tmp_intersected.geojson")
            ##~ a <- ursa_crop(allocate(spatial_centroid(ref[ind,])),border=1)
            ##~ exchange$selection <- (!is.na(a))*99L
         ##~ }
      }
      else {
         print("just an 's'")
         exchange$selection <- s
      }
   }
   cat("exchange$selection -- before:\n")
   print(c('exchange$selection'=exchange$selection))
   cat("exchange$selection -- after:\n")
   if (is_ursa(exchange$selection))
      print(as.table(exchange$selection))
   ind <- input$tbl_rows_selected
   if (is.integer(ind))
      exchange$cf <- rvActivityStat()[ind,]
  # exchange$selection
})
if (F) observe({
   ind <- input$tbl_rows_selected
   if (is.integer(ind)) {
      tbl <- rvActivityStat()[ind,,drop=TRUE]
      ind2 <- ursa:::.sample(grep(tbl[[1]],cfmeta$CF_code))
      updateSelectInput(session,"cfcode",selected=cfmeta$label[ind2])
   }
})
if (T) observe({
   print("observe crosstable, update 'industry' input")
  # ind <- input$cfdata_rows_selected
   cell <- input$cross_cells_selected
   res <- rvActivityStat()
   if (!is.null(res)) {
      cname <- colnames(res)
      sname <- NULL
      if (sum(dim(cell))>0) {
         sColumn <- cell[1,2]
         if (sColumn>0) {
            sname <- cname[sColumn] # gsub(pattRules,"\\2",cname[sColumn])
            if (!(sname %in% unlist(industries)))
               sname <- NULL
         }
      }
      else {
        # ind <- input$listCF_rows_selected
        # cat("-----\n")
        # str(ind)
        # cat("-----\n")
      }
     # req(!is.null(input$listCF_rows_selected))
     # cname <- gsub(pattRules,"\\2",grep(sepRules,cname,value=TRUE))
      cname <- cname[cname %in% unlist(industries)]
      updateSelectInput(session,"industry"
                       ,choices=industries
                       ,selected=if (!is.null(sname)) sname else sample(cname,1)
                       )
   }
   else if (T) {
      updateSelectInput(session,"industry",choices="Please choose AOI before")
   }
   else {
      updateSelectInput(session,"industry"
                       ,choices=industries
                       ,selected=sample(unlist(industries),1)
                       )
   }
})
if (T) observe({
   print("observe crosstable, update 'cfcode' input")
   cell <- input$cross_cells_selected
   res <- rvActivityStat()
  # saveRDS(res,"c:/tmp/update_res.rds")
   if (!is.null(res)) {
      rname <- rownames(res)
      fname <- NULL
      if (!is.null(ind <- input$listCF_rows_selected)) {
         ##~ str(ind)
         ##~ b <- res[ind,]
         ##~ str(b)
         fname <- rname[ind]
      }
      else if (sum(dim(cell))>0) {
         sRow <- cell[1,1]
         if (sRow>0) {
            fname <- rname[sRow] # gsub(pattRules,"\\2",cname[sColumn])
            if (!(fname %in% spec$cf))
               fname <- NULL
         }
      }
      fname <- fname[fname %in% spec$cf]
      rname <- paste0(rname," - ",res[rname,1])
      if (!is.null(fname))
         fname <- paste0(fname," - ",res[fname,1])
      updateSelectInput(session,"cfcode"
                       ,choices=sort(rname)
                       ,selected=if (!is.null(fname)) fname else sample(rname,1)
                       )
   }
   else if (T) {
      updateSelectInput(session,"cfcode",choices="Please choose AOI before")
   }
   else {
      da <- rvHumanUseIndustry()
     # saveRDS(da,"C:/tmp/update_cfcode.rds")
      if (is.null(da))
         updateSelectInput(session,"cfcode",choices="Please choose AOI before")
      else {
         value <- paste0(da$'CF Code'," - ",da$'CF Name')
         updateSelectInput(session,"cfcode"
                          ,choices=value
                          ,selected=sample(value,1)
                          )
      }
   }
})
if (F) observe({ ## setView for Selector
   rvAOI()
   print("rvAOI()")
   proxy <- leafletProxy("selector",data=NULL)
   if (!is.null(rvAOI())) {
      proxy %>% leaflet::addMeasure(primaryLengthUnit="meters",primaryAreaUnit="sqmeters")
   }
})
rvActivity <- reactive({
   cat("rvActivity():\n")
   ret <- list(map=NULL,stat=NULL)
  # return(ret)
   if (input$sheet!=nameInit) {
      activity <- paste0(input$sheet,sepRules,input$column)
      print(activity)
     # ind <- grep(activity,rules$activity)
     # str(ind)
     # if (req(length(ind)>0)) {
      if (T) {
         group <- names(groupList[match(input$group,groupList)])
         print(c(group=group))
         aoi <- rvAOI()
         cat("aoi:\n")
         print(aoi)
        # if (!is.null(aoi))
        #    saveRDS(aoi,"c:/tmp/interim_none.rds")
         a <- interim(activity,group=group,aoi=aoi,season=input$season)
         str(a)
        # ret$interim <- a
         ret$map <- a$map
         ret$stat <- a$stat
      }
      else
         print("RULE NOT FOUND")
   }
   else
      print("EMPTY SHEET")
   ret
})
rvAOI <- reactive({
   cat("rvAOI():\n")
   exchange$selection
})
rvActivityMap <- reactive({
   cat("rvActivityMap():\n")
   industry <- rvSelectIndustry()
   req(industry)
  # a <- rvActivity()$map
   print(industry)
   iname <- unname(industry)
   a <- interimMap(industry=iname)
   a
})
rvActivityStat <- reactive({
   cat("rvActivityStat():\n")
  # return(NULL)
   if ((is.null(input$sheet))||(is.null(input$column)))
      activity <- "not applicable"
   else {
      if ((input$sheet==nameInit)||(input$column==nameInit))
         return(NULL)
      activity <- paste0(input$sheet,sepRules,input$column)
   }
   group <- names(groupList[match(input$group,groupList)])
   aoi <- rvAOI()
  # if (!is.null(aoi))
  #    saveRDS(aoi,"c:/tmp/interim_stat.rds")
  # interim(activity,group=group,aoi=aoi,season=input$season,simplify="stat")
   ret <- crosstable(aoi=aoi)
   ret
})
rvConflictMap <- reactive({
   cat("rvConflictMap():\n")
   a <- NULL # rvActivityMap()
   if (is.null(a))
      return(conflictBasemap())
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
   m <- conflictMap(d6)
   m
})
rvCustomer <- reactive({
   cat("rvCustomer:\n")
   print(c('region'=input$region,'predefined'=input$predefined))
   if (is.null(input$region)) {
      return(input$region)
   }
   if ((input$region!=nameEditor)&&(input$predefined!=nameSelector)&&
       (input$predefined!=nameClick)) {
      ret <- input$predefined
      names(ret) <- input$region
      return(ret)
   }
   ret <- NULL
   m <- rvConflictMap()
   if (input$region %in% nameEditor) {
      shp <- drawShapeOptions(color="#092",fillColor="#092")
      ret <- callModule(editMod,"editor"
                       ,leafmap=m
                       ,editor=if (input$predefined==nameClick) c("leaflet.extras","leafpm")[1]
                               else input$predefined
                      # ,editor=input$predefined
                       ,editorOptions=list(NULL
                          ,toolbarOptions=pmToolbarOptions(position="topright"
                                                          ,drawCircle=FALSE
                                                          ,drawPolyline=FALSE
                                                          ,drawMarker=FALSE
                                                          )
                          ,position="topright"
                          ,polygonOptions=drawPolygonOptions(shapeOptions=shp)
                          ,rectangleOptions=if (devel) drawRectangleOptions(shapeOptions=shp) else FALSE
                          ,circleOptions=FALSE
                          ,circleMarkerOptions=FALSE
                          ,markerOptions=FALSE
                          ,polylineOptions=FALSE
                          )
                       )
   }
   else if (input$predefined %in% nameClick) {
      m <- addFeatures(m,regionSF[[input$region]]
                      ,label=regionSF[[input$region]][[1]]
                      ,color="#092B"
                      ,layerId=~seq(spatial_count(regionSF[[input$region]]))
                      ,group="Region overlay"
                      )
      m <- addLayersControl(m
                          # ,baseGroups=c(NULL
                          #              )
                           ,overlayGroups=c(NULL
                                           ,"Region overlay"
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
      ret <- callModule(selectMod,"selector"
                       ,leafmap=m
                       )
   }
   else
      cat("SKIP REGION OVERLAPPING\n")
   ret
})
rvSelectCF <- reactive({
   cat("rvSelectCF():\n")
   b <- rvActivityStat()
   cat("from listCF:\n")
   str(input$listCF_rows_selected)
   cat("from crosstable:\n")
   str(input$cross_cells_selected)
   cat("from cfcode input:\n")
   str(input$cfcode)
   if (!is.integer(ind <- input$listCF_rows_selected)) {
      if (sum(dim(ind <- input$cross_cells_selected))>0) {
         ind <- ind[1,1]
      }
   }
   if (!is.integer(ind))
      ret <- NULL
   else
      ret <- rownames(b)[ind]
   if (T & is.null(ret)) {
      if (nchar(ret <- gsub("^(\\d{4}).*","\\1",input$cfcode))!=4)
         ret <- NULL
   }
   cat("-----------------------------------\n")
   print(ret)
   cat("-----------------------------------\n")
   ret
})
rvSelectIndustry <- reactive({
   cat("rvSelectIndustry():\n")
   cat("from cfdata:\n")
   str(input$cfdata_rows_selected)
   cat("from crosstable:\n")
   str(input$cross_cells_selected)
   cat("from industry input:\n")
   str(input$industry)
   ret <- NULL
   if (!is.integer(ind <- input$cfdata_rows_selected)) {
      if (sum(dim(ind <- input$cross_cells_selected))>0) {
         ind <- ind[1,2]
         b <- rvActivityStat()
         ret <- colnames(b)[ind]
         if (!ret %in% unlist(industries))
            ret <- NULL
      }
   }
   else
      ret <- unlist(industries)[ind]
   if ((TRUE)&&(T | !is.character(ret))&&
           (!is.null(input$industry))&&(input$industry %in% unlist(industries)))
      ret <- input$industry
   if (!is.character(ret))
      return(NULL)
   names(ret) <- names(industries)[sapply(industries,function(a) ret %in% a)]
   cat("-----------------------------------\n")
   print(ret)
   cat("-----------------------------------\n")
   ret
})
rvHumanUseIndustry <- reactive({
   cat("rvHumanUseIndustry():\n")
   print("0731p")
   req(industry <- rvSelectIndustry())
   print("0731q")
   da <- human_use(industry)
   st <- rvActivityStat()
   if (!is.null(st))
      da <- da[da$'CF Code' %in% rownames(st),]
   da
})
