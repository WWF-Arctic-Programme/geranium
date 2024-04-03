proxyRegion <- leafletProxy("regionLeaflet")
exchange <- reactiveValues(editor=NULL,domain=NULL,selection=NULL
                          ,prev=integer(),curr=integer() ## need for removing forgiven clean
                          ,clear=data.frame(),history=data.frame()
                          ,cd=character(),overlay=NULL ## need for removing forgiven clean
                          ,CF=NULL,industry=NULL,conflict=NULL#,initEPA=FALSE
                          ,rebuildNAO=TRUE,subset=NULL,config=config
                          ,region=NULL
                          ##~ ,clicked=c(crossCF=Sys.time(),crossIndustry=Sys.time()
                                    ##~ ,dataCF=Sys.time(),dataIndustry=Sys.time()
                                    ##~ ,onlyCF=Sys.time(),onlyIndustry=Sys.time()
                                    ##~ )
                          ,selectedIndustry=FALSE
                          )
clicked <- reactiveValues(crossCF=Sys.time(),crossIndustry=Sys.time()
                         ,dataCF=Sys.time(),dataIndustry=Sys.time()
                         ,onlyCF=Sys.time(),onlyIndustry=Sys.time()
                         )
if (F) ## patch for shiny 2022-12-17
   spatial_crs(PAs) <- 4326
if (F) observe({ ## update 'input$sheet'  ## DEPRECATED input$sheet
   verbosing()
   cat("observe: update input$sheet:\n")
   req(!is.null(input$sheet))
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
if (F) observe({ ## update 'input$column' ## DEPRECATED input$column
   verbosing()
   cat("observe: `req` input$sheet and input$sheet in activity\n")
   req(!is.null(input$sheet))
   req(input$sheet %in% activity)
   cat("observe: update input$column:\n")
   forcing <- grep(paste0("^",input$sheet),rules$unique,value=TRUE)
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
   verbosing()
   cat("req(input$region):\n")
  # req(input$sheet %in% activity)
   req(input$region)
   ##~ cat("observe: update input$predefined:\n")
   ##~ print("1")
   ##~ print(input$region)
   ##~ str(rvRegionSF())
   ##~ print(input$region %in% names(rvRegionSF()))
   ##~ print("2")
   cat("observe input$region + update input$predefined:\n")
  # if (length(exchange$curr)>0)
  #    exchange$prev <- exchange$curr
  # else
  #    exchange$prev <- -20L
  # if (!length(exchange$curr))
  #    exchange$curr <- -20L
  # else
  #    exchange$curr <- -70L
   if (input$region %in% names(rvRegionSF()))
      updateSelectInput(session,"predefined"
                       ,label="Select region of interest"
                       ,choices=c(nameClick,rvRegionSF()[[input$region]][[1]])[1]
                       )
   else
      updateSelectInput(session,"predefined"
                       ,label="Select instrument for drawing"
                       ,choices=c("leaflet.extras","leafpm"))
   print("this observe was passed successful")
})
if (T) observe({ ## update 'exchange$selection'
# if (T) observeEvent(list(rvCustomer(),exchange),{ ## update 'exchange$selection'
   verbosing()
   cat("observe 'exchange$selection'\n")
   selection <- rvCustomer()
   cl <- class(selection)
   if ("character" %in% cl) {
      cat("   PAC\n")
      print(exchange$curr)
     # if (!is.null(selection))
     # saveRDS(selection,"c:/tmp/selection.rds")
      exchange$selection <- zoomToAOI(selection)
      exchange$overlay <- NULL
      exchange$prev <- exchange$curr
   }
   else {
      cat("   MANUAL\n")
     # str(exchange$selection)
      req(selection)
      cat("      found\n")
      opW <- options(warn=1)
      gs <- selection()
      options(opW)
      ##~ cat("----------\n")
      ##~ str(selection)
      cat("----------\n")
      str(gs)
      str(exchange$curr)
      cat("----------\n")
      if (input$region %in% nameEditor) {
         if (is.null(gs$finished)) {
            s <- NULL
         }
         else {
           # s <- gs$finished["X_leaflet_id"]
            cat("0918a\n")
            s <- gs$finished["_leaflet_id"]
            cat("0918b\n")
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
         if (nrow(gs)>0)
            gs$region <- input$region
         ind <- as.numeric(gs[which(gs$selected==TRUE),"id"])
         if (isTRUE(exchange$prev==(-60))) {
            ind <- integer()
           # exchange$curr <- integer()
         }
         if (TRUE) {
            cat("         selected:\n")
            cat("str(exchange$selection): ============+++++++++++++++++++\n")
            str(exchange$selection)
            cat("str(gs): ============================+++++++++++++++++++\n")
           # str(exchange$prev)
           # str(exchange$curr)
            str(gs)
            cat("str(ind) ============================+++++++++++++++++++\n")
            str(ind)
            cat("str(exchange$history) ===============+++++++++++++++++++\n")
            str(exchange$history)
            cat("=====================================+++++++++++++++++++\n")
         }
         if (length(ind)) {
            if (FALSE) { # {&&(exchange$prev==(-20))&&(all(exchange$curr>0))) {
               exchange$prev <- -40L
              # exchange$curr <- -40L
               s <- NULL
            }
            else if ((!is.null(exchange$selection))&&(nrow(exchange$selection)>0)&&
                     (input$region!=exchange$selection$region[1])) {
               da <- spatial_data(exchange$selection)
               da <- da[da$id==ind,]
               da$entry <- 1L
               exchange$curr <- da
               s <- NULL
            }
            else {
              # print("assign 30")
               da <- data.frame(id=as.integer(ind),region=input$region)
               if ((!is.null(exchange$curr))&&(is.data.frame(exchange$curr))) {
                 # eda <- spatial_data(exchange$selection)
                  ind2 <- which(da$region %in% exchange$curr$region)
                 # ind3 <- which(!is.na(match(exchange$curr$id,da$id)))
                  ind3 <- which(is.na(match(da$id,exchange$curr$id)))
                 # print(eda)
               }
               else {
                  ind2 <- seq_along(ind)
                  ind3 <- integer()
               }
               if (FALSE) {
                  cat("exchange$curr:\n")
                  print(exchange$curr)
                  cat("da:\n")
                  print(da)
               }
               if (length(ind2)) {
                  print(c('ind2.'=ind2))
                  s <- rvRegionSF()[[input$region]][ind[ind2],] ## class 'st_sf'
                  spatial_data(s) <- da[ind2,]
                 # exchange$curr <- rbind(exchange$curr,spatial_data(s))
               }
               else if (length(ind3)) {
                  print(c('ind3.'=ind3))
                  s <- rvRegionSF()[[input$region]][ind[ind3],] ## class 'st_sf'
                  spatial_data(s) <- da[ind3,]
               }
               else if (length(ind)<0){
                  print(c('neg ind.'))
                  s <- rvRegionSF()[[input$region]][ind,] ## class 'st_sf'
                  spatial_data(s) <- da
               }
               else {
                  print("inconsistent regions")
                  if (exchange$curr$entry==1L)
                     exchange$curr$entry <- 2L
                  else
                     print("ready to select again")
                  s <- NULL
               }
              # print("assign 30")
              # exchange$curr <- as.integer(ind)
              # exchange$prev <- 30L
            }
         }
         else {
           # exchange$curr <- -10L
            if (isTRUE(exchange$prev!=-60))
               exchange$prev <- -50L
            s <- NULL
         }
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
        # exchange$prev <- -10L
         print("just a 's'")
         da2 <- exchange$clear
         cat("--------- clear ---------- \n")
         str(da2)
         cat("--------- clear ---------- \n")
         if ((is.data.frame(da2))&&(nrow(da2)>0)) {
            da1 <- spatial_data(s)
            str(da1)
            str(da2)
            ind <- apply(da1[,1,drop=FALSE],1,paste,collapse="-") %in% apply(da2[,1,drop=FALSE],1,paste,collapse="-")
            if (!any(ind))
               exchange$clear <- data.frame()
            else if (all(ind))
               s <- NULL
            else
               s <- s[!ind,]
            if (FALSE) {
               da3 <- rbind(da1,da2)
               if (any(duplicated(da3))) {
                  print("trying to clear previous selection")
                  s <- NULL
                 # exchange$clear <- data.frame()
               }
            }
         }
        # saveRDS(s,"C:/tmp/selectionS.rds")
         exchange$selection <- s
      }   
   }
   if (T) {
      cat("exchange$selection -- before:\n")
      str(list(prev=exchange$prev,curr=exchange$curr))
      print(c('exchange$selection'=exchange$selection))
      cat("exchange$selection -- after:\n")
      if (is_ursa(exchange$selection))
         print(as.table(exchange$selection))
   }
   if (!is.null(exchange$selection)) {
      updateSelectInput(session,"region"
                       ,choice={
                           ch <- input$region
                           if (input$economy!="none")
                              ch <- c(ch,'Reset selection'='clear')
                           ch
                       }
                      # ,choice=c('Selected fegion is frozen'='frozen','Reset selection'='clear')
                      # ,label="Frozen until selection is cleared"
                       )
      val <- epsg[match(as.integer(input$epsg),epsg)]
      updateSelectInput(session,"epsg",choice=val # input$epsg
                       ,label="Projection (frozen)")
   }
   else {
      if (isPause <- input$region %in% c("clear")) {
         if (!is.null(exchange$clear))
            rs <- exchange$clear$region
         else
            rs <- nameEditor # exchange$clear
        # exchange$curr <- integer()
      }
      else
         rs <- input$region
      updateSelectInput(session,"region",choice=c(names(rvRegionSF()),nameEditor)
                       ,selected=rs,label="Spatial query")
      updateSelectInput(session,"epsg",selected=input$epsg,choice=epsg
                       ,label="Projection")
   }
  # ind <- input$tbl_rows_selected
  # if (is.integer(ind))
  #    exchange$cf <- rvCrossTable()[ind,]
  # exchange$selection
})
observeEvent(rvCustomer(),{
   cat('observe rvCustomer() to update selection history\n"')
   selection <- rvCustomer()
   req(selection)
   if ("character" %in% class(selection)) {
   }
   else {
      gs <- selection()
      if (input$region %in% nameEditor) {
         if (!is.null(gs$finished)) {
           # str(gs$finished)
         }
      }
      else if (input$predefined %in% nameClick) {
         if (nrow(gs)>0) {
            gs$region <- input$region
            exchange$history <- exchange$history[!duplicated(exchange$history),]
            str(exchange$history)
         }
      }
   }
})
if (F) observe({ ## update 'session$cfcode'
   ind <- input$tbl_rows_selected
   if (is.integer(ind)) {
      tbl <- rvCrossTable()[ind,,drop=TRUE]
      ind2 <- ursa:::.sample(grep(tbl[[1]],cfmeta$CF_code))
      updateSelectInput(session,"cfcode",selected=cfmeta$label[ind2])
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
'rvAOI' <- reactive({
   verbosing()
   cat("rvAOI():\n")
  # if (isTRUE(exchange$curr<0)) {
  #    return(NULL)
  # }
  # str(exchange$clear)
   exchange$selection
})
'rvActivityMap' <- reactive({
   verbosing()
   cat("rvActivityMap():\n")
   if (T) {
      conflict <- exchange$conflict
      if (is.null(conflict)) {
         cat("conflict is NULL\n")
         return(NULL)
      }
      conflict$subset <- exchange$subset
      str(conflict)
   }
   else {
      print(c(economy=input$economy))
      return(NULL)
      if (any(c(nameInit,"none","skip") %in% input$economy))
         return(NULL)
      conflict <- list(industry=input$industry,group=input$group3
                      ,season=input$season,economy=input$economy
                      ,subset=exchange$subset
                      )
   }
   economy <- conflict$economy
   if (any(c(nameInit,"none","skip","reset") %in% economy))
      return(NULL)
   industry <- conflict$subset |> industryName()
   print("0124g")
   str(list(industry=industry,economy=economy))#,'input$industry'=input$industry))
   print("0124h")
  # if ((!length(industry))&&(economy=="none"))
  #    return(NULL)
  # if ("none" %in% industry)
  #    return(NULL)
  # if ("none" %in% economy)
  #    return(NULL)
  # if ("none" %in% input$industry)
  #    return(NULL)
   if (T | !length(economy)) {
      season <- conflict$season
     # if (!length(season))
     #    season <- "max"
      if (F) {
         group <- conflict$group
         group <- groupList[na.omit(match(group,groupList))]
        # str(list(season=seson,group=group))
         if (!length(group)) {
           # return(NULL)
            group <- "\\d"
         }
         group <- names(group)
      }
      else
         group <- rvSubsetCF()
     # a <- rvActivity()$map
     # print(industry)
     # iname <- unname(industry)
   }
   else if (F) {
     # season <- "max HERE"
     # group <- "\\d HERE"
      season <- conflict$season
      group <- conflict$group
      group <- groupList[na.omit(match(group,groupList))]
      str(group)
      if (!length(group))
         return(NULL)
      group <- names(group)
   }
   str(list(industry=industry,season=season,group=group,economy=economy))
   print("next to 'conditionMap'")
   a <- conditionMap(industry=industry,season=season,group=group,economy=economy,epoch=input$epoch)
   a
})
'rvConflictMap' <- reactive({
   verbosing()
   cat("rvConflictMap():\n")
  # plutil::timeHint(as.character(input$customize))
   a <- if (T) rvActivityMap() else NULL
   if (is.null(a)) {
      print("next to 'conflictBasemap'")
      return(conflictBasemap(epsg=input$epsg))
   }
   coloring <- names(methodList[match(input$coloring,methodList)])
   print("1226a")
   print(a)
   print(coloring)
  # print(input$sheet)
   print("1226b")
   if (length(a)==3)
      d6 <- map3_1d(a,kind=coloring,source=input$sheet)
   else {
      if (band_blank(a[1]))
         return(conflictBasemap(epsg=input$epsg))
      d6 <- map1_1d(a[1],kind="devel",ncolor=input$ncolor)
   }
   print("1226c")
   print(d6)
   print("1226d")
   m <- conflictMap(d6,aoi=rvAOI(),epsg=input$epsg)
   print("1226e")
   m
})
observeEvent(if (F) input$drawIndustry else input$economy,{ ## eventReactive actionlink
   verbosing()
   cat("observeEvent input$drawIndustry:\n")
  # showNotification(paste("'drawIndustry' is clicked:",input$drawIndustry),duration=3)
   exchange$conflict <- list(industry=rvSubsetIndustry() # input$industry
                            ,group=rvSubsetCF()
                           # ,group=input$group3
                            ,season=input$season,economy=input$economy)
})
if (F) observeEvent(input$economy,{ ## update 'input$economy'
   verbosing()
   cat("observeEvent input$economy:\n")
   exchange$conflict <- list(industry=rvSubsetIndustry() # input$industry
                            ,group=rvSubsetCF()
                           # ,group=input$group3
                            ,season=input$season,economy=input$economy)
})
if (F) observeEvent(input$drawEconomy,{ ## eventReactive actionlink
   verbosing()
   cat("observeEvent input$drawEconomy:\n")
  # showNotification(paste("'drawIndustry' is clicked:",input$drawIndustry),duration=3)
   exchange$conflict <- list(industry=input$industry,group=input$group3
                            ,season=input$season,economy=input$economy)
})
'rvSubsetIndustry' <- reactive({
   verbosing()
   cat("rvSubsetIndustry:\n")
   ref <- unname(industryName(industries))
   if ("all" %in% input$industry) { # low, level1
      if (allActivity %in% input$activity) { ## up, level2
         industry <- ref
      }
      else {
         industry <- unname(industryName(industries[input$activity]))
      }
   }
   else {
     # industry <- ref
      industry <- unname(industryName(input$industry))
   }
   ret <- industry[order(match(industry,ref))]
   ret <- industryCode(ret)
   ret
})
'rvGroupCF' <- reactive({
   ret <- groupCF(c(input$group3,input$group2,input$group1,input$group0))
   as.character(ret) ## character needs for 'CFMap'
})
'rvSubsetCF' <- reactive({
   cat("rvSubsetCF:\n")
   if (!is.null(rvAOI())) {
      da <- rvCrossTable()
     # str(dimnames(da))
      ret <- rownames(da)
   }
   else
      ret <- rvGroupCF()
   sort(ret)
})
'rvCrossTable' <- reactive({
   verbosing()
   showModal(modalDialog(title="Creating Conservation Concern table…","Please wait"
                        ,size="s",easyClose=T,footer=NULL))
   on.exit(removeModal())
   if ((FALSE)&&(isTRUE(switchDomain()))) {
      cat("   domain is TRUE:\n")
     # switchDomain(FALSE) ## FALSE is bad idea
      groupCF <- rvGroupCF() ## don't use `rvSubsetCF` to avoid recursion
      if (!is.null(indCF <- input$onlyCF_rows_selected))
         groupCF <- groupCF[indCF]
      industry <- rvSubsetIndustry()
      if (!is.null(indI <- input$onlyIndustry_rows_selected))
         industry <- industry[indI]
      ret <- crossTable(aoi=NULL
                       ,group=groupCF
                       ,activity=industryCode(industry)
                       ,season=input$season
                       ,minCover=input$omitPercent
                       )
     # proxyOnlyCF %>% selectRows(NULL)
     # proxyCross %>% selectRows(NULL)
     # proxyCross %>% selectColumns(NULL)
     # proxyIndustrydata %>% selectRows(NULL)
     # proxyOnlyIndustry %>% selectRows(NULL)
     # proxyCFdata %>% selectRows(NULL)
      return(ret)
   }
   if (FALSE) {
      if ((is.null(input$sheet))||(is.null(input$column)))
         activity <- "not applicable"
      else {
         if ((input$sheet==nameInit)||(input$column==nameInit))
            return(NULL)
         activity <- paste0(input$sheet,sepRules,input$column)
      }
      if (all(input$activity %in% names(industries))) {
         activity <- input$activity
        # industry <- unlist(industries[input$activity]
      }
      else
         activity <- "all"
     # str(vulner$industry) ## deprecated
   }
   aoi <- rvAOI()
  # group <- rvSubsetCF() # groupList[match(input$group3,groupList)]
   group <- rvGroupCF() ## don't use `rvSubsetCF` to avoid recursion
   if (!is.null(names(group)))
      group <- names(group)
   industry <- rvSubsetIndustry()
  # if (!is.null(aoi))
  #    saveRDS(aoi,"c:/tmp/interim_stat.rds")
  # interim(activity,group=group,aoi=aoi,season=input$season,simplify="stat")
   ##~ exchange$domain <- FALSE
   ##~ plutil::timeHint(paste("domain is",exchange$domain))
   if (staffOnly) {
      count <<- count+1L
      if (staffOnly)
         showNotification(paste("call `crossTable` right now:",count),duration=5)
   }
   ret <- crossTable(aoi=aoi
                    ,group=group
                    ,activity=industryCode(industry)
                   # ,activity=activity
                   # ,activity=unlist(exchange$subset)
                    ,season=input$season
                    ,minCover=input$omitPercent ## +++
                    )
  # ret <- ret[ret$'Cover'>=input$omitPercent,] ## ---
   ret
})
'rvCustomer' <- reactive({
   verbosing()
   cat("rvCustomer:\n")
   print(c('region'=input$region,'predefined'=input$predefined))
   if (is.null(input$region)) {
      return(input$region)
   }
   if ((input$region!=nameEditor)&&(input$predefined!=nameSelector)&&
       (input$predefined!=nameClick)) {
      ret <- input$predefined
      names(ret) <- input$region
      print(paste0(" --> ",ret))
      return(ret)
   }
   ret <- NULL
   m <- rvConflictMap()
   if (input$region %in% nameEditor) {
      print(paste0(" --> ","editor"))
      shp <- drawShapeOptions(color="#092",fillColor="#092")
     # opW <- options(warn=-10)
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
                                                          ,drawRectangle=FALSE
                                                          )
                          ,position="topright"
                          ,polygonOptions=drawPolygonOptions(shapeOptions=shp)
                          ,rectangleOptions=if (F & devel) drawRectangleOptions(shapeOptions=shp) else FALSE
                          ,circleOptions=FALSE
                          ,circleMarkerOptions=FALSE
                          ,markerOptions=FALSE
                          ,polylineOptions=FALSE
                          )
                       )
     # options(opW)
   }
   else if (input$predefined %in% nameClick) {
      print(paste0(" --> ","clicker"))
      if (!input$region %in% "clear") {
         reg <- rvRegionSF()[[input$region]]
        # cat("0401a ------------ \n")
        # str(reg)
        # cat("0401b ------------ \n")
         m <- addFeatures(m,reg
                         ,label=reg[[1]]
                         ,color="#092B"
                         ,layerId=~seq(spatial_count(reg))
                         ,group="AOI overlay"
                         )
         m <- addLayersControl(m
                             # ,baseGroups=c(NULL
                             #              )
                              ,overlayGroups=c(NULL
                                              ,"Arctic SDI"
                                              ,"AOI overlay"
                                              )
                              ,options=layersControlOptions(collapsed=FALSE)
                              )
      }
      else {
         cat("NEED TO CLEAR SELECTION -------------------\n")
         exchange$clear <- spatial_data(rvAOI()) |> tail(1)
         str(exchange$clear)
        # exchange$selection <- NULL
         cat("----------------------- -------------------\n")
         return(NULL)
         if (!is.null(exchange$selection)) {
           # exchange$clear <- exchange$selection$region
           # exchange$curr <- -60L
           # print(exchange$clear)
            exchange$prev <- -60L
            exchange$selection <- NULL
         }
         return(NULL)
      }
      print("callModule 'SelectMod: selector'")
      ret <- callModule(selectMod,"selector"
                       ,leafmap=m
                       )
   }
   else
      cat("SKIP REGION OVERLAPPING\n")
   ret
})
if (T) {
   ##~ observeEvent(input$cross_row_last_clicked,exchange$clicked["crossCF"] <- Sys.time())
   ##~ observeEvent(input$cross_column_last_clicked,exchange$clicked["crossIndustry"] <- Sys.time())
   ##~ observeEvent(input$onlyCF_row_last_clicked,exchange$clicked["onlyCF"] <- Sys.time())
   ##~ observeEvent(input$onlyIndustry_row_last_clicked,exchange$clicked["onlyIndustry"] <- Sys.time())
   ##~ observeEvent(input$cfdata_row_last_clicked,exchange$clicked["dataIndustry"] <- Sys.time())
   ##~ observeEvent(input$industrydata_row_last_clicked,exchange$clicked["dataCF"] <- Sys.time())
   observeEvent(input$cross_row_last_clicked,clicked$crossCF <- Sys.time())
   observeEvent(input$cross_column_last_clicked,clicked$crossIndustry <- Sys.time())
   observeEvent(input$onlyCF_row_last_clicked,clicked$onlyCF <- Sys.time())
   observeEvent(input$onlyIndustry_row_last_clicked,clicked$onlyIndustry <- Sys.time())
   observeEvent(input$cfdata_row_last_clicked,clicked$dataIndustry <- Sys.time())
   observeEvent(input$industrydata_row_last_clicked,clicked$dataCF <- Sys.time())
}
'rvSelectCF_deprecated' <- reactive({
   verbosing()
   cat("rvSelectCF():\n")
   if (useExchange)
      return(exchange$CF)
   if (length(ind <- input$industrydata_rows_selected)>0) {
      print("input$industrydata_rows_selected")
      if (!is.null(da <- rvHumanUseIndustry())) {
         ret <- da[[1]][ind]
         print(ret)
         return(ret)
      }
   }
   if (length(ind <- input$cross_rows_selected)) {
      if (!is.null(da <- rvCrossTable())) {
         ret <- rownames(da)[ind]
        # exchange$CF <- ret
         print("input$cross_rows_selected")
         return(ret)
      }
   }
   if (length(ind <- input$onlyCF_rows_selected)) {
      if (!is.null(da <- rvSubsetCF())) {
         str(da)
         str(ind)
         ret <- da[ind]
        # exchange$CF <- ret
         print("input$onlyCF_rows_selected")
         return(ret)
      }
      if ((FALSE)&&(!is.null(da <- rvCrossTable()))) {
         ret <- rownames(da)[ind]
        # exchange$CF <- ret
         print("input$onlyCF_rows_selected (deprecated)")
         return(ret)
      }
   }
  # exchange$CF <- NULL
   NULL
})
'rvSelectCF' <- reactive({
   verbosing()
   cat("rvSelectCF():\n")
   if (useExchange)
      return(exchange$CF)
   if (T & !is.null(r <- input$allComments_rows_selected)) {
      da <- rvAllComments()[r,]
     # return(da$Industry)
      return(da$CF)
   }
   if (T) {
     # subsetCF <- rvSubsetCF()
      ##~ clicked <- exchange$clicked
      ##~ isCross <- ((clicked["crossCF"]>clicked["onlyCF"])&&
                  ##~ (clicked["crossCF"]>clicked["dataCF"]))
      ##~ isData <- ((clicked["dataCF"]>clicked["onlyCF"])&&
                  ##~ (clicked["dataCF"]>clicked["crossCF"]))
      ##~ isOnly <- (#(length(indO <- input$onlyCF_rows_selected)>=0)&&
                  ##~ (clicked["onlyCF"]>clicked["dataCF"])&&
                  ##~ (clicked["onlyCF"]>clicked["crossCF"]))
      isCross <- ((clicked$crossCF>clicked$onlyCF)&&
                  (clicked$crossCF>clicked$dataCF))
      isData <- ((clicked$dataCF>clicked$onlyCF)&&
                  (clicked$dataCF>clicked$crossCF))
      isOnly <- ((clicked$onlyCF>clicked$dataCF)&&
                  (clicked$onlyCF>clicked$crossCF))
     # str(as.list(clicked))
      print(c(isCrossCF=isCross,isDataCF=isData,isOnlyCF=isOnly))
      if (isCross) {
         if (length(indC <- input$cross_rows_selected)>0) {
            if (!is.null(da <- rvCrossTable())) {
               ret <- rownames(da)[indC]
               print(data.frame(indCross=indC,ret=ret))
               indC1 <- match(ret,rvSubsetCF())
               if (!is.na(indC1)) {
                  DT::selectRows(proxyIndustrydata,indC1)
                  DT::selectRows(proxyOnlyCF,indC1)
               }
              # if (!is.null(da <- rvHumanUseIndustry())) {
              #    if (!is.null(indC2 <- match(ret,da[[1]]))) {
              #       DT::selectRows(proxyIndustrydata,indC2)
              #    }
              # }
              # DT::selectRows(proxyOnlyCF,indC)
               return(ret)
            }
         }
         else {
            DT::selectRows(proxyIndustrydata,NULL)
            DT::selectRows(proxyOnlyCF,NULL)
            return(NULL)
         }
         showNotification("rvSelectCF: isProxy unhandled",duration=3)
         return(NULL)
      }
      if (isData) {
         indD <- input$industrydata_rows_selected
         str(indD)
         if (length(indD)>0) {
            print("1121c")
           # if (!is.null(da <- rvHumanUseIndustry())) {
           #    print("1121d")
              # ret <- da[[1]][indD]
               ret <- rvSubsetCF()[indD]
               print(data.frame(indData=indD,ret=ret))
               if (!is.null(da <- rvCrossTable())) {
                  if (!is.na(indD1 <- match(ret,rownames(da))))
                 # print(data.frame(indData=indD,ret=ret,indD1=indD1))
                     DT::selectRows(proxyCross,indD1)
               }
               DT::selectRows(proxyOnlyCF,indD)
               return(ret)
           # }
           # else {
           #    print("1121e")
           #    showNotification("rvSelectCF: isData table corruption",duration=3)
           #    return(NULL)
           # }
         }
         else {
            DT::selectRows(proxyCross,NULL)
            DT::selectRows(proxyOnlyCF,NULL)
            return(NULL)
         }
         showNotification("rvSelectCF: isData unhandled",duration=3)
         return(NULL)
      }
      if (isOnly) {
         indO <- input$onlyCF_rows_selected
         if (length(indO)==1) {
           # if (!is.null(da <- rvHumanUseIndustry())) {
           #    str(da)
              # ret <- da[[1]][indO] # ret <- subsetCF[indO]
               ret <- rvSubsetCF()[indO]
               print(data.frame(indOnly=indO,ret=ret))
               DT::selectRows(proxyIndustrydata,indO)
           #    print(data.frame(indData=indO,ret=ret))
               if (!is.null(da <- rvCrossTable()))
                  if (!is.na(indO1 <- match(ret,rownames(da))))
                     DT::selectRows(proxyCross,indO1)
           # }
            return(ret)
         }
         else {
            if (!length(indO)) {
               DT::selectRows(proxyIndustrydata,NULL)
               DT::selectRows(proxyCross,NULL)
               return(NULL)
            }
            return(rvSubsetCF()[indO])
         }
         showNotification("rvSelectCF: isData unhandled",duration=3)
         return(NULL)
      }
      showNotification("rvSelectCF: general unhandled",duration=3)
      return(NULL)
   }
   if (length(ind <- input$cross_rows_selected)) {
      if (!is.null(da <- rvCrossTable())) {
         ret <- rownames(da)[ind]
        # exchange$CF <- ret
         print("input$cross_rows_selected")
         return(ret)
      }
   }
   if (length(ind <- input$onlyCF_rows_selected)) {
      if (!is.null(da <- rvSubsetCF())) {
         str(da)
         str(ind)
         ret <- da[ind]
        # exchange$CF <- ret
         print("input$onlyCF_rows_selected (domain)")
         return(ret)
      }
   }
   if (T & length(ind <- input$industrydata_rows_selected)) {
      print("1110p")
      if (T) {
         if (!is.null(da <- rvHumanUseIndustry())) {
            str(da[[1]])
            ret <- da[[1]][ind]
            print(ret)
            return(ret)
         }
      }
      if (!is.null(da <- rvSubsetCF())) {
         ret <- da[ind]
        # exchange$CF <- ret
         print("input$industrydata_rows_selected (domain)")
         return(ret)
      }
      else {
         if (!is.null(da <- rvCrossTable())) {
            ret <- rownames(da)[ind]
            print("input$industrydata_rows_selected (aoi), row with cf selected")
            print(ret)
            return(ret)
         }
      }
     # da <- rvHumanUseCF()
     # str(dimnames(da))
     # da <- rvHumanUseIndustry()
     # str(dimnames(da))
     # da <- rvSubsetCF()
     # str(da)
     # str(da[ind])
      print("1110q")
   }
  # exchange$CF <- NULL
   NULL
})
if (T) observe({
   selected <- length(input$cross_columns_selected)>0
   if (selected) {
      if (!exchange$selectedIndustry) {
         exchange$selectedIndustry <- TRUE
        # exchange$clicked["crossIndustry"] <- Sys.time()
         clicked$crossIndustry <- Sys.time()
      }
   }
   else {
      if (exchange$selectedIndustry) {
         exchange$selectedIndustry <- FALSE
        # exchange$clicked["crossIndustry"] <- Sys.time()
         clicked$crossIndustry <- Sys.time()
      }
   }
})
'rvSelectIndustry' <- reactive({
   verbosing()
   cat("rvSelectIndustry():\n")
   if (T & !is.null(r <- input$allComments_rows_selected)) {
      da <- rvAllComments()[r,]
      return(da$Industry)
     # return(da$CF)
   }
   if (T) {
     # str(as.list(clicked))
      ##~ clicked <- exchange$clicked
      ##~ isCross <- ((clicked["crossIndustry"]>clicked["onlyIndustry"])&&
                  ##~ (clicked["crossIndustry"]>clicked["dataIndustry"]))
      ##~ isData <- ((clicked["dataIndustry"]>clicked["onlyIndustry"])&&
                  ##~ (clicked["dataIndustry"]>clicked["crossIndustry"]))
      ##~ isOnly <- (#(length(indO <- input$onlyCF_rows_selected)>=0)&&
                  ##~ (clicked["onlyIndustry"]>clicked["dataIndustry"])&&
                  ##~ (clicked["onlyIndustry"]>clicked["crossIndustry"]))
      isCross <- ((clicked$crossIndustry>clicked$onlyIndustry)&&
                  (clicked$crossIndustry>clicked$dataIndustry))
      isData <- ((clicked$dataIndustry>clicked$onlyIndustry)&&
                  (clicked$dataIndustry>clicked$crossIndustry))
      isOnly <- ((clicked$onlyIndustry>clicked$dataIndustry)&&
                  (clicked$onlyIndustry>clicked$crossIndustry))
      print(c(isCrossI=isCross,isDataI=isData,isOnlyI=isOnly))
     # subsetIndustry <- rvSubsetIndustry()
      if (isCross) {
         if (length(indC <- input$cross_columns_selected)>0) {
            if (!is.null(da <- rvCrossTable())) {
               ret <- industryCode(colnames(da)[indC])
               indC1 <- match(ret,rvSubsetIndustry())
               print(data.frame(indCross=indC,indC1=indC1,ret=ret))
               if (is.na(indC1)) {
                  indC1 <- NULL
                  ret <- NULL
               }
               DT::selectRows(proxyCFdata,indC1)
               DT::selectRows(proxyOnlyIndustry,indC1)
               return(ret)
            }
         }
         else {
            DT::selectRows(proxyCFdata,NULL)
            DT::selectRows(proxyOnlyIndustry,NULL)
            return(NULL)
         }
         showNotification("rvSelectIndustry: isCross unhandled",duration=3)
         return(NULL)
      }
      if (isData) {
         indD <- input$cfdata_rows_selected
         if (length(indD)>0) {
            str(indD)
           # if (!is.null(da <- rvHumanUseCF())) {
           #    print("1121a")
              # ret <- industryCode(da[[1]][indD])
               ret <- rvSubsetIndustry()[indD]
               print(data.frame(indData=indD,ret=ret))
               if (!is.null(da <- rvCrossTable())) {
                  if (!is.na(indD1 <- match(ret,colnames(da))))
                     DT::selectColumns(proxyCross,indD1)
               }
               DT::selectRows(proxyOnlyIndustry,indD)
               return(ret)
           # }
           # else {
           #    print("1121b")
           #    showNotification("rvSelectIndustry: isData table corruption",duration=3)
           #    return(NULL)
           # }
         }
         else {
            DT::selectColumns(proxyCross,NULL)
            DT::selectRows(proxyCFdata,NULL)
            return(NULL)
         }
         showNotification("rvSelectIndustry: isData unhandled",duration=3)
         return(NULL)
      }
      if (isOnly) {
         indO <- input$onlyIndustry_rows_selected
         if (length(indO)==1) {
           # if (!is.null(da <- rvHumanUseIndustry())) {
           #    str(da)
              # ret <- da[[1]][indO] # ret <- subsetCF[indO]
               ret <- rvSubsetIndustry()[indO]
               print(data.frame(indOnly=indO,ret=ret))
               DT::selectRows(proxyCFdata,indO)
           #    print(data.frame(indData=indO,ret=ret))
               if (!is.null(da <- rvCrossTable())) {
                  str(dimnames(da))
                  print(colnames(da))
                  str(match(ret,industryCode(colnames(da))))
                  if (!is.na(indO1 <- match(ret,industryCode(colnames(da))))) {
                     DT::selectColumns(proxyCross,indO1)
                  }
               }
           # }
            return(ret)
         }
         else {
            if (!length(indO)) {
               DT::selectRows(proxyCFdata,NULL)
               DT::selectColumns(proxyCross,NULL)
               return(NULL)
            }
            return(rvSubsetIndustry()[indO])
         }
         showNotification("rvSelectIndustry: isData unhandled",duration=3)
         return(NULL)
      }
      showNotification("rvSelectIndustry: general unhandled",duration=3)
     # return(NULL)
   }
   ret <- NULL
   if (F) on.exit({
      print("1101 -- selected industry")
      exchange$industry <- ret
      print(ret)
      print("1101 -- selected industry")
   })
  # str(as.list(exchange$clicked))
   if (useExchange)
      return(exchange$industry)
   if (length(input$cfdata_row_last_clicked)>0) {
      if ((T)&&(length(ind <- input$cfdata_rows_selected)>0)) {
         print("input$cfdata_rows_selected")
         if (T) {
            if (!is.null(b <- rvSubsetIndustry())) {
               print("1119c")
               cname <- b[ind]
               str(cname)
               print("1119d")
               ret <- cname
               return(ret)
            }
         }
         else if (F) {
            if (!is.null(da <- rvHumanUseCF())) {
               ret <- rownames(da)[ind]
               print(ret)
               ret <- industryCode(ret)
               print(ret)
               return(ret)
            }
         }
      }
   }
   ##~ else if (T & length(ind <- input$onlyIndustry_rows_selected)) {
      ##~ print("input$onlyIndustry_rows_selected")
      ##~ if (!is.null(b <- rvSubsetIndustry())) {
         ##~ cname <- b[ind]
         ##~ ret <- indusrtyCode(cname)
        ##~ # exchange$industry <- ret
         ##~ return(ret)
      ##~ }
   ##~ }
   if (length(ind <- input$cross_columns_selected)) {
      print("input$cross_columns_selected")
      if ((FALSE)&&(!is.null(b <- rvSubsetIndustry()))) {
         print("1110a")
         print(b)
         print(ind)
         cname <- b[ind]
         print(cname)
         print("1110b")
         iname <- industryName(industries)
         if (cname %in% iname) {
           # proxyCFdata %>% selectRows(NULL)
           # proxyOnlyIndustry %>% selectRows(NULL)
            ret <- cname
           # exchange$industry <- ret
            print("input$cross_columns_selected (just proposed)")
            return(ret)
         }
      }
      if ((TRUE)&&(!is.null(b <- rvCrossTable()))) {
         cname <- colnames(b)[ind]
         iname <- industryName(industries)
         if (cname %in% iname) {
           # proxyCFdata %>% selectRows(NULL)
           # proxyOnlyIndustry %>% selectRows(NULL)
            ret <- industryCode(cname)
           # exchange$industry <- ret
            return(ret)
         }
      }
   }
  # exchange$industry <- NULL
   print("no industry selection detected")
   NULL
})
'rvHumanUseIndustry' <- reactive({
   verbosing()
   cat("rvHumanUseIndustry():\n")
   industry <- rvSelectIndustry()
   str(industry)
  # industry <- exchange$industry
  # industry <- "Mass tourism"
  # plutil::timeHint(paste("human use:",industry," --- ",exchange$industry))
   req(industry %in% industryCode(industryName(industries)))
   da <- human_use(industry)
   if (any(input$season %in% tail(seasonList,-1))) {
      ind <- which(substr(colnames(da),1,3) %in% substr(input$season,1,3))
      if (length(ind))
         da <- da[,c(1,ind)]
   }
   if (T) {
      st <- rvCrossTable()
      if (!is.null(st))
         da <- da[da$'CF Code' %in% rownames(st),]
   }
   da
})
'rvHumanUseCF' <- reactive({
   verbosing()
   cat("rvHumanUseCF():\n")
   cf <- rvSelectCF()
   str(cf)
  # cf <- exchange$cf
  # cf <- "9038"
  # plutil::timeHint(paste("human use:",cf," --- ",exchange$cf))
   req(cf %in% spec$cf)
   da <- human_use(cf)
   str(input$season)
   str(tail(seasonList,-1))
   if (any(input$season %in% tail(seasonList,-1))) {
      ind <- which(substr(colnames(da),1,3) %in% substr(input$season,1,3))
      if (length(ind)) {
         da <- da[,c(1,ind)]
      }
   }
   if (T) {
      st <- rvCrossTable()
      if (!is.null(st))
         da <- da[da$'Industry' %in% colnames(st),]
   }
   da
})
'rvIndustryGroup' <- reactive({
   cat("rvIndustryGroup:\n")
   req(industry <- rvSelectIndustry())
   industry <- industryCode(industry)
   ret <- sapply(industries,function(a) industry %in% industryCode(a))
   ret <- names(ret)[ret]
   ret 
})
if (F) observe({ ## clear chekbox not to show map on region Desc
   if (is.null(rvAOI()))
      updateCheckboxInput(session,"regionDesc",value=FALSE)
})
if (F) observeEvent(input$regionAction,{
   cat("observe 'input$regionAction'\n")
   if (isShiny)
      showNotification(id="regionMap",closeButton=FALSE,duration=120
                      ,"Rendering Existing Protected Areas"
                      ,type="warning")
   grPAs <- "Existing Protected Areas"
   colPAs <- "#992B"
  # regionProxy <- leafletProxy("regionLeaflet")
   ursa:::.elapsedTime("add PAs -- begin")
   regionProxy %>% leaflet::addPolygons(data=PAs # spatial_transform(PAs,4326) ## PAs
                      ,label=grPAs
                      ,color=colPAs
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                 )
                      ,group=grPAs
                      )
   ursa:::.elapsedTime("add PAs -- end")
   ##~ regionProxy %>% leaflet::addLegend(position="bottomleft"
                    ##~ ,colors=colPAs
                    ##~ ,opacity=0.2
                    ##~ ,labels=grPAs
                    ##~ ,group=grPAs
                    ##~ )
   ##~ regionProxy %>% removeLayersControl()
   ##~ regionProxy %>% addLayersControl(overlayGroups=c("Arctic SDI",grPAs,grAOI)
                             ##~ ,options=layersControlOptions(collapsed=FALSE)
                             ##~ )
   regionProxy %>% showGroup(grPAs)
   if (isShiny)
      removeNotification(id="regionMap")
})
'rvRegionConcern' <- reactive({
   verbosing()
   cat("rvRegionConcern:\n")
   rvRegionMetrics()$result
})
'rvRegionMetrics' <- reactive({
   verbosing()
   cat("rvRegionMetrics:\n")
   result <- regionStats(aoi=rvAOI(),ctable=rvCrossTable(),isPA=input$actionEPA
                        ,raw=TRUE)
   result
})
'rvRegionActivity' <- reactive({
   verbosing()
   cat("rvRegionActivity:\n")
   result <- regionActivityIndices(aoi=rvAOI(),ctable=rvCrossTable()
                                 # ,isPA=input$actionEPA,raw=TRUE
                                  ,epoch=input$epoch
                                 # ,type="raw"
                                  )
   result
})
if (T) observe({ ## update 'input$industry'
   verbosing()
   cat("observe 'input$activity' + update 'input$industry'\n")
  # print(input$activity)
  # req(!is.null(choice <- input$activity))
   choice <- input$activity
   print(choice)
   if ((length(choice)==1)&&(choice==nameAllHuman)) {
      choice2 <- c('All industrial activities'="all",'Do not show'="none",industryCodeName(industries))[-2]
      select2 <- choice2[1]
   }
   else if ((F & staffOnly)&&(all(activitySubset %in% choice))&&(all(choice %in% activitySubset))) {
      activitySubset <- c("Infrastructure","Mining","Shipping")
      industrySubset <- industryName(c("ICI","MOP","ST"))
      choice2 <- c('All industrial activities'="all",'Do not show'="none",industrySubset)[-2]
      select2 <- industryName("ST")
   }
   else {
      choice2 <- choice[choice %in% names(industries)]
      choice2 <- c('All industrial activities'="all",'Do not show'="none",industries[choice2])[-2]
      choice2 <- industryCodeName(choice2)
      select2 <- choice2[1]
   }
  # exchange$conflict <- NULL
  # exchange$subset <- tail(choice2,-1)
  # print(c(subset=exchange$subset))
   updateSelectInput(session,"industry",choices=choice2,selected=select2)
  # updateRadioButtons(session,"actionNAC",selected=character())
   updateCheckboxInput(session,"actionNAC",value=FALSE)
   updateCheckboxInput(session,"actionNAO",value=FALSE)
   updateCheckboxInput(session,"actionHU",value=FALSE)
   updateCheckboxInput(session,"actionCAP",value=FALSE)
})
if (F) observe({ ## update 'input$conserve'
   verbosing()
   cat("observe 'input$group3' + update 'input$conserve'\n")
   choice <- input$group3
   print(choice)
   if ((length(choice)==1)&&(choice==nameAllHuman)) {
      choice2 <- c('All conservation features'="all",'Do not show'="none",industries)[-2]
      select2 <- choice2[1]
   }
   else {
      choice2 <- choice[choice %in% names(industries)]
      choice2 <- c('All conservation features'="all",'Do not show'="none",industries[choice2])[-2]
      select2 <- choice2[1]
   }
  # exchange$conflict <- NULL
  # exchange$subset <- tail(choice2,-1)
  # print(c(subset=exchange$subset))
   updateSelectInput(session,"industry",choices=choice2,selected=select2)
})
if (T) observeEvent(input$industry, { ## update 'input$actionNAC'
   verbosing()
   cat("observe input$industry + update 'input$actionNAC'\n")
  # updateRadioButtons(session,"actionNAC",selected=character())
   updateCheckboxInput(session,"actionNAC",value=FALSE)
   updateCheckboxInput(session,"actionNAO",value=FALSE)
   updateCheckboxInput(session,"actionHU",value=FALSE)
   updateCheckboxInput(session,"actionCAP",value=FALSE)
})
if (T) observe({ ## update 'input$economy'
   verbosing()
   cat("observe 'input$economy'\n")
   industry <- input$industry
   req(industry)
  # print(c(industry=industry))
  # print(isTRUE(industry %in% industryAbbr$industry))
  # print(c(selection=!is.null(exchange$selection)))
   if (F & !is.null(exchange$selection)) {
      if (input$economy=="skip") {
         return(NULL)
        # choice <- c('Unsupported with selection'="skip")
      }
      else {
         choice <- "skip"
         names(choice) <- names(choiceMap[match(input$economy,choiceMap)])
      }
      updateSelectInput(session,"economy",choices=choice,selected=choice[1])
      return(NULL)
   }
   choice <- choiceMap
   print(choice)
   if ("all" %in% industry) {
      if (allActivity %in% input$activity)
         industry2 <- industryName(industries)
      else
         industry2 <- industryName(industries[input$activity])
   }
   else
      industry2 <- industry
  # print(c(industry2=industry2))
   industry2 <- industryCode(industry2)
   exchange$subset <- industry2
   rHU <- rvHumanUseRaster()
   if (!any(industry2 %in% names(rHU))) {
      choice <- grep("(capr|humanuse)",choice,invert=TRUE,value=TRUE)
   }
  # print(c('industry.'=input$industry))
  # print(c('industry2.'=industry2))
  # print(choice)
   if (F) {
      readyToSelect <- FALSE
      if ((length(industry)==1)&&(industry %in% industryAbbr$industry)) {
         choice <- "SIGHT list is expected here"
         cvalue <- sight[[industryCode(industry)]]
        # print(c(cvalue=cvalue))
        # print(c(cond1=!is.na(cvalue[1])))
        # print(c(cond2=isTRUE(cvalue[1]!="NA")))
         if (!is.na(cvalue[1])) {
            if (isTRUE(cvalue[1]=="NA")) {
               choice <- c('CAP index'="cap",'Not applicable for show'="none")[2]
            }
            else {
               if (length(cvalue)>1)
                  choice <- list('CAP index'="cap",'Can be paired'=cvalue,'Do not show'="none")
               else
                  choice <- list('CAP index'="cap",cvalue,'Do not show'="none")
               readyToSelect <- !TRUE
            }
         }
         else
            choice <- c('Not applicable for show'="none")
         ##~ if ((!is.na(cvalue[1]))||(isTRUE(cvalue[1]!="NA"))) {
            ##~ choice <- c('Do not show'="none",cvalue)
         ##~ }
      }
      else if ((length(industry)==1)&&(isTRUE(industry=="all"))) {
         choice <- list('CAP index'="cap",'Not applicable for show'="none")#[1]
      }
      else if (length(industry)>1)
         choice <- list('CAP index'="cap",'Not applicable for show'="none")
      else
         choice <- list('Do not show'="none"#,'CAP index'="cap"
                       ,'Source (devel)'=spatial_basename(economyList))
      if ("cap" %in% choice) {
         if ("all" %in% industry) {
            if (allActivity %in% input$activity)
               industry2 <- industryName(industries)
            else
               industry2 <- industryName(industries[input$activity])
         }
         else
            industry2 <- industry
        # print(c(industry2=industry2))
         industry2 <- industryCode(industry2)
        # print(c(industry2=industry2))
         if (!any(industry2 %in% names(rHU))) {
            choice <- choice[-1]
           # print(choice)
           # print("SKIP CAP")
           # choice <- choice[-which("cap",choice)]
         }
      }
   }
  # exchange$conflict <- list(industry=input$industry,group=input$group3
  #                          ,season=input$season,economy=input$economy)
  # if (input$economy==nameInit)
   updateSelectInput(session,"economy",choices=choice,selected=choice[1])
})
if (T) observeEvent(input$economy,{
   choice <- tail(choiceMap,-1)
   if (T & input$economy %in% tail(choiceMap,-1)) {
      ind <- match(input$economy,choiceMap)
      choice <- c('Reset map'="none",choiceMap[ind])
     # updateSelectInput(session,"region",choices=nameInit)
      updateSelectInput(session,"economy",label="Reset shown index map for further steps"
                       ,choices=choice,selected=tail(choice,1))
   }
   else if (input$economy %in% choiceMap[1]) {
      industry <- input$industry
      req(industry)
      choice <- choiceMap
      if ("all" %in% industry) {
         if (allActivity %in% input$activity)
            industry2 <- industryName(industries)
         else
            industry2 <- industryName(industries[input$activity])
      }
      else
         industry2 <- industry
      industry2 <- industryCode(industry2)
      exchange$subset <- industry2
      rHU <- rvHumanUseRaster()
      if (!any(industry2 %in% names(rHU))) {
         choice <- grep("(capr|humanuse)",choice,invert=TRUE,value=TRUE)
      }
      choice <- choiceMap
      updateSelectInput(session,"economy",label="Indexes maps",choices=choice,selected=choice[1])
   }
})
if (T) observeEvent(input$industry,{ ## update 'input$industry'
   verbosing()
   cat("observeEvent 'input$industry':\n")
   event <- input$industry
   choice <- NULL
   if (is.null(event))
      choice <- "none"
   else {
      if ((length(event)>1)&&(tail(event,1)=="none"))
         choice <- "none"
      else if ((length(event)>1)&&(tail(event,1)=="all"))
         choice <- "all"
      else {
         if (anyNA(ind <- match(event,unlist(industries)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               choice <- industryCodeName(event[ind2])
            }
         }
      }
   }
   if (length(choice)) {
      exchange$conflict <- NULL
      updateSelectInput(session,"industry",selected=choice)
   }
   if (F & isTRUE(choice=="none"))
      showNotification("Remove layer from map",duration=3)
   if ((FALSE)&&(length(choice)==1)&&(choice!="none")) {
      economy <- choice
      updateSelectInput(session,"economy",choice=economy,selected=economy[1])
   }
})
if (F) observeEvent(input$economy,{ ## update 'input$economy'
   verbosing()
   cat("observeEvent 'input$economy':\n")
   event <- input$economy
   choice <- NULL
   if (is.null(event))
      choice <- "none"
   else {
      if ((length(event)>1)&&(tail(event,1)=="none"))
         choice <- "none"
      else if ((length(event)>1)&&(tail(event,1)=="cap"))
         choice <- "cap"
      else {
         if (anyNA(ind <- match(event,spatial_basename(economyList)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               choice <- event[ind2]
            }
         }
      }
   }
   if (length(choice)) {
      updateSelectInput(session,"economy",selected=choice)
   }
})
if (T) observeEvent(input$activity,{ ## update 'input$activity'
   verbosing()
   cat("observeEvent 'input$activity':\n")
   choice <- NULL
   if (is.null(input$activity))
      choice <- nameAllHuman
   else {
      if ((length(input$activity)>1)&&(tail(input$activity,1)==nameAllHuman))
         choice <- nameAllHuman
      else {
         if (anyNA(ind <- match(input$activity,names(industries)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               choice <- input$activity[ind2]
            }
         }
      }
   }
   if (length(choice)) {
      updateSelectInput(session,"activity",selected=choice)
      exchange$conflict <- NULL
   }
})
if (F) observe({ ## update 'input$group3'
   verbosing()
   cat("observe 'input$group3':\n")
   grList <- c(nameAllCF['3']
              ,unique(taxonCF$group3[taxonCF$CF_code %in% unique(concern$CF_code)]))
   updateSelectInput(session,"group3",selected=input$group3,choices=grList)
})
if (T) observeEvent(input$group3,{ ## update 'input$group3'
   verbosing()
   cat("observeEvent 'input$group3':\n")
   grList3 <- c(nameAllCF['3']
               ,unique(taxonCF$group3[taxonCF$CF_code %in% unique(concern$CF_code)]))
   if (is.null(input$group3)) {
      updateSelectInput(session,"group3",selected=nameAllCF['3'])#,choices=grList)
      exchange$conflict <- NULL
   }
   else {
      if ((length(input$group3)>1)&&(tail(input$group3,1)==nameAllCF['3'])) {
         updateSelectInput(session,"group3",selected=nameAllCF['3'])#,choices=grList)
      }
      else {
         if (anyNA(ind <- match(input$group3,tail(unname(grList3),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"group3",selected=input$group3[ind2]
                               # ,choices=grList
                                )
               exchange$conflict <- NULL
            }
         }
      }
   }
  # s2name <- ifelse(input$group3==nameAllCF['3'],nameAllCF['2'],paste("All",sQuote(input$group3)))
   gr2list <- c(unname(nameAllCF['2'])
               ,unique(taxonCF$group2[taxonCF$CF_code %in% groupCF(input$group3)]))
   if (length(gr2list)==2)
      updateSelectInput(session,"group2",selected=gr2list[2],choices=gr2list[2])
   else
      updateSelectInput(session,"group2",selected=nameAllCF['2'],choices=gr2list)
   gr1list <- c(unname(nameAllCF['1'])
               ,unique(taxonCF$group1[taxonCF$CF_code %in% groupCF(input$group3)]))
   if (length(gr1list)==2)
      updateSelectInput(session,"group1",selected=gr1list[2],choices=gr1list[2])
   else
      updateSelectInput(session,"group1",selected=nameAllCF['1'],choices=gr1list)
   gr0list <- c(unname(nameAllCF['0'])
               ,unique(taxonCF$group0[taxonCF$CF_code %in% groupCF(input$group3)]))
   if (length(gr0list)==2)
      updateSelectInput(session,"group0",selected=gr0list[2],choices=gr0list[2])
   else
      updateSelectInput(session,"group0",selected=nameAllCF['0'],choices=gr0list)
})
if (T) observeEvent(input$group2,{ ## update 'input$group2'
   verbosing()
   cat("observeEvent 'input$group2':\n")
   grList2 <- c(unname(nameAllCF['2'])
              ,unique(taxonCF$group2[taxonCF$CF_code %in% unique(concern$CF_code)]))
   if (is.null(input$group2)) {
      updateSelectInput(session,"group2",selected=nameAllCF['2'])#,choices=grList)
      exchange$conflict <- NULL
   }
   else {
      if ((length(input$group2)>1)&&(tail(input$group2,1)==nameAllCF['2'])) {
         updateSelectInput(session,"group2",selected=nameAllCF['2'])#,choices=grList)
      }
      else {
         if (anyNA(ind <- match(input$group2,tail(unname(grList2),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"group2",selected=input$group2[ind2]
                               # ,choices=grList
                                )
               exchange$conflict <- NULL
            }
         }
      }
   }
   gr2list <- groupCF(c(input$group3,input$group2))
   gr1list <- c(unname(nameAllCF['1'])
               ,unique(taxonCF$group1[taxonCF$CF_code %in% gr2list]))
   if (length(gr1list)==2)
      updateSelectInput(session,"group1",selected=gr1list[2],choices=gr1list[2])
   else
      updateSelectInput(session,"group1",selected=nameAllCF['1'],choices=gr1list)
   gr0list <- c(unname(nameAllCF['0'])
               ,unique(taxonCF$group0[taxonCF$CF_code %in% gr2list]))
   if (length(gr0list)==2)
      updateSelectInput(session,"group0",selected=gr0list[2],choices=gr0list[2])
   else
      updateSelectInput(session,"group0",selected=nameAllCF['0'],choices=gr0list)
})
if (T) observeEvent(input$group1,{ ## update 'input$group1'
   verbosing()
   cat("observeEvent 'input$group1':\n")
   grList1 <- c(nameAllCF['1']
              ,unique(taxonCF$group1[taxonCF$CF_code %in% unique(concern$CF_code)]))
   if (is.null(input$group1)) {
      updateSelectInput(session,"group1",selected=nameAllCF['1'])#,choices=grList)
      exchange$conflict <- NULL
   }
   else {
      if ((length(input$group1)>1)&&(tail(input$group1,1)==nameAllCF['1'])) {
         updateSelectInput(session,"group1",selected=nameAllCF['1'])#,choices=grList)
      }
      else {
         if (anyNA(ind <- match(input$group1,tail(unname(grList1),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"group1",selected=input$group1[ind2]
                               # ,choices=grList
                                )
               exchange$conflict <- NULL
            }
         }
      }
   }
   gr1list <- groupCF(c(input$group3,input$group2,input$group1))
   gr0list <- c(unname(nameAllCF['0'])
               ,unique(taxonCF$group0[taxonCF$CF_code %in% gr1list]))
   if (length(gr0list)==2)
      updateSelectInput(session,"group0",selected=gr0list[2],choices=gr0list[2])
   else
      updateSelectInput(session,"group0",selected=nameAllCF['0'],choices=gr0list)
})
if (T) observeEvent(input$group0,{ ## update 'input$group0'
   verbosing()
   cat("observeEvent 'input$group0':\n")
   grList0 <- c(nameAllCF['0']
              ,unique(taxonCF$group0[taxonCF$CF_code %in% unique(concern$CF_code)]))
   if (is.null(input$group1)) {
      updateSelectInput(session,"group0",selected=nameAllCF['0'])#,choices=grList)
      exchange$conflict <- NULL
   }
   else {
      if ((length(input$group0)>1)&&(tail(input$group0,1)==nameAllCF['0'])) {
         updateSelectInput(session,"group0",selected=nameAllCF['0'])#,choices=grList)
      }
      else {
         if (anyNA(ind <- match(input$group0,tail(unname(grList0),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"group0",selected=input$group0[ind2]
                               # ,choices=grList
                                )
               exchange$conflict <- NULL
            }
         }
      }
   }
})
if (T) observeEvent(input$season,{ ## update 'input$season'
   verbosing()
   cat("observeEvent 'input$season':\n")
   exchange$conflict <- NULL
   if (is.null(input$season)) {
      updateSelectInput(session,"season",selected=nameAllSeason)
   }
   else {
      if ((length(input$season)>1)&&(tail(input$season,1)==nameAllSeason))
         updateSelectInput(session,"season",selected=nameAllSeason)
      else {
         if (anyNA(ind <- match(input$season,tail(unname(seasonList),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"season",selected=input$season[ind2])
               exchange$conflict <- NULL
            }
         }
      }
   }
})
'rvInitEPA' <- reactive({
   verbosing()
   ret <- (length(input$initEPA)>0)&&(input$initEPA>0)
   cat("rvInitEPA:",ret,"\n")
   ret
   
})
if (F) observe({ ## exchange$initEPA
  # req(length(input$actionEPA))
   if ((!exchange$initEPA)&&(isTRUE(input$actionEPA)))
      exchange$initEPA <- TRUE
})
# if (T) observeEvent(rvAOI(),{ ## -- this ignored when all deselected
if (T) observe({ ## regionAddAOI() and fitBounds()
   verbosing()
   isAOI <- !is.null(aoi <- rvAOI())
  # isEPA <- rvInitEPA()
   cat("regionAddAOI():\n")
  # map <- leafletProxy("regionLeaflet")
   map <- regionAddAOI(map=proxyRegion,aoi=aoi,addOverlay=TRUE)#,showEPA=isEPA)
  # map <- indexMap(proxyRegion,"AOI")
   if (!isAOI) {
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=70,value=0),crs=4326)
     # e <- spatial_transform(e,3575)
      bbox <- unname(spatial_bbox(e))
   }
   else {
      bbox <- unname(spatial_bbox(aoi))
   }
   if (F) {
      grBasemap <- as.list(args(conflictBasemap))$group
      prmAOI <- as.list(args(regionAddAOI))
      grAOI <- prmAOI$group
      layerAOI <- prmAOI$layerID
      cat(ifelse(isAOI,"add AOI layer to","remove AOI layer from"),"region map:\n")
      colAOI <- "#092B"
      map <- leafletProxy("regionLeaflet")
     # map <- proxyRegion
      if (F) { ## before
        # map <- clearGroup(map,grAOI)
         map <- removeShape(map,layerAOI)
        # map <- hideGroup(map,grAOI)
        # map <- removeControl(map,layerAOI)
         map <- clearShapes(map) ## bad idea
         map <- addLayersControl(map
                              ,overlayGroups=c(grBasemap
                                             # ,if (isEPA) grEPA
                                              )
                              ,options=layersControlOptions(collapsed=FALSE)
                              )
      }
      if (T & !isAOI) {
         ursa:::.elapsedTime("0904h")
         str(grAOI)
         ##~ map <- clearShapes(map) ## bad idea
         if (T)
            map <- clearGroup(map,grAOI)
         if (T)
            map <- removeShape(map,grAOI)
         if (T)
            map <- addLayersControl(map
                                 ,overlayGroups=c(grBasemap
                                                # ,if (isEPA) grEPA
                                                 )
                                 ,options=layersControlOptions(collapsed=FALSE)
                                 )
         ##~ map <- clearControls(map)
         e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=70,value=0),crs=4326)
        # e <- spatial_transform(e,3575)
         ursa:::.elapsedTime("0904i")
         bbox <- unname(spatial_bbox(e))
      }
      if (isAOI) {
        # spatial_write(aoi,"C:/tmp/aoi.sqlite")
         map <- removeShape(map,layerAOI)
        # map <- hideGroup(map,grAOI)
        # map <- showGroup(map,grAOI)
         ursa:::.elapsedTime("0904c")
        # spatial_write(aoi,"c:/tmp/aoi.sqlite")
         map <- clearGroup(map,grAOI)
         map <- regionAddAOI(map=map,aoi=aoi,addOverlay=FALSE)
         if (F)
            map <- leaflet::addPolygons(map,data=spatial_union(aoi)
                            ,label=grAOI
                            ,color=colAOI
                           # ,weight=0
                           # ,popup=~gsub(";\\s*","\n",name)
                           # ,stroke=TRUE
                            ,fillOpacity=0.2
                            ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                      # ,sendToBack=TRUE
                                                                      # ,bringToFront=TRUE
                                                                       )
                           # ,group=grAOI
                           # ,layerId=grAOI ## only sinlge polygon when multiple selected
                            )
         if (F)
            map <- addLayersControl(map
                                 ,overlayGroups=c(grBasemap
                                                 ,grAOI
                                                # ,if (isEPA) grEPA
                                                 )
                                 ,options=layersControlOptions(collapsed=FALSE)
                                 )
         if (F)
            map <- showGroup(map,grAOI)
         ursa:::.elapsedTime("0904g")
         bbox <- unname(spatial_bbox(aoi))
      }
   }
   ret <- leaflet::fitBounds(map
                     ,lng1=bbox[1],lat1=bbox[2],lng2=bbox[3],lat2=bbox[4]
                     ,options=list(minZoom=2)
                     )
})
if (F) observe({ ## fitBounds()
   verbosing()
   cat("observe for fitBounds():\n")
# observeEvent(input$regionLeaflet_shape_click,{
# observeEvent(rvAOI(),{
  # rvAOI()
  # cat(" ************* CLICKED *************\n")
  # proxyRegion <- leafletProxy("regionLeaflet")
   isEPA <- rvInitEPA()
  # isEPA <- ((length(input$initEPA)>0)&&(input$initEPA>0))
   prmAOI <- as.list(args(regionAddAOI))
   prmEPA <- as.list(args(regionAddEPA))
   grAOI <- prmAOI$group
   layerAOI <- prmAOI$layerID
   grEPA <- prmEPA$group
   isAOI <- !is.null(aoi <- rvAOI())
   cat(ifelse(isAOI,"add AOI layer to","remove AOI layer from"),"region map:\n")
   cat("- AOI --------------------------\n")
   str(aoi)
   cat("- AOI --------------------------\n")
   str(layerAOI)
   cat("- AOI --------------------------\n")
  # proxyRegion <- regionAddAOI(map=proxyRegion,aoi=aoi,addOverlay=FALSE)
   if (!isAOI) {
     # showNotification("remove AOI layer from region map",duration=2)
     # if (file.exists("C:/tmp/aoi.sqlite"))
     #    file.remove("C:/tmp/aoi.sqlite")
     # proxyRegion <- clearGroup(proxyRegion,grAOI)
      proxyRegion <- removeShape(proxyRegion,layerAOI)
     # proxyRegion <- hideGroup(proxyRegion,grAOI)
      proxyRegion <- removeControl(proxyRegion,layerAOI)
     # proxyRegion <- hideGroup(proxyRegion,grAOI)
     # proxyRegion <- clearShapes(proxyRegion) ## bad idea
      proxyRegion <- addLayersControl(proxyRegion
                           ,overlayGroups=c("Arctic SDI",if (isEPA) grEPA)
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
     # proxyRegion <- clearControls(proxyRegion)
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=70,value=0),crs=4326)
     # e <- spatial_transform(e,3575)
      bbox <- unname(spatial_bbox(e))
   }
   if (isAOI) {
      cat("add AOI layer to region map:\n")
     # showNotification(paste("add AOI",spatial_count(aoi),"layer(s) to region map"),duration=2)
     # spatial_write(aoi,"C:/tmp/aoi.sqlite")
     # proxyRegion <- removeShape(proxyRegion,layerAOI)
     # proxyRegion <- hideGroup(proxyRegion,grAOI)
     # proxyRegion <- showGroup(proxyRegion,grAOI)
      proxyRegion <- addLayersControl(proxyRegion
                           ,overlayGroups=c("Arctic SDI"
                                           ,grAOI
                                           ,if (isEPA) grEPA)
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
     # proxyRegion <- showGroup(proxyRegion,grAOI)
      if (F) proxyRegion <- addLayersControl(proxyRegion
                           ,options=layersControlOptions(addOverlayLayer=grAOI)
                           )
      bbox <- unname(spatial_bbox(aoi))
   
   }
   proxyRegion <- leaflet::fitBounds(proxyRegion
                     ,lng1=bbox[1],lat1=bbox[2],lng2=bbox[3],lat2=bbox[4]
                     ,options=list(minZoom=2)
                     )
})
'rvMetricsMap' <- reactive({
   verbosing()
   cat("rvMetricsMap:\n")
   ctable <- rvCrossTable()
   metricsMap(ctable)
})
'rvConcern' <- reactive({
   verbosing()
   cat("rvConcern:\n")
   d <- concernSubset(concern
                     ,group=input$group3
                     ,activity=input$activity
                     ,season=season$activity
                     )
   d
})
'rvIceConcCover' <- reactive({
   verbosing()
   cat("rvIceConcCover:\n")
  # req(isTRUE(input$iceConcDetails))
   showNotification(id="iceConcCover",closeButton=FALSE,duration=120
                   ,if (!staffOnly) "Processing and rendering..." else
                    "Preparing data for ice concentration stats. Pleas wait for rendering."
                   ,type="warning")
   ret <- iceConcCover(CF=rvSelectCF(),industry=rvSelectIndustry())
   removeNotification(id="iceConcCover")
   ret
})
if (F) observe({
  # isolate(rvAOI())
   aoi <- rvAOI()
   cat("observe 'rvAOI(), not Event'\n")
   indexMap(proxyRegion,"AOI")
})
if (T) observeEvent(input$initEPA,{
#observeEvent(rvInitEPA(),{
# observe({
  # req(isTRUE(exchange$initEPA))
   verbosing()
   cat("'input$initEPA' / 'exhchange$initEPA' -- before:\n")
   isEPA <- isTRUE(input$initEPA>0)
  # proxyRegion <- leafletProxy("regionLeaflet")
   if (isEPA)
      shiny::showNotification(id="initEPA",closeButton=FALSE,duration=6
                      ,paste("Rendering EPA is slow."
                            ,"This layer will be appear at soon...")
                      ,type="warning")
  # aoi <- rvAOI()
  # regionAddEPA(proxyRegion,aoi,addPolygon=isEPA)
   indexMap(proxyRegion,"EPA")
   cat("'input$initEPA' / 'exhchange$initEPA' -- after:\n")
})
if (T) observeEvent(input$actionNAC,{
   verbosing()
  # req(isTRUE(input$actionNAC))
  # req(nchar(input$actionNAC)>0)
   cat("initialize NACR map...\n")
   indexMap(proxyRegion,"NACR") ## ,add=isTRUE(input$actionNAC)
})
if (T) observeEvent(input$actionNAO,{
   verbosing()
   cat("initialize NAOR map...\n")
   indexMap(proxyRegion,"NAOR")
  # updateActionLink(input$actionNAC)
})
if (T) observeEvent(input$actionCAP,{
   verbosing()
   cat("initialize CAPR map...\n")
   indexMap(proxyRegion,"CAPR")
})
if (T) observeEvent(input$actionHU,{
   verbosing()
   cat("initialize HU map...\n")
   indexMap(proxyRegion,"humanuse")
})
if (F) observe({ ## addLayersControl(proxyRegion)
   verbosing()
   cat("observe for addLayersControl():\n")
  # showNotification("update controls",duration=3)
   proxyRegion <- leafletProxy("regionLeaflet")
   showAOI <- !is.null(rvAOI())
   showPAs <- isTRUE(input$initEPA>0)
   showNAO <- isTRUE(input$actionNAO>0)
   showNAC <- isTRUE(input$actionNAC>0)
   showCAP <- isTRUE(input$actionCAP>0)
   showHU <- isTRUE(input$actionHU>0)
   if (showAOI)
      grAOI <- as.list(args(regionAddAOI))$group
   if (showPAs)
      grPAs <- as.list(args(regionAddEPA))$group
   if (showNAO)
      grNAO <- c("SR index","SC-P")[2]
   if (showNAC)
      grNAC <- c("MNSR index","OP-P")[2]
   if (showCAP)
      grCAP <- c("CAPR index","OIP-P")[2]
   if (showHU)
      grHU <- "Industrial Activities"
   proxyRegion <- addLayersControl(proxyRegion
                        ,overlayGroups=c(NULL
                                        ,"Arctic SDI"
                                        ,if (showAOI) grAOI
                                        ,if (showPAs) grPAs
                                        ,if (showNAO) grNAO
                                        ,if (showNAC) grNAC
                                        ,if (showCAP) grCAP
                                        ,if (showHU) grHU
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
})
observeEvent(input$comment,{
   verbosing()
   cat("observeEvent 'input$comment'\n")
  # config <- config_read()
   exchange$config$comment <- input$comment
   config_write(exchange$config)
})
observeEvent(input$relative,{
   verbosing()
   cat("observeEvent 'input$relative'\n")
  # config <- config_read()
   exchange$config$relative <- input$relative
   config_write(exchange$config)
})
if (F) observeEvent(input$sleepVerbose,{
   verbosing()
   cat("observeEvent 'input$sleepVerbose'\n")
  # config <- config_read()
   exchange$config$sleepVerbose <- input$sleepVerbose
   config_write(exchange$config)
})
observeEvent(input$sleepValue,{
   verbosing()
   cat("observeEvent 'input$sleepValue'\n")
  # config <- config_read()
   exchange$config$sleepValue <- input$sleepValue
   config_write(exchange$config)
}) 
'rvCommentTable' <- reactive({
   verbosing()
   cat("rvCommentTable\n")
   if (comment_exists()) {
      da <- comment_read()
   }
   else
      da <- data.frame()
   req(cf <- CFCode(rvSelectCF()))
   req(industry <- industryCode(rvSelectIndustry()))
   if (input$submit) {
      isolate({
         cat("submission!\n")
         req(!is.null(opinion <- input$opinion))
         da2 <- data.frame(CF=cf,Industry=industry
                          ,Time=format(Sys.time(),"%Y-%m-%d %H:%M")
                          ,Author=if (isTRUE(nchar(input$author)>0)) input$author else rvUserName()
                          ,Comment=opinion
                          )
         if (nrow(da2)) {
            da <- rbind(da,da2)
            da <- unique(da)
            da <- da[nchar(da$Comment)>0,]
            if (length(da)>0)
               comment_write(da)
         }
      })
   }
   da <- da[da$CF %in% cf & da$Industry %in% industry,]
   req(nrow(da)>0)
   da$CF <- NULL
   da$Industry <- NULL
   da <- da[order(da$Time,decreasing=TRUE),]
   da
})
'rvAllComments' <- reactive({
   verbosing()
   cat("rvAllComments:\n")
   req(comment_exists())
   da <- comment_read()
   req(nrow(da)>0)
   da
})
if (T) observeEvent(input$submit,{
   verbosing()
   cat("observe 'input$submit'\n")
   req(!is.null(opinion <- input$opinion))
   updateTextAreaInput(session,"opinion",value="")
})
if (T) observeEvent(input$rebuildNAC,{ ## eventReactive actionlink
   verbosing()
   cat("observe 'input$rebuildNAC'\n")
   concern <- c(input$mulNAR,input$mulNAY,input$mulNAG)
   if (!identical(as.numeric(concern),as.numeric(exchange$config$concern))) {
     # config <- config_read()
      exchange$config$concern <- concern
      config_write(exchange$config)
      dummy <- "restart is required"
      save(dummy,file=sessionFile)
      prm_upload(sessionFile)
      session$reload()
   }
})
if (F) observeEvent(input$actionNAC,{
   showNotification(paste("showNAC is",input$actionNAC),duration=2)
})
'rvRegion' <- reactive({
   exchange$region
})
observeEvent(input$reset,{
   updateTextInput(session,"pwd",value="")
  # ind <- match(names(re))
  # updateTextInput(session,"customAOI",value=NULL) ## HOW TO update file input?
})
'rvPassword' <- reactive(digest::digest(input$pwd,"crc32"))
'rvAdmin' <- reactive(T & rvPassword()==adminPWD)
'rvUser' <- reactive(exchange$config$user)
'rvPinEntered' <- reactive({
   cat("rvPinEntered:\n")
   if (rvAdmin())
      return(0L)
   if (F & staffOnly)
      return(1L)
   if (is.null(input$pwd))
      return(NULL)
   if (!nchar(input$pwd))
      return(NULL)
   user <- rvUser()
   if (!length(user))
      ind <- NA
   else {
      lapply(user,\(u) {str(u)})
      lapply(user,\(u) u$name)
      ind <- match(input$pwd,sapply(sapply(user,\(u) u$name),getPIN))
      if ((length(ind)==1)&&(!is.na(ind)))
         return(ind)
   }
   if (nchar(input$pwd)>=4)
      ind <- -9L
   if (is.na(ind))
      return(NULL)
   ind
})
'rvAuthorized' <- reactive({
   cat("rvAuthorized:\n")
   req(ind <- rvPinEntered())
   req(ind>=0)
   ind
})
'rvUserName' <- reactive({
   cat("rvUserName:\n")
   ind <- rvPinEntered()
   if (!length(ind))
      ret <- ""
   else if (ind>0) {
      ret <- rvUser()
      ret <- ret[[ind]]$name 
   }
   else if (ind==0)
      ret <- "Administrator"
   else
      ret <- ""
   ret
})
'rvUserInd' <- reactive({
   req(user <- rvUser())
   ind <- match(input$userName,sapply(user,\(u) u$name))
   req(!is.na(ind))
   user[[ind]]
})
observeEvent(input$userForgot,{
  # ind <- rvPassword()
   showNotification(getPIN(input$userName),duration=3)
})
observeEvent(input$userRemove,{
   cat("observeEvent 'input$userRemove'\n")
   req(nchar(input$userName)>0)
   user <- exchange$config$user
   ind <- match(input$userName,sapply(user,\(u) u$name))
   str(ind)
   req(!is.na(ind))
   user <- user[-ind]
   exchange$config$user <- user
   config_write(exchange$config)
   showNotification(paste(dQuote(input$userName),"is removed"),duration=3)
   updateSelectInput(session,"userName",selected="")
})
observeEvent(input$userAdd,{
   req(nchar(input$userAdd)>0)
   cat("observeEvent 'input$userAdd'\n")
   newUser <- list(name=input$userNew
                  ,level=0L
                  ,ncolor=exchange$config$ncolor
                  ,quantile=exchange$config$quantile
                  )
   user <- exchange$config$user
   if (is.null(user))
      user <- list(newUser)
   else
      user <- c(user,list(newUser))
   user <- user[sapply(user,\(u) nchar(u$name)>0)]
  # user <- user[!duplicated(user$name),]
   exchange$config$user <- user
   config_write(exchange$config)
   showNotification(paste(dQuote(input$userNew),"is added"),duration=3)
   updateTextInput(session,"userNew",value="")
})
observe({ ## update 'session$userName'
   cat("observeEvent 'rvUserName'\n")
   req(rvAdmin())
   userName <- rvUser()
   userName <- sapply(userName,\(u) u$name)
  # userName <- rvUser()$name
  # ind <- match(input$userName,userName)
  # if (!is.na(ind))
  #    selected <- 
   updateSelectInput(session,"userName",choices=userName,selected=input$userName)
})
observeEvent(input$userName,{
   cat("observeEvent 'input$userName' --> update user$level\n")
   req(nchar(input$userName)>0)
   user <- exchange$config$user
   ind <- match(input$userName,sapply(user,\(u) u$name))
   req(!is.na(ind))
   updateSelectInput(session,"userLevel",selected=user[[ind]]$level)
})
observeEvent(input$userLevel,{
   cat("observeEvent 'input$userLevel'\n")
  # req(nchar(input$userName)>0)
  # user <- exchange$config$user
   req(rvAdmin())
   user <- rvUser()
  # ind <- rvAuthorized()
   ind <- match(input$userName,sapply(user,\(u) u$name))
   level <- user[[ind]]$level
   req(level!=input$userLevel)
   exchange$config$user[[ind]]$level <- input$userLevel
   showNotification(paste(dQuote(input$userName),"user level is updated"),duration=3)
   config_write(exchange$config)
})
observeEvent(input$ncolor,{
   verbosing()
   cat("observeEvent 'input$ncolor'\n")
  # config <- config_read()
   if (isTRUE((ind <- rvAuthorized())>0)) {
      print("   user")
      exchange$config$user[[ind]]$ncolor <- input$ncolor
   }
   else {
      print("   admin")
      exchange$config$ncolor <- input$ncolor
   }
   config_write(exchange$config)
})
observeEvent(input$levelNAC,{
   verbosing()
   cat("observeEvent 'input$levelNAC (quantile)'\n")
  # config <- config_read()
   if (isTRUE((ind <- rvAuthorized())>0)) {
      print("   user")
      exchange$config$user[[ind]]$quantile <- input$levelNAC
   }
   else {
      print("   admin")
      exchange$config$quantile <- input$levelNAC
   }
   config_write(exchange$config)
})
if (T) observe({ ## # observeEvent(rvAuthorized,{
   cat("update user settings:\n")
   req(ind <- rvAuthorized())
   user <- rvUser()
   if (ind>0) {
      updateSelectInput(session,"ncolor",selected=user[[ind]]$ncolor)
      updateSelectInput(session,"levelNAC",selected=user[[ind]]$quantile)
   }
   else if (ind==0) {## admin
      ind2 <- match(input$userName,user$name)
      if ((FALSE)&&(length(ind2))&&(!is.na(ind2))) {
         updateSelectInput(session,"ncolor",selected=user$ncolor[ind2])
      }
      else {
         updateSelectInput(session,"ncolor",selected=exchange$config$ncolor)
         updateSelectInput(session,"levelNAC",selected=exchange$config$quantile)
      }
   }
})
'rvIsComment' <- reactive({
   exchange$config$comment
})
'rvUserInitials' <- reactive({
   ind <- rvAuthorized()
   if (!length(ind))
      ret <- ""
   else if (ind==0)
      ret <- "Admin"
   else if (ind>0)
      ret <- gsub("(\\w)(\\w*)","\\U\\1.",rvUser()[[ind]]$name,perl=TRUE)
   else
      ret <- ""
   ret
})
'rvRegionSF' <- reactive({
   cat("rvRegionSF:\n")
   if (is.null(input$customAOI))
      return(regionSF)
   fname <- input$customAOI$datapath
   session_grid(pu)
   reg <- spatial_read(fname)
   if (!length(spatial_fields(reg))) {
      spatial_data(reg) <- data.frame(region="User defined")
   }
   else {
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
      if (!is.character(reg$region))
         reg$region <- as.character(reg$region)
   }
   reg <- reg[!is.na(reg$region),]
   regionSF <- c(regionSF,list('<Custom AOI>'=spatial_transform(reg,4326)))
   if (length(ind <- which(is.na(match(names(regionSF),names(regionU)))))>0) {
      session_grid(blank)
      for (i in ind) {
         regU <- list(ursa:::.fasterize(regionSF[[i]]))
         names(regU) <- names(regionSF)[i]
         regionU <- c(regionU,regU)
      }
   }
   regionSF
})
'rvRegionU' <- reactive({
   cat("rvRegionU:")
   regionSF <- rvRegionSF()
   if (!length(ind <- which(is.na(match(names(regionSF),names(regionU))))))
      return(regionU)
   session_grid(blank)
   for (i in ind) {
      regU <- list(ursa:::.fasterize(regionSF[[i]]))
      names(regU) <- names(regionSF)[i]
      regionU <- c(regionU,regU)
   }
   regionU
})
'rvReviewComments' <- reactive({
   isTRUE(!is.null(input$allComments_rows_selected))
})
'rvSeason' <- reactive({
   season <- match(input$season,seasonList)-1L
   if (0L %in% season)
      season <- seq(12)
   season
})
'rvHumanUseRaster' <- reactive({
   huAmount(epoch=input$epoch,season=rvSeason(),subset=rvSubsetIndustry())
})
observeEvent(input$'thClickRSC-PU',{ ## 'SR'
   showModal(modalDialog(title="RSC-PU – Relative Significant Concern Level for a given Area of Interest by Planning Units"
            ,HTML("RSC-PU the index showing the significant cumulative concern level for an AOI for all CFs and all IAs within the AOI and then averaged by PU and relative to the average significant concern level for a PU in the ArcNet domain. These indexes are helpful because they are independent of the size of an AOI, which enables the comparison of different AOI (such as PACs). The closer index RSC-PU to 100% the closer cumulative significant concern level for a selected AOI to the average concern level for the entire ArcNet domain. If it is higher than 100% than it means that the significant concern level for a given AOI is higher compared to the average for the ArcNet domain.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROC-PU',{ ## 'MNSR'
   showModal(modalDialog(title="ROC-PU – Relative Overall Concern Level for a given Area of Interest by Planning Units"
            ,HTML("ROC-PU the index showing the overall cumulative concern level for an AOI where Overall Concern levels (Significant, Notable, and Minor) are summed for all CFs and all IAs within the AOI and then averaged by PU and relative to the average concern level for a PU in the ArcNet domain. These indexes are helpful because they are independent of the size of an AOI, which enables the comparison of different AOI (such as PACs). The closer index ROC-PU to 100% the closer overall concern level for a selected AOI to the average concern level for the entire ArcNet domain. If it is higher than 100% than it means that the overall concern level for a given AOI is higher compared to the average for the ArcNet domain.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickRSC-AOI',{ ## 'SA'
   showModal(modalDialog(title="RSC-AOI - Relative Significant Concern Level for a given Area of Interest "
            ,HTML("RSC-AOI is the index showing the significant cumulative concern level for an AOI where only Significant Concern levels are considered and summed for all CFs and all IAs within the AOI and then compared to a baseline representing the maximum possible concern for all CFs within the given area and allow for comparison of different AOI.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROC-AOI',{ ## 'MNSA'
   showModal(modalDialog(title="ROC-AOI - Relative Overall Concern Level for a given Area of Interest"
            ,HTML("ROC-AOI is the index showing the overall cumulative concern level for an AOI where Overall Concern levels (Significant, Notable, and Minor) are summed for all CFs and all IAs within the AOI and then compared to a baseline representing the maximum possible concern for all CFs within the given area and allow for comparison of different AOI. The closer ROC-AOI to 100% the higher overall concern for a given AOI is.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickOIP-P',{ ## 'CAPR'
   showModal(modalDialog(title="OIP-P - Overall Industrial Pressure Level calculated for each Planning Unit"
            ,HTML("OIP-P is an index calculated for mapping overall industrial pressure and is calculated for each PU independently by multiplying amount of each activity present in a given PU with amount of each CF in a given PU by the level of concern caused by the industrial activity and then summing the results for each IA-CF pair.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROIP-AOI',{ ## 'CAP'
   showModal(modalDialog(title="ROIP-AOI - Relative Overall Industrial Pressure for a given Area of Interest"
            ,HTML("ROIP-AOI index is calculated as a percentage of a maximum potential pressure for AOI. It compares level of industrial pressure for AOI to its highest possible theoretical values and highlights areas where the pressure is the highest due to both – amount of industrial activities and susceptibility of conservation features to concerns arising from these activities.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickAA-PU',{ ## 'AAR'
   showModal(modalDialog(title="AA-PU – Relative Industrial Activity amount for a given Area of Interest by Planning Units"
            ,HTML("AA-PU is a measure of the cumulative amount or occurrence of industrial activities per PU, normalised and averaged for each PU. It is indicative of the overall industrial presence that exists for each AOI compared to the average presence across the ArcNet domain.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROIP-CF',{ ## 'CAPCF'
   showModal(modalDialog(title="ROIP-CF – Relative Overall Industrial Pressure for a given Conservation Feature"
            ,HTML("ROIP-CF index is calculated as a proportion of the maximum possible industrial pressure (baseline scenario) defined as if the amount of industrial activities stays as it is and the concern level for a given CF within given AOI is the highest possible (significant concern).")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickRSC-CF',{ ## 'MNS/B for CF'
   showModal(modalDialog(title="Relative Significant Concern Level for a given Conservation Feature"
            ,HTML(readLines("RSC-CF is the index showing the significant cumulative concern level for a CF where only Significant Concern are summed for all IAs which occur within the CF area and then compared to a baseline that represents the maximum possible concern level for that CF. It is used to identify CFs whose conservation goals are at the greatest risk. The closer RSC-CF to 100% the higher significant concern for a given CF is."))
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROC-CF',{ ## 'S/B for CF'
   showModal(modalDialog(title="ROC-CF - Relative Overall Concern Level for a given Conservation Feature"
            ,HTML("ROC-CF is the index showing the overall cumulative concern level for a CF where Overall Concern levels (Significant, Notable, and Minor) are summed for all IAs which occur within the CF area and then compared to a baseline that represents the maximum possible concern level for that CF. It is used to identify CFs whose conservation goals are at the greatest risk. The closer ROC-CF to 100% the higher overall concern for a given CF is.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROC-IA',{
   showModal(modalDialog(title="ROC-IA - Relative Overall Concern Level for a given Industrial Activity"
            ,HTML("ROC-IA is the index showing the overall cumulative concern level for an IA where Overall Concern levels (Significant, Notable, and Minor) are summed for all CFs which occur within the area where IA can theoretically occur and then compared to a baseline that represents the maximum possible concern level for that IA. CF coverage is applied to scale the concern levels of a CF by the proportion of that CF that occurs in the AOI. It is used to identify IAs that are sources of the greatest risk and IAs that cause only minor risk for CFs within selected AOI. The closer ROC-IA to 100% the higher overall concern the IA causes.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROIP-IA',{
   showModal(modalDialog(title="ROIP-IA - Relative Overall Industrial Pressure from a given Industrial Activity"
            ,HTML("ROIP-IA index is calculated as a proportion of the maximum possible industrial pressure (baseline scenario) defined as if the amount of a given IA stays as it is and the concern level for all CFs within the IA distribution within given AOI is the highest possible (significant concern).")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROC-M',{
   showModal(modalDialog(title="ROC-M - Relative Overall Concern Level for a given month"
            ,HTML("ROC-M is the index showing the overall cumulative concern level for a month where Overall Concern levels (Significant, Notable, and Minor) are summed for all CFs and all IAs which within an and then compared to a baseline that represents the maximum possible concern level for that month. The closer ROC-M to 100% the higher overall concern for a given month is.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickROIP-M',{
   showModal(modalDialog(title="ROIP-M - Relative Overall Industrial Pressure for a given month"
            ,HTML("ROIP-M index is calculated as a proportion of the maximum possible industrial pressure (baseline scenario) defined as if the amount of all IAs within a given AOI stays as it is and the concern level for all CFs within the AOI is the highest possible (significant concern) for a selected month.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickOC-P',{
   showModal(modalDialog(title="OC-P – Overall Concern level calculated for each Planning Unit"
            ,HTML("OC-P is an index calculated for mapping overall concern and is calculated for each PU independently. It is calculated as the overall cumulative concern level for a PU where Overall Concern levels (Significant, Notable, and Minor) are summed for all CFs and all IAs within the PU and then compared to a baseline representing the maximum possible concern for all CFs within the given PU and multiplied by amount of the CFs within the PU.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickSC-P',{
   showModal(modalDialog(title="SC-P - Significant Concern level calculated for each Planning Unit"
            ,HTML("SC-P is an index calculated for mapping significant concern and is calculated for each PU independently. It is calculated as the significant cumulative concern level for a PU where Significant Concern levels are summed for all CFs and all IAs within the PU and then compared to a baseline representing the maximum possible concern for all CFs within the given PU and multiplied by amount of the CFs within the PU.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickOIP-P',{
   showModal(modalDialog(title="OIP-P - Overall Industrial Pressure Level calculated for each Planning Unit"
            ,HTML("OIP-P is an index calculated for mapping overall industrial pressure and is calculated for each PU independently by multiplying amount of each activity present in a given PU with amount of each CF in a given PU by the level of concern caused by the industrial activity and then summing the results for each IA-CF pair.")
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
observeEvent(input$'thClickFoo',{
   showModal(modalDialog(title="Lorem Ipsum"
            ,HTML(readLines("https://loripsum.net/api/3/decorate/ul"))
            ,easyClose=TRUE
            ,footer = NULL
            ))
})
