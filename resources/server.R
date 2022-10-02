source("resources/global.R",encoding="UTF-8")
source("resources/header.R",encoding="UTF-8")
exchange <- reactiveValues(editor=NULL,overlay=NULL,selection=NULL
                          ,prev=integer(),curr=integer() ## need for removing forgiven clean
                          ,cd=character(),domain=NULL
                          ,CF=NULL,industry=NULL,conflict=NULL#,initEPA=FALSE
                          )
proxyCross <- DT::dataTableProxy("cross")
proxyOnlyIndustry <- DT::dataTableProxy("onlyIndustry")
proxyOnlyCF <- DT::dataTableProxy("onlyCF")
proxyIndustrydata <- DT::dataTableProxy("industrydata")
proxyCFdata <- DT::dataTableProxy("cfdata")
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
   cat("observe input$refion: update input$predefined:\n")
  # if (length(exchange$curr)>0)
  #    exchange$prev <- exchange$curr
  # else
  #    exchange$prev <- -20L
  # if (!length(exchange$curr))
  #    exchange$curr <- -20L
  # else
  #    exchange$curr <- -70L
   if (input$region %in% names(regionSF))
      updateSelectInput(session,"predefined"
                       ,label="Select region of interest"
                       ,choices=c(nameClick,regionSF[[input$region]][[1]])[1]
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
      opW <- options(warn=1)
      cat("0918a -- dplyr::select_() deprecation -- begin")
      gs <- selection()
      cat("0918a -- dplyr::select_() deprecation -- end")
      options(opW)
      ##~ cat("----------\n")
      ##~ str(selection)
      ##~ cat("----------\n")
      ##~ str(gs)
      ##~ cat("----------\n")
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
         ind <- as.numeric(gs[which(gs$selected==TRUE),"id"])
         if (FALSE) {
            cat("         selected:\n")
            cat("str(exchange$selection):\n")
            cat("============================+++++++++++++++++++\n")
            str(exchange$selection)
            cat("str(gs):\n")
            cat("============================+++++++++++++++++++\n")
            str(exchange$prev)
           # str(exchange$curr)
            str(gs)
            cat("============================+++++++++++++++++++\n")
            cat("str(ind):\n")
            str(ind)
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
                 # print(c('ind2.'=ind2))
                  s <- regionSF[[input$region]][ind[ind2],] ## class 'st_sf'
                  spatial_data(s) <- da[ind2,]
                 # exchange$curr <- rbind(exchange$curr,spatial_data(s))
               }
               else if (length(ind3)) {
                 # print(c('ind3.'=ind3))
                  s <- regionSF[[input$region]][ind[ind3],] ## class 'st_sf'
                  spatial_data(s) <- da[ind3,]
               }
               else if (length(ind)<0){
                  s <- regionSF[[input$region]][ind,] ## class 'st_sf'
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
        # print("just an 's'")
        # s <- NULL
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
  # ind <- input$tbl_rows_selected
  # if (is.integer(ind))
  #    exchange$cf <- rvActivityStat()[ind,]
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
if (F) observe({ ## update 'input$industry'
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
if (F) observe({ ## update 'input$cfcode'
   print("observe crosstable, update 'cfcode' input")
   cell <- input$cross_rows_selected
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
'rvAOI' <- reactive({
   cat("rvAOI():\n")
   exchange$selection
})
'rvActivityMap' <- reactive({
   cat("rvActivityMap():\n")
  # conflict <- input$industry
   conflict <- exchange$conflict
   str(conflict)
   if (is.null(conflict))
      return(NULL)
   industry <- conflict$industry
   if ((length(industry)==1)&&(industry=="all")) {
      if (nameAllHuman %in% input$activity)
         industry <- industries |> unlist() |> unname()
      else
         industry <- industries[input$activity] |> unlist() |> unname()
   }
   industry <- industry[industry %in% unname(unlist(industries))]
   if (!length(industry))
      return(NULL)
   season <- conflict$season
   group <- conflict$group
   group <- groupList[na.omit(match(group,groupList))]
   str(group)
   if (!length(group))
      return(NULL)
   group <- names(group)
  # a <- rvActivity()$map
  # print(industry)
  # iname <- unname(industry)
   str(list(industry=industry,season=season,group=group))
   print("next to 'interimMap'")
   a <- interimMap(industry=industry,season=season,group=group)
   a
})
'rvConflictMap' <- reactive({
   cat("rvConflictMap():\n")
  # plutil::timeHint(as.character(input$customize))
   a <- if (T) rvActivityMap() else NULL
   if (is.null(a))
      return(conflictBasemap())
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
   m <- conflictMap(d6)
   m
})
observeEvent(input$drawIndustry,{ ## eventReactive actionlink
  # showNotification(paste("'drawIndustry' is clicked:",input$drawIndustry),duration=3)
   exchange$conflict <- list(industry=input$industry,group=input$group,season=input$season)
})
'rvActivityStat' <- reactive({
   if (isTRUE(exchange$domain)) {
      cat("rvActivityStat() domain is TRUE:\n")
      return(crosstable())
   }
   cat("rvActivityStat():\n")
  # return(NULL)
   if ((is.null(input$sheet))||(is.null(input$column)))
      activity <- "not applicable"
   else {
      if ((input$sheet==nameInit)||(input$column==nameInit))
         return(NULL)
      activity <- paste0(input$sheet,sepRules,input$column)
   }
   cat("------------\n")
   group <- names(groupList[match(input$group,groupList)])
   str(input$activity)
   str(names(industries))
   if (all(input$activity %in% names(industries)))
      activity <- input$activity
   else
      activity <- "all"
   str(activity)
   str(rules$activity)
   str(vulner$industry)
   cat("------------\n")
   aoi <- rvAOI()
  # if (!is.null(aoi))
  #    saveRDS(aoi,"c:/tmp/interim_stat.rds")
  # interim(activity,group=group,aoi=aoi,season=input$season,simplify="stat")
   ##~ exchange$domain <- FALSE
   ##~ plutil::timeHint(paste("domain is",exchange$domain))
   ret <- crosstable(aoi=aoi,group=group,activity=activity)
   ret <- ret[ret$'Cover'>=input$omitPercent,]
   ret
})
'rvCustomer' <- reactive({
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
                                           ,"Arctic SDI"
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
if (T) { ## CFSelection
   observeEvent(input$onlyCF_row_last_clicked,{
      cat(as.character(Sys.time())
         ,"synchro CF selection -- from 'onlyCF' to 'industrydata'\n")
      ind3c <- input$onlyCF_row_last_clicked
      ind3s <- input$onlyCF_rows_selected
      clear3 <- is.null(ind3s)
      if (clear3) {
         cat("   clear row\n")
         if (useExchange)
            exchange$CF <- NULL
         if (!useExchange) {
            DT::selectRows(proxyIndustrydata,NULL)
         }
      }
      else {
         da <- rvActivityStat()
         cname <- rownames(da)[ind3s]
         if (!length(cname))
            cname <- NULL
         else {
            cat("   select row",dQuote(cname),"\n")
            if (useExchange)
               exchange$CF <- cname
         }
         if (!useExchange) {
            ind2 <- ind3s # match(cname,unlist(industries))
            DT::selectRows(proxyIndustrydata,ind2)
         }
      }
   })
   if (F) observe({
     # prev <- rvSelectCF()
      if (isFALSE(exchange$domain)) {
         cat(as.character(Sys.time())
            ,"synchro CF selection -- from 'cross' to 'industrydata'\n")
         ind1s <- input$cross_rows_selected
         clear1 <- is.null(ind1s)
        # showNotification(paste("observeEvent: input$table1_columns_selected:"
        #    ,if (clear1) "NULL" else as.character(ind1s)),duration=dur)
         if (clear1) {
            cat("   clear row\n")
           # print(data.frame(prev=ifelse(is.null(prev),"NULL",prev),cname="NA"))
            if (useExchange)
               exchange$CF <- NULL
            if (!useExchange) {
               DT::selectRows(proxyIndustrydata,NULL)
            }
         }
         else {
            da <- rvActivityStat()
            cname <- rownames(da)[ind1s]
            cat("   select row",dQuote(cname),"\n")
           # print(data.frame(prev=ifelse(is.null(prev),"NULL",prev),cname=cname))
            if (useExchange)
               exchange$CF <- cname
            if (!useExchange) {
               b <- rvHumanUseIndustry()
               if (!is.null(b)) {
                 # str(b)
                 # str(rownames(b))
                 # str(b[[1]])
                  ind2 <- match(rownames(da)[ind1s],b[[1]]) ## abbr in rownames(b)
                 # str(ind2)
                 # showNotification("need to select row refferred to selected industry",duration=3)
                  DT::selectRows(proxyIndustrydata,ind2)
               }
            }
         }
      }
   })
   observeEvent(input$industrydata_row_last_clicked,{
     # prev <- exchange$CF
      cat(paste0(as.character(Sys.time())
                ," synchro CF selection -- from 'industrydata' to '"
                ,ifelse(isFALSE(exchange$domain),"cross","onlyCF"),"'\n"))
      ind2c <- input$industrydata_row_last_clicked
      ind2s <- input$industrydata_rows_selected
      clear2 <- is.null(ind2s)
     # showNotification(paste("observeEvent: input$table2_row_last_clicked:"
     #    ,if (clear2) "NULL" else as.character(ind2s)),duration=3)
      if (clear2) {
         cat("   clear row\n")
         if (useExchange)
            exchange$CF <- NULL
         if (!useExchange) {
            if (isFALSE(exchange$domain))
               DT::selectRows(proxyCross,NULL)
            if (isTRUE(exchange$domain))
               DT::selectRows(proxyOnlyCF,NULL)
         }
      }
      else {
         b <- rvHumanUseIndustry()
         cname <- b[[1]][ind2s]
         cat("   select row",dQuote(cname),"\n")
         if (useExchange)
            exchange$CF <- cname
         if (!useExchange) {
            da <- rvActivityStat()
            ind1 <- match(cname,rownames(da)) ## abbr in rownames(b)
           # str(ind1)
            if (isFALSE(exchange$domain)) {
               DT::selectRows(proxyCross,ind1)
            }
            if (isTRUE(exchange$domain)) {
               DT::selectRows(proxyOnlyCF,ind1)
            }
         }
      }
   })
}
'rvSelectCF' <- reactive({
   cat("rvSelectCF():\n")
   if (useExchange)
      return(exchange$CF)
   if (length(ind <- input$cross_rows_selected)) {
      if (!is.null(da <- rvActivityStat())) {
         ret <- rownames(da)[ind]
        # exchange$CF <- ret
         print("input$cross_rows_selected")
         return(ret)
      }
   }
   if (length(ind <- input$onlyCF_rows_selected)) {
      if (!is.null(da <- rvActivityStat())) {
         ret <- rownames(da)[ind]
        # exchange$CF <- ret
         print("input$onlyCF_rows_selected")
         return(ret)
      }
   }
  # exchange$CF <- NULL
   NULL
})
if (T) { ## industrySelection
   observeEvent(input$onlyIndustry_row_last_clicked,{
      cat(as.character(Sys.time())
         ,"synchro industry selection -- from 'onlyIndustry' to 'cfdata'\n")
      ind3c <- input$onlyIndustry_row_last_clicked
      ind3s <- input$onlyIndustry_rows_selected
      clear3 <- is.null(ind3s)
      if (clear3) {
         cat("   clear row\n")
         if (useExchange)
            exchange$industry <- NULL
         if (!useExchange) {
            DT::selectRows(proxyCFdata,NULL)
         }
      }
      else {
         da <- rvActivityStat()
         cname <- colnames(da)[ind3s]
         if (!length(cname))
            cname <- NULL
         else {
            cat("   clear row\n")
            if (useExchange)
               exchange$industry <- cname
         }
         if (!useExchange) {
            ind2 <- match(cname,unlist(industries))
            DT::selectRows(proxyCFdata,ind2)
         }
      }
   })
   observe({
      if (isFALSE(exchange$domain)) {
         cat(as.character(Sys.time())
            ,"synchro industry selection -- from 'cross' to 'cfdata'\n")
         ind1s <- input$cross_columns_selected
         clear1 <- is.null(ind1s)
        # showNotification(paste("observeEvent: input$table1_columns_selected:"
        #    ,if (clear1) "NULL" else as.character(ind1s)),duration=dur)
         if (clear1) {
            if (useExchange)
               exchange$industry <- NULL
            cat("   clear row\n")
            if (!useExchange) {
               if (!is.null(input$cfdata_rows_selected))
                  DT::selectRows(proxyCFdata,NULL)
            }
         }
         else {
            da <- rvActivityStat()
            cname <- colnames(da)[ind1s]
            if (!length(cname))
               cname <- NULL
            else {
               cat("   select row\n")
               if (useExchange)
                  exchange$industry <- cname
            }
            if (!useExchange) {
               b <- rvHumanUseCF()
               if (!is.null(b)) {
                 ##~ # str(b)
                 ##~ # str(rownames(b))
                 ##~ # str(b[[1]])
                  ind2 <- match(colnames(da)[ind1s],b[[1]]) ## abbr in rownames(b)
                 # str(ind2)
                 # showNotification("need to select row refferred to selected CF",duration=3)
                  DT::selectRows(proxyCFdata,ind2)
               }
            }
         }
      }
   })
   observeEvent(input$cfdata_row_last_clicked,{
      cat(paste0(as.character(Sys.time())
                ," synchro industry selection -- from 'cfdata' to '"
                ,ifelse(isFALSE(exchange$domain),"cross","onlyIndustry"),"'\n"))
      ind2c <- input$cfdata_row_last_clicked
      ind2s <- input$cfdata_rows_selected
      clear2 <- is.null(ind2s)
     # showNotification(paste("observeEvent: input$table2_row_last_clicked:"
     #    ,if (clear2) "NULL" else as.character(ind2s)),duration=3)
      if (clear2) {
         if (useExchange)
            exchange$industry <- NULL
         if (isFALSE(exchange$domain))
            cat("   clear column\n")
         if (isTRUE(exchange$domain))
            cat("   clear row\n")
         if (!useExchange) {
            if (isFALSE(exchange$domain)) {
               DT::selectColumns(proxyCross,NULL)
            }
            if (isTRUE(exchange$domain)) {
               DT::selectRows(proxyOnlyIndustry,NULL)
            }
         }
      }
      else {
         b <- rvHumanUseCF()
         cname <- b[[1]][ind2s]
         if (!length(cname))
            cname <- NULL
         else {
            if (useExchange)
               exchange$industry <- cname
         }
         if (isFALSE(exchange$domain))
            cat("   select column\n")
         if (isTRUE(exchange$domain))
            cat("   select row\n")
         if (!useExchange) {
            if (isFALSE(exchange$domain)) {
               da <- rvActivityStat()
               ind1 <- match(b[[1]][ind2s],colnames(da)) ## abbr in rownames(b)
               DT::selectColumns(proxyCross,ind1)
            }
            if (isTRUE(exchange$domain)) {
              # da <- rvActivityStat()
               cname <- b[[1]][ind2s]
               ind1 <- match(cname,industryAbbr$industry)
              # cname <- colnames(da)
              # cname <- cname[which(!is.na(match(cname,unlist(industries))))]
              # ind1 <- match(cname,unlist(industries)) ## abbr in rownames(b)
               DT::selectRows(proxyOnlyIndustry,ind1)
            }
         }
      }
   })
}
'rvSelectIndustry' <- reactive({
   cat("rvSelectIndustry():\n")
   if (useExchange)
      return(exchange$industry)
   if (length(ind <- input$cross_columns_selected)) {
      if (!is.null(b <- rvActivityStat())) {
         cname <- colnames(b)[ind]
         if (cname %in% unlist(industries)) {
           # proxyCFdata %>% selectRows(NULL)
           # proxyOnlyIndustry %>% selectRows(NULL)
            ret <- cname
           # exchange$industry <- ret
            print("input$cross_columns_selected")
            return(ret)
         }
      }
   }
   if (length(ind <- input$onlyIndustry_rows_selected)) {
      if (!is.null(b <- rvActivityStat())) {
        # print(colnames(b))
         cname <- colnames(b)
         cname <- cname[which(!is.na(match(cname,unlist(industries))))]
        # cname <- cname[cname %in% unlist(industries)]
         cname <- cname[ind]
        # cname <- colnames(b)[ind]
         if (cname %in% unlist(industries)) {
           # proxyCFdata %>% selectRows(NULL)
           # proxyCross %>% selectColumns(NULL)
            ret <- cname
           # exchange$industry <- ret
            print("input$onlyIndustry_rows_selected")
            return(ret)
         }
      }
   }
  # exchange$industry <- NULL
   NULL
})
'rvHumanUseIndustry' <- reactive({
   cat("rvHumanUseIndustry():\n")
   industry <- rvSelectIndustry()
  # industry <- exchange$industry
  # industry <- "Mass tourism"
  # plutil::timeHint(paste("human use:",industry," --- ",exchange$industry))
   req(industry %in% unlist(industries))
   da <- human_use(industry)
   if (T) {
      st <- rvActivityStat()
      if (!is.null(st))
         da <- da[da$'CF Code' %in% rownames(st),]
   }
   da
})
'rvHumanUseCF' <- reactive({
   cat("rvHumanUseCF():\n")
   cf <- rvSelectCF()
  # cf <- exchange$cf
  # cf <- "9038"
  # plutil::timeHint(paste("human use:",cf," --- ",exchange$cf))
   req(cf %in% spec$cf)
   da <- human_use(cf)
   if (T) {
      st <- rvActivityStat()
      if (!is.null(st))
         da <- da[da$'Industry' %in% colnames(st),]
   }
   da
})
'rvIndustryGroup' <- reactive({
   cat("rvIndustryGroup():\n")
   req(industry <- rvSelectIndustry())
   str(industry)
   u <- unlist(industries)
   a <- sapply(industries,function(a) industry %in% a)
   str(a)
   ret <- names(industries)[sapply(industries,function(a) industry %in% a)]
  # names(industry)
   ret 
})
if (F) { ## proxy select columns/rows NULL
   observeEvent(input$industrydata_rows_selected,{ ## CF
      proxyOnlyCF %>% selectRows(NULL)
      proxyCross %>% selectRows(NULL)
   })
   observeEvent(input$onlyCF_rows_selected,{ ## CF
      proxyIndustrydata %>% selectRows(NULL)
      proxyCross %>% selectRows(NULL)
   })
   observeEvent(input$cross_rows_selected,{ ## CF
      proxyIndustrydata %>% selectRows(NULL)
      proxyOnlyCF %>% selectRows(NULL)
   })
   observeEvent(input$cfdata_rows_selected,{ ## industry
      proxyOnlyIndustry %>% selectRows(NULL)
      proxyCross %>% selectColumns(NULL)
   })
   observeEvent(input$onlyIndustry_rows_selected,{ ## industry
      proxyCFdata %>% selectRows(NULL)
      proxyCross %>% selectColumns(NULL)
   })
   observeEvent(input$cross_columns_selected,{ ## industry
      proxyCFdata %>% selectRows(NULL)
      proxyOnlyIndustry %>% selectRows(NULL)
   })
}
if (F) { 
   observeEvent(input$cross_columns_selected,{
      ind <- input$cross_columns_selected
      b <- rvHumanUseCF()
      da <- rvActivityStat()
      cname <- colnames(da)[ind]
     # ind2 <- match(cname)
      ind2 <- match(cname,industryAbbr$industry)
      cname <- industryAbbr$abbr[ind2]
      ind3 <- match(cname,rownames(b))
     # plutil::timeHint(paste(cname,collapse=" "));Sys.sleep(1)
     # plutil::timeHint(paste(rownames(b),collapse=" "));Sys.sleep(1)
     # plutil::timeHint(as.character(ind3));Sys.sleep(1)
      if (is.integer(ind3)) {
         selectRows(proxyCFdata,ind3)
      }
   })
   observe({
      cname <- rvSelectIndustry()
      ind2 <- match(cname,industryAbbr$industry)
      cname <- industryAbbr$abbr[ind2]
      b <- rvHumanUseCF()
      ind3 <- match(cname,rownames(b))
      if (is.integer(ind3)) {
         selectRows(proxyCFdata,ind3)
      }
   })
}
if (F) observe({
   indRow <- input$cross_rows_selected
   indCol <- input$cross_rows_selected
   indI <- input$industrydata_rows_selected
   indCF <- input$cfdata_rows_selected
   if (is.null(indRow)) {
      showNotification("wonna clear CF-row in Industry-table",duration=2)
      selectRows(proxyIndustrydata,NULL)
   }
   else {
      selectRows(proxyIndustrydata,indRow)
   }
   if (is.null(input$cross_columns_selected)) {
      showNotification("wonna clear Industry-row in CF-table",duration=2)
      selectRows(proxyCFdata,NULL)
   }
   if (!is.null(indRow)) {
      indI <- input$industrydata_rows_selected
      if (is.null(indI)) {
         if (!is.null(indC <- input$industrydata_row_last_clicked)) {
           # if (indC!=indI) {
              # showNotification("'industrydata' row is deselected",duration=2)
               ##~ if (!is.null(da <- rvHumanUseIndustry())) {
                  ##~ showNotification("'industrydata' exists",duration=2)
                  ##~ ind <- match(rvSelectCF(),da[[1]])
                  ##~ if (is.integer(na.omit(ind)))
                     ##~ selectRows(proxyIndustrydata,ind)
               ##~ }
           # }
         }
      }
   }
   else {
     # showNotification("\"crosstable\" row is not selected")
   }
   ##~ if (is.null(input$cross_columns_selected)) {
      ##~ showNotification("clear Industry-row in CF-table",duration=2)
      ##~ selectRows(proxyOnlyCF,NULL)
   ##~ }
   indRow <- input$industrydata_rows_selected
   indClick <- input$industrydata_row_last_clicked
   if ((!is.null(indRow))&&(!is.null(indClick))&&(indRow==indClick)) {
  # if (!is.null(indRow)) {
      cat("#################\n")
      str(list(row=indRow))
      da <- rvHumanUseIndustry()
      cf <- da[[1]][indRow]
      str(cf)
      b <- rvActivityStat()
      ind <- match(cf,rownames(b))
      str(ind)
      if (isFALSE(exchange$domain)) {
         selectRows(proxyCross,ind)
      }
      if (isTRUE(exchange$domain)) {
         selectRows(proxyOnlyCF,ind)
        # selectRows(proxyOnlyIndustry,3)
      }
      cat("#################\n")
   }
   else if (F) {
      showNotification("'industrydata' is deselected",duration=2)
      if (isFALSE(exchange$domain)) {
        # selectRows(proxyCross,NULL)
      }
      else if (isTRUE(exchange$domain)) {
        # selectRows(proxyOnlyCF,NULL)
      }
   }
   indCol <- input$cfdata_rows_selected
   if (!is.null(indCol)) {
      cat("%%%%%%%%%%%%%%%%%\n")
      str(list(col=indCol))
      da <- rvHumanUseCF()
     # str(da)
     # str(rownames(da))
      industry <- da[[1]][indCol]
      str(industry)
     # str(b)
     # str(colnames(b))
      if (isFALSE(exchange$domain)) {
         b <- rvActivityStat()
         ind <- match(industry,colnames(b))
         selectColumns(proxyCross,ind)
      }
      if (isTRUE(exchange$domain)) {
         ind <- match(industry,industryAbbr$industry)
         selectRows(proxyOnlyIndustry,ind)
      }
      str(ind)
      cat("%%%%%%%%%%%%%%%%%\n")
   }
   else if (F) {
      if (!is.null(input$cfdata_rows_selected)) {
         showNotification("'cfdata' is deselected",duration=2)
         if (isFALSE(exchange$domain)) {
            selectColumns(proxyCross,NULL)
         }
         else if (isTRUE(exchange$domain)) {
           # selectRows(proxyOnlyCF,NULL)
         }
      }
   }
   if (F & isTRUE(exchange$domain)) {
      selectRows(proxyCross,NULL)
      selectColumns(proxyCross,NULL)
   }
})
if (F) observeEvent(exchange$domain,{
   cat("observe exchange$domain:\n")
   if (isTRUE(exchange$domain)) {
      selectRows(proxyCross,NULL)
      selectColumns(proxyCross,NULL)
   }
   if (isFALSE(exchange$domain)) {
      selectRows(proxyOnlyCF,NULL)
      selectRows(proxyOnlyIndustry,NULL)
   }
})
if (F) observe({
   if (is.null(input$industrydata_rows_selected)) { ## deselect CF
      if (!is.null(input$industrydata_row_last_clicked)) {
        # cat("$$$$$$$$$$$$$$$\n")
        # if (isFALSE(exchange$domain))
        #    selectRows(proxyCross,NULL)
        # cat("$$$$$$$$$$$$$$$\n")
      }
   }
   if (is.null(input$cfdata_rows_selected)) { ## deselect Industry
     # if (isFALSE(exchange$domain))
     #    selectColumns(proxyCross,NULL)
   }
   if (!is.null(indRow <- input$cross_rows_selected)) {
      if (!is.null(indCol <- input$cross_columns_selected)) {
        # showNotification("select cfdata after cross row selection")
        # selectColumns(proxyIndustrydata,2)
        # selectColumns(proxyCFdata,3)
      }
   }

})
if (F) observeEvent(input$cross_columns_selected,{
   showNotification("initial selection in 'industrydata'")
   indC <- input$cross_columns_selected
   indR <- input$cfdata_rows_selected
   if (is.null(indR)) {
      showNotification("select industry 2")
      DT::selectRows(proxyCFdata,2L)
      DT::selectRows(proxyIndustrydata,4L)
   }
   indI <- input$industrydata_rows_selected
   if (is.null(indI)) {
      showNotification("select CF 3")
      DT::selectRows(proxyIndustrydata,3L)
      DT::selectRows(proxyCFdata,6L)
   }
})
# if (T & useExchange) observeEvent(exchange$CF,{
if (T & useExchange)  observe({
   cf <- exchange$CF
   cat(as.character(Sys.time()),"observe 'exchange$CF':"
      ,ifelse(is.null(cf),"NULL",cf),"\n")
   if (!is.null(cf)) {
      da <- rvActivityStat()
      if (!is.null(da)) {
         ind <- match(cf,rownames(da))
         cat("   select row in 'cross'",cf,"\n")
         selectRows(proxyCross,ind)
      }
      b <- rvHumanUseIndustry()
      if (!is.null(b)) {
         ind <- match(cf,b[[1]])
         selectRows(proxyIndustrydata,ind)
      }
   }
   else {
      cat("   clear row in 'cross' and/or 'industrydata'\n")
      if (!is.null(input$cross_rows_selected))
         selectRows(proxyCross,NULL)
      if (!is.null(input$industrydata_rows_selected))
         selectRows(proxyIndustrydata,NULL)
   }
})
if (T & useExchange)  observe({
   industry <- exchange$industry
   cat(as.character(Sys.time()),"observe 'exchange$industry':"
      ,ifelse(is.null(industry),"NULL",industryCode(industry)),"\n")
   if (!is.null(industry)) {
      da <- rvActivityStat()
      if (!is.null(da)) {
         ind <- match(industry,colnames(da))
         selectColumns(proxyCross,ind)
      }
      b <- rvHumanUseCF()
      if (!is.null(b)) {
         ind <- match(industry,b[[1]])
         selectRows(proxyCFdata,ind)
      }
   }
   else {
      if (!is.null(input$cross_columns_selected))
         selectColumns(proxyCross,NULL)
      if (!is.null(input$cfdata_rows_selected))
         selectRows(proxyCFdata,NULL)
   }
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
   regionProxy <- leafletProxy("regionLeaflet")
   ursa:::.elapsedTime("add PAs -- begin")
   regionProxy %>% leaflet::addPolygons(data=PAs
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
'rvRegionStats' <- reactive({
   cat("rvRegionStats:\n")
   rvRegionMetrics()$result
})
'rvRegionMetrics' <- reactive({
   cat("rvRegionMetrics:\n")
   result <- regionStats(aoi=rvAOI(),ctable=rvActivityStat(),isPA=input$actionEPA
                        ,raw=TRUE)
   result
})
if (T) observe({ ## update 'input$industry'
   cat("observe 'input$activity', update 'input$industry'\n")
  # print(input$activity)
  # req(!is.null(choice <- input$activity))
   choice <- input$activity
   print(choice)
   if ((length(choice)==1)&&(choice==nameAllHuman))
      choice2 <- c('Do not show'="none",'All selected activity'="all",industries)
   else {
      choice2 <- choice[choice %in% names(industries)]
      choice2 <- c('Do not show'="none",'All selected activity'="all",industries[choice2])
   }
   exchange$conflict <- NULL
   updateSelectInput(session,"industry",choices=choice2,selected=choice2[1])
})
if (T) observeEvent(input$industry,{ ## update 'input$industry'
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
               choice <- event[ind2]
            }
         }
      }
   }
   if (length(choice)) {
      updateSelectInput(session,"industry",selected=choice)
   }
})
if (T) observeEvent(input$activity,{ ## update 'input$activity'
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
   }
})
if (T) observeEvent(input$group,{ ## update 'input$group'
   cat("observeEvent 'input$group':\n")
   if (is.null(input$group))
      updateSelectInput(session,"group",selected=nameAllCF)
   else {
      if ((length(input$group)>1)&&(tail(input$group,1)==nameAllCF))
         updateSelectInput(session,"group",selected=nameAllCF)
      else {
         if (anyNA(ind <- match(input$group,tail(unname(groupList),-1)))) {
            if (length(ind2 <- which(!is.na(ind)))) {
               updateSelectInput(session,"group",selected=input$group[ind2])
            }
         }
      }
   }
})
'rvInitEPA' <- reactive({
   ((length(input$initEPA)>0)&&(input$initEPA>0))
})
##~ observe({
  ##~ # req(length(input$actionEPA))
   ##~ if ((!exchange$initEPA)&&(isTRUE(input$actionEPA)))
      ##~ exchange$initEPA <- TRUE
##~ })
if (T) observe({
# observeEvent(input$regionLeaflet_shape_click,{
# observeEvent(rvAOI(),{
  # rvAOI()
   cat(" ************* CLICKED *************\n")
   proxyRegion <- leafletProxy("regionLeaflet")
   isPAs <- rvInitEPA()
  # isPAs <- ((length(input$initEPA)>0)&&(input$initEPA>0))
   prmAOI <- as.list(args(regionAddAOI))
   prmEPA <- as.list(args(regionAddEPA))
   grAOI <- prmAOI$group
   layerAOI <- prmAOI$layerID
   grPAs <- prmEPA$group
   if (is.null(aoi <- rvAOI())) {
      cat("remove AOI layer from region map:\n")
     # showNotification("remove AOI layer from region map",duration=2)
     # if (file.exists("C:/tmp/aoi.sqlite"))
     #    file.remove("C:/tmp/aoi.sqlite")
     # proxyRegion <- removeShape(proxyRegion,grAOI)
      proxyRegion <- clearGroup(proxyRegion,grAOI)
     # proxyRegion <- removeShape(proxyRegion,layerAOI)
      proxyRegion <- removeControl(proxyRegion,layerAOI)
     # proxyRegion <- hideGroup(proxyRegion,grAOI)
     # proxyRegion <- clearShapes(proxyRegion)
      ##~ proxyRegion <- addLayersControl(proxyRegion
                           ##~ ,overlayGroups=c("Arctic SDI"
                              ##~ ,if (isPAs) grPAs)
                           ##~ ,options=layersControlOptions(collapsed=FALSE)
                           ##~ )
      proxyRegion <- clearControls(proxyRegion)
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=70,value=0),crs=4326)
     # e <- spatial_transform(e,3575)
      bbox <- unname(spatial_bbox(e))
   }
   else {
      cat("add AOI layer to region map:\n")
     # showNotification(paste("add AOI",spatial_count(aoi),"layer(s) to region map"),duration=2)
     # spatial_write(aoi,"C:/tmp/aoi.sqlite")
      proxyRegion <- removeShape(proxyRegion,layerAOI)
     # proxyRegion <- hideGroup(proxyRegion,grAOI)
      proxyRegion <- clearGroup(proxyRegion,grAOI)
     # proxyRegion <- showGroup(proxyRegion,grAOI)
      proxyRegion <- regionAddAOI(map=proxyRegion,aoi=aoi,addOverlay=FALSE)
      ##~ proxyRegion <- addLayersControl(proxyRegion
                           ##~ ,overlayGroups=c("Arctic SDI"
                                           ##~ ,grAOI
                                           ##~ ,if (isPAs) grPAs)
                           ##~ ,options=layersControlOptions(collapsed=FALSE)
                           ##~ )
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
# eventReactive(input$actionRegion,{
observeEvent(input$initEPA,{
#observeEvent(rvInitEPA(),{
# observe({
  # req(isTRUE(exchange$initEPA))
   cat("'inputactionEPA' / 'exhchange$initEPA' :\n")
   proxyRegion <- leafletProxy("regionLeaflet")
   aoi <- rvAOI()
   regionAddEPA(proxyRegion,aoi)
})
'rvMetricsMap' <- reactive({
   metricsMap()
})
observeEvent(input$actionNAC,{
   cat("initialice NACR map...\n")
   indexMap(leafletProxy("regionLeaflet"),"NAC")
})
observeEvent(input$actionNAO,{
   cat("initialice NAOR map...\n")
   indexMap(leafletProxy("regionLeaflet"),"NAO")
})
observe({
  # showNotification("update controls",duration=3)
   map <- leafletProxy("regionLeaflet")
   showAOI <- !is.null(rvAOI())
   showPAs <- isTRUE(input$initEPA>0)
   showNAO <- isTRUE(input$actionNAO>0)
   showNAC <- isTRUE(input$actionNAC>0)
   if (showAOI)
      grAOI <- as.list(args(regionAddAOI))$group
   if (showPAs)
      grPAs <- as.list(args(regionAddEPA))$group
   if (showNAO)
      grNAO <- "NAOR index"
   if (showNAC)
      grNAC <- "NACR index"
   map <- addLayersControl(map
                        ,overlayGroups=c(NULL
                                        ,"Arctic SDI"
                                        ,if (showAOI) grAOI
                                        ,if (showPAs) grPAs
                                        ,if (showNAO) grNAO
                                        ,if (showNAC) grNAC
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
})
observeEvent(input$comment,{
   config <- jsonlite::fromJSON(configFile)
   config$comment <- input$comment
   writeLines(jsonlite::toJSON(config),configFile)
})
'rvCommentTable' <- reactive({
   cat("'rvCommentTable()'\n")
   if (file.exists(commentFile)) {
      da <- jsonlite::fromJSON(commentFile)
   }
   else
      da <- NULL
   str(da)
   req(cf <- CFCode(rvSelectCF()))
   req(industry <- industryCode(rvSelectIndustry()))
   if (input$submit) {
      isolate({
         cat("submission!\n")
         req(!is.null(opinion <- input$opinion))
         da2 <- data.frame(CF=cf,Industry=industry
                          ,Time=format(Sys.time(),"%Y-%m-%d %H:%M")
                          ,Author=if (is.null(input$author)) "" else input$author
                          ,Comment=opinion
                          )
         if (nrow(da2)) {
            da <- rbind(da,da2)
            da <- unique(da)
            da <- da[nchar(da$Comment)>0,]
            if (nrow(da)>0)
               writeLines(jsonlite::toJSON(da),commentFile)
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
if (T) observeEvent(input$submit,{
   cat("observe 'input$submit'\n")
   req(!is.null(opinion <- input$opinion))
   updateTextAreaInput(session,"opinion",value="")
})
