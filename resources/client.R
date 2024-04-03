'verbose' <- function() 'cat(as.character(match.call())[1],":\n")'
'verbosing' <- function() {
   if (F) {
      req(input$sleepValue)
     # if (isTRUE(input$sleepVerbose))
      if (input$sleepValue>0) {
         cat("                                                              (pausing)\n")
        # print(gc())
      }
      Sys.sleep(input$sleepValue/1000)
   }
   else if (F & staffOnly) {
      memoryUsage()
   }
   NULL
}
'selectionTables' <- function() {
   cat(as.character(match.call())[1],":\n")
   cell <- input$cross_cells_selected
   isNULL <- !sum(dim(cell))
   a <- list(NULL
           # ,sRow=if (isNULL) NULL else cell[1,1]
           # ,sColumn=if (isNULL) NULL else cell[1,2]
            ,idata_R_s=input$industrydata_rows_selected
            ,onlyI_R_s=input$onlyIndustry_rows_selected
           # ,industrydata_row_last_c=input$industrydata_row_last_clicked
            ,cfdata_R_s=input$cfdata_rows_selected
            ,onlyCF_R_s=input$onlyCF_rows_selected
           # ,cfdata_row_last_c=input$cfdata_row_last_clicked
            ,cross_C_s=input$cross_columns_selected
            ,cross_R_s=input$cross_rows_selected
           # ,cross_rows_c=input$cross_row_last_clicked
           # ,cross_columns_c=input$cross_column_last_clicked ## unimplemented
            ,choosen_CF=rvSelectCF()
            ,choosen_I=rvSelectIndustry()
            ,subsetCF=length(rvSubsetCF())
            ,subsetI=length(rvSubsetIndustry())
            ,domain=switchDomain()
            ,eDomain=exchange$domain
           # ,exchange_CF=exchange$CF
           # ,exchange_industry=industryCode(exchange$industry)
            )[-1]
   str(a)
}
'selectionRegion' <- function() {
   cat(as.character(match.call())[1]
      ,as.character(packageVersion("flexdashboard"))
      ,as.character(packageVersion("DT"))
      ,as.character(packageVersion("ursa"))
     # ,shinybrowser::get_browser()
     # ,shinybrowser::get_width()
     # ,shinybrowser::get_height()
      ,":\n"
      )
   s2 <- rvAOI()
   if (is.null(s2))
      ret <- "Not selected"
   else if (is.ursa(s2)) {
      ##~ str(names(s2))
      ##~ id <- as.integer(names(s2))
      ##~ names(s2) <- paste(ifelse(all(id %in% seq(83)),"PAC","AOI"),names(s2))
      ret <- as.table(s2)
      id <- as.integer(names(ret))
      names(ret) <- paste(ifelse(all(id %in% seq(83)),"PAC","AOI"),names(ret))
   }
   else if (is_spatial(s2)) {
      ret <- spatial_data(s2)
      ret$area <- spatial_area(s2)*1e-6
     # ret$crs <- sf::st_crs(s2)$Name
   }
   else
      ret <- s2
   print(ret)
   str(exchange$prev)
   str(exchange$curr)
   str(spatial_data(exchange$selection))
   if (is.character(ret))
      return(invisible(NULL))
   ret
}
'cellComment' <- function() {
   cat(as.character(match.call())[1],":\n")
   req(cf <- rvSelectCF())
   req(industry <- rvSelectIndustry())
   print(data.frame(cf=cf,industry=industry))
   if ("comment" %in% colnames(concern)) {  ## via 'concern'
      pair <- concern[concern$CF_code %in% cf & concern$industry %in% industry,]
      pair$comment  <- gsub("^Relative intersection area is ","Coverage ",pair$comment)
      if (!isShiny) {
         ind <- nchar(pair$comment)>44
         pair$comment[ind] <- paste0(substr(pair$comment[ind],1,44),"...")
      }
      cmt <- by(pair,list(value=pair$value,comment=pair$comment),\(x) {
         ret <- data.frame(value=x$value[1]
                          ,Months=bymonth(x$month,name=TRUE)
                          ,Comment=x$comment[1]
                          )[,-1]
         ret
      })
      req(length(cmt)>0)
      cmt <- do.call(rbind,cmt)
   }
   else { ## deprecated; via 'vulner/comments'
      activity <- rvIndustryGroup()
      patt <- paste0(activity,sepRules,industry)
      cmt <- comments[comments$industry %in% patt & comments$CF_code %in% cf
                     ,c("Limitations","Comment")]
      rownames(cmt) <- NULL
      colnames(cmt)[grep("Limit",colnames(cmt))] <- "Months"
      if (!isShiny)
         cmt$Comment <- paste0(substr(cmt$Comment,1,44),"...")
   }
   cmt
}
'buttonsTemplate' <- function() {
   cat("   ",as.character(match.call())[1],":\n")
   eDomain <- switchDomain()
   if (isFALSE(eDomain)) {
     # b1 <- navButton("Selected Area Details","#crosstable","cross",span=T)
      b1 <- navButton("Table","#crosstable","cross",span=T)
   }
   else
      b1 <- NULL
   if (isTRUE(eDomain)) {
      b6 <- navButton("All Insustrial Activities","#list","list",span=T)
      b7 <- navButton("All Conservation Features","#list","list",span=T)
      b9 <- NULL
   }
   else {
      b6 <- NULL
      b7 <- NULL
      b9 <- NULL # navButton("Lists...","#list","list",span=T)
   }
   ref2 <- "#annualIndustry"
   cf <- rvSelectCF()
   industry <- rvSelectIndustry()
   if (!is.null(cf)) {
      b2 <- navButton(paste(dQuote(cf),"Details"),"#annualCF","annual",span=T)
      b3 <- navButton(paste(dQuote(cf),"Overview"),"#descCF","desc",span=T)
   }
   else {
      b2 <- NULL
      b3 <- NULL
   }
   if (!is.null(industry)) {
      industry <- industryCode(industry)
      b4 <- navButton(paste(dQuote(industry),"Details"),"#annualIndustry","annual",span=T)
      b5 <- navButton(paste(dQuote(industry),"Overview"),"#descIndustry","desc",span=T)
   }
   else {
      b4 <- NULL
      b5 <- NULL
   }
   if ((rvIsComment())&&(!is.null(industry))&&(!is.null(cf)))
      b8 <- navButton(paste(paste0(industry,"/",cf),"Discussion")
                     ,"#comment","comment",span=T)
   else
      b8 <- NULL
   if (rvReviewComments()) {
      b1 <- b9 <- b6 <- b7 <- b2 <- b4 <- NULL
      b10 <- navButton("Admin","#admin",span=T)
   }
   else
      b10 <- NULL
   b11 <- navButton("Bibliography","#bibliography","refs",span=T)
   list('crosstale'=b1,'general_list'=b9,'industry_list'=b6,'cf_list'=b7
       ,'cf_details'=b2,'cf_overview'=b3
       ,'industry_details'=b4,'industry_overview'=b5
       ,'comment'=b8,'admin'=b10,'biblio'=b11
       )
}
'buttonsCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
   b$cf_details <- NULL
   b$biblio <- NULL
  # b$industry_list <- NULL
   navButton(b)
}
'buttonsIndustry' <- function() {
   cat(as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
   b$industry_details <- NULL
   b$biblio <- NULL
  # b$cf_list <- NULL
   navButton(b)
}
'buttonsComment' <- function() {
   cat(as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
  # b$industry_details <- NULL
  # b$cf_details <- NULL
   b$comment <- NULL
   b$biblio <- NULL
   navButton(b)
}
'tableCFdata' <- function() {
   cat(as.character(match.call())[1],":\n")
   if (F) {
      if (F) {
         cf <- gsub("^(\\d{4}).*","\\1",input$cfcode)
      }
      else {
         req(ind <- input$cross_rows_selected)
         cf <- rownames(rvCrossTable())[ind]
      }
   }
   ##~ cf <- rvSelectCF()
   ##~ req(cf %in% spec$cf)
   ##~ da <- human_use(cf)
   da <- rvHumanUseCF()
   if (hlink) {
     # da$Industry <- paste0("[",da$Industry,"](#annualIndustry)")
      da$Industry <- paste0("<a href=#section-annualIndustry data-toggle=\"tab\">",da$Industry,"</a>")
   }
   if (length(ind <- grep("industry",colnames(da),ignore.case=TRUE))>0)
      colnames(da)[ind] <- "Activity"
  # b <- DT::datatable(b)
  # return(DT::datatable(iris))
  # saveRDS(da,"C:/tmp/interim.rds")
   m <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
   m <- m[m %in% colnames(da)]
   da0 <- unname(unlist(da[,m]))
   patt <- "(\\S+)<sup>(\\S+)</sup>"
   da1 <- gsub(patt,"\\1",da0)
   da2 <- gsub(patt,"\\2",da0)
   da3 <- paste0("<span class='hidden'>",da2,"</span>")
   # da0 <- gsub("<(/)*sup>","",da0)
   da0 <- paste0(da1,da3)
   da[,m] <-  da0
   ct <- c(clrLUT,'n/a'="grey90")
   ct <- data.frame(value=da0,color=ct[da2]) |> unique()
   ct <- DT::styleEqual(ct$value,ct$color)
   ct <- gsub("&gt;",">",ct)
   ct <- gsub("&lt;","<",ct)
   b <- DT::datatable(da,rownames=TRUE,escape=FALSE,selection="single"
                       ,extensions="Scroller"
                       ,options=list(NULL
                                    ,ordering=F
                                    ,scroller=T
                                    ,scrollY="calc(100vh - 280px)"
                                    ,pageLength=nrow(da)
                                    ,dom="ift"
                                    )
                       )
   b <- DT::formatStyle(b,colnames(da)
                     ,backgroundColor=ct
                     ,backgroundSize='95% 18pt'
                     ,backgroundRepeat='no-repeat'
                     ,backgroundPosition='center'  
                     )
  # if ((T)&&(!is.null(iname <- rvSelectIndustry()))) {
  #    print(iname)
     # if (!is.na(ind <- match(industryCode(iname),rownames(da)))) {
     #    DT::selectRows(proxyCFdata,ind)
     # }
  # }
   b
}
'tableIndustryData' <- function() {
   cat(as.character(match.call())[1],":\n")
   ##~ industry <- rvSelectIndustry()
   ##~ cat("--- industry ----\n")
   ##~ str(industry)
   ##~ cat("--- industry ----\n")
   ##~ req(industry)
  # return(DT::datatable(mtcars))
   da <- rvHumanUseIndustry() # human_use(industry)
   if (!nrow(da)) {
      return(DT::datatable(da))
   }
   if (hlink)
      da$'CF Code' <- paste0("[",da$'CF Code',"](#annualCF)")
  # b <- DT::datatable(b)
  # useJS <- TRUE
  # colW <- ifelse(useJS,68,999)
   m <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
   m <- m[m %in% colnames(da)]
   da0 <- unname(unlist(da[,m]))
   patt <- "(\\S+)<sup>(\\S+)</sup>"
   da1 <- gsub(patt,"\\1",da0)
   da2 <- gsub(patt,"\\2",da0)
   da3 <- paste0("<span class='hidden'>",da2,"</span>")
   # da0 <- gsub("<(/)*sup>","",da0)
   da0 <- paste0(da1,da3)
   da[,m] <-  da0
   ct <- c(clrLUT,'n/a'="grey90")
   ct <- data.frame(value=da0,color=ct[da2]) |> unique()
   ct <- DT::styleEqual(ct$value,ct$color)
   ct <- gsub("&gt;",">",ct)
   ct <- gsub("&lt;","<",ct)
   cname <- gsub("<(/)*em>","",da[[2]])
   if (length(ind <- which(nchar(da[[2]])>69))) {
      da[[2]][ind] <- paste0("<span title=\"",cname[ind],"\">",substr(da[[2]][ind],1,67),"...</span>")
   }
   b <- DT::datatable(da
                       ,rownames=""
                       ,escape=FALSE
                       ,selection="single"
                       ,extensions=c("Scroller","FixedColumns","ColReorder")
                       ,options=list(NULL
                                    ,ordering=FALSE
                                    ,scroller=TRUE
                                    ,scrollY="calc(100vh - 280px)"
                                    ,scrollX=TRUE
                                    ,fixedColumns=if (F) F else list(leftColumns=2)
                                    ,pageLength=nrow(da)
                                    ,dom="ift"
                                    ,columnDefs=list(list(targets=2
                                       ,render = if (F) JS(
                                        "function(data, type, row, meta) {",
                                        "return type === 'display' && data.length > 69 ?",
                                        "'<span title=\"' + data + '\">' + data.substr(0, 69) + '...</span>' : data;",
                                        "}") else NULL
                                      # ,width='50px'
                                       ))
                                   # ,autoWidth=!FALSE
                                    )
                       )
   b <- DT::formatStyle(b,colnames(da)
                     ,backgroundColor=ct
                     ,backgroundSize='95% 18pt'
                     ,backgroundRepeat='no-repeat'
                     ,backgroundPosition='center'  
                     )
   if (F)
      b <- formatStyle(b,'CF Name'
                     # ,width="150px" 
                      ,whiteSpace="nowrap"
                      ,overflow="hidden"
                      ,textOverflow="ellipsis"
        )
  # if ((T)&&(!is.null(cf <- rvSelectCF()))) {
  #    print(cf)
     # if (!is.na(ind <- match(cf,da[[1]]))) {
     #    DT::selectRows(proxyIndustrydata,ind)
     # }
  # }
   b
}
'tableOnlyIndustry' <- function() {
   cat(as.character(match.call())[1],":\n")
  # switchDomain(TRUE)
  # req(res <- rvCrossTable())
  # listI <- rvSubsetIndustry()
   res <- industryAbbr[,c(3,1,2)] # read.csv("requisite/industry_conditions.csv")
   res <- res[res$abbr %in% industryCode(rvSubsetIndustry()),]
   b <- DT::datatable(res,rownames=FALSE,escape=!FALSE
                ,colnames=c("","Activity","Industry")
                ##~ ,class="display overcontent"
                ,extension=c("Scroller")
                ,selection=list(mode=c("multiple","single")[1],target="row")
                ,options=list(NULL
                             ##~ ,columnDefs=list(list(className='dt-right',targets=seq(1,ncol(res))))
                             ,scrollY="calc(100vh - 265px)"
                            # ,fixedColumns=if (F) F else list(leftColumns=2)
                             ,ordering=T
                             ,scroller=T
                             ,pageLength=nrow(res)
                            # ,fixedHeader=F
                            # ,responsive=F
                             ,dom="ift" ## pl
                             )
                )
   b
}
'tableOnlyCF' <- function() {
   cat(as.character(match.call())[1],":\n")
  # switchDomain(TRUE)
   if (F) { # (!is.null(rvAOI())) {
      req(res <- rvCrossTable())
     # res <- data.frame('CF code'=as.integer(rownames(res)),'CF name'=res[[1]])
      res <- data.frame('CF code'=as.integer(rownames(res)),'CF name'=res[[1]],check.names=FALSE)
   }
   else {
      listCF <- rvSubsetCF()
      res <- data.frame('CF code'=listCF,'CF name'=CFName(listCF),check.names=FALSE)
   }
   b <- DT::datatable(res,rownames=FALSE,escape=FALSE
                ##~ ,class="display overcontent"
                ,extension=c("Scroller")
                ,selection=list(mode=c("multiple","single")[1],target="row")
                ,options=list(NULL
                             ##~ ,columnDefs=list(list(className='dt-right',targets=seq(1,ncol(res))))
                             ,scrollY="calc(100vh - 265px)"
                            # ,fixedColumns=if (F) F else list(leftColumns=2)
                             ,ordering=T
                             ,scroller=T
                             ,pageLength=nrow(res)
                            # ,fixedHeader=F
                            # ,responsive=F
                             ,dom="ift" ## pl
                             )
                )
   b
}
'tableCross' <- function() {
   cat(as.character(match.call())[1],": (crosstable -> datatable)\n")
  # switchDomain(FALSE)
   res <- rvCrossTable()
   if (!prod(dim(res))) {
      res <- data.frame('Missed CFs'="No data available in table",check.names=FALSE)
      b <- DT::datatable(res,selection="none",rownames=FALSE,options=list(dom="t"))
      return(b)
   }
   if (all(res$'CAP' %in% c("n/a","N/A"))) {
      actionPriority <- regionActivityIndices(aoi=rvAOI()
                         ,ctable=res # res[,-grep("CAPR",colnames(res))]
                         ,epoch=input$epoch)
      metrics <- 100*actionPriority$CAA$capCF/actionPriority$CAB$capCF
      metrics[is.na(metrics)] <- 0
      ind <- match(rownames(res),names(metrics))
      res$'CAP' <- NA_real_
      res$'CAP'[!is.na(ind)] <- metrics[na.omit(ind)]
   }
   lab <- rvRegionSF()[[input$region]]$region[rvAOI()$id]
   isChicory <- ((length(lab)==1)&&(grepl("^PAC",lab)))
  # rname - colnames(res)
   ind <- match(colnames(res),industryAbbr$industry)
   ind2 <- which(!is.na(ind))
  # res <- cbind('CF Code'=rownames(res),res)
   res$'Cover' <- as.numeric(res$'Cover')/100
   hasNAI <- length(indNAI <- grep('^NA[CO]',colnames(res)))==2
   if (hasNAI) {
      res$'NAC' <- as.numeric(res$'NAC')/100
      res$'NAO' <- as.numeric(res$'NAO')/100
      indNAC <- grep("^NAC",colnames(res))
      indNAO <- grep("^NAO",colnames(res))
      indCAPR <- grep("^CAP",colnames(res))
      if (length(unique(res[[indCAPR]]))>1)
      res[[indCAPR]] <- round(res[[indCAPR]],1)
      if (T) {
         lut <- c('ROC-CF'="Relative Overall Concern Level for a given Conservation Feature" ## 'MNS/B'
                 ,'RSC-CF'="Relative Significant Concern Level for a given Conservation Feature" ## 'S/B'
                 ,'ROIP-CF'="Relative Overall Industrial Pressure for a given Conservation Feature" ## 'CAPR'
                 )
         lname <- names(lut)
         cname <- sapply(seq_along(lname)
            ,\(i) as.character(actionLink(inputId=paste0("click",lname[i])
                              ,label=lname[i]
                              ,onClick=sprintf("Shiny.setInputValue(id='thClick%s',value=%d,{priority: \"event\"});"
                                              ,lname[i],sample(seq(1000,9999),1))
                              )))
         cname <- sapply(seq_along(lut)
                        ,\(i) paste0("<em><abbr title='",lut[i],"'>",cname[i],"</abbr></em>"))
         colnames(res)[c(indNAC,indNAO,indCAPR)] <- cname
      }
      else {
         colnames(res)[indNAC] <- "<em><abbr title=\"Minor Notable Significant for CF index achievement\">ROP</abbr></em>" ## 'MNS/B'
         colnames(res)[indNAO] <- "<em><abbr title=\"Significant for CF index achievement\">RSC</abbr></em>" ## 'S/B'
         colnames(res)[indCAPR] <- "<em><abbr title=\"Conservation Action Priority\">OIP</abbr></em>" ## 'CAPR'
        # colnames(res)[indNAI] <- paste0("<abbr title='",colnames(res)[indNAI]
        #                                ," index achievement'>",colnames(res)[indNAI],"</abbr>")
     }
   }
   indCover <- grep("^Cover",colnames(res))
   colnames(res)[indCover] <- "<em>Coverage</em>"
   colnames(res)[ind2] <- paste0("<abbr title='"
                                              ,industryAbbr$industry[na.omit(ind)],"'>"
                                              ,industryAbbr$abbr[na.omit(ind)],"</abbr>")
  # res$'CF name' <- substr(res$'CF name',1,12)
  # res <- input$industry
  # req(res %in% unlist(industries))
  # rownames(res) <- paste0("<a href=\"ya.ru\" target=\"_blank\"",rownames(res),"</a>")
   cname <- gsub("<(/)*em>","",res[[1]])
   if (length(ind <- which(nchar(res[[1]])>69))) {
      res[[1]][ind] <- paste0("<span title=\"",cname[ind],"\">",substr(res[[1]][ind],1,67),"...</span>")
   }
   if (isChicory) {
      res[[1]] <- paste0("<a href=\"","https://wwf-arctic-programme.github.io/chicory/cf/"
                        ,paste0("s",sapply(paste0("cf",rownames(res)),digest::digest,"crc32"),".html")
                        ,"\" target=\"_blank\">",res[[1]],"</a>")
   }
  # colnames(res)[1] <- ""
   b <- DT::datatable(res,rownames=TRUE,escape=FALSE
                ##~ ,class="display overcontent"
                ,extension=c("Scroller","FixedColumns","FixedHeader","Responsive"
                            ,"ColReorder")
                ,selection=list(mode="single",target=c("cell","row+column")[2])
               ## check option 'selectable' 
                ,options=list(NULL
                             ##~ ,columnDefs=list(list(className='dt-right',targets=seq(1,ncol(res))))
                             ,scrollY="calc(100vh - 280px)"
                             ,scrollX=T
                             ,fixedColumns=if (F) F else list(leftColumns=2)
                             ,ordering=T
                             ,scroller=T
                             ,pageLength=nrow(res)
                             ,fixedHeader=F
                             ,responsive=F
                             ,colReorder=T
                             ##~ ,columnDefs=list(list(responsivePriority=1,targets=0)
                                             ##~ ,list(responsivePriority=2,targets=3)
                                             ##~ ,list(responsivePriority=3,targets=7)
                                             ##~ ,list(responsivePriority=4,targets=6)
                                             ##~ ,list(responsivePriority=5,targets=4)
                                             ##~ ,list(responsivePriority=6,targets=5)
                                             ##~ ,list(responsivePriority=7,targets=2)
                                             ##~ )
                             ,dom="ift" ## pl
                             ##~ ,columnDefs=list(list(targets=1,render=JS(tooltip(9))))
                              ##~ ,columnDefs=list(list(
                                ##~ targets = 1,
                                ##~ render = JS(
                                  ##~ "function(data, type, row, meta) {",
                                  ##~ "return type === 'display' && data.length > 9 ?",
                                  ##~ "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
                                  ##~ "}")))
                             )
                )
  # b <- DT::formatStyle(b,ind2
  #                     ,'background-image'="linear-gradient(to right,red,blue)")
   if (T | !staffOnly) {
      grG <- trafficValue('1')
      grY <- trafficValue('2')
      grR <- trafficValue('3')
      grRG <- paste0(grR,"/",grG)
      grRY <- paste0(grR,"/",grY)
      grYG <- paste0(grY,"/",grG)
      grRYG <- paste0(grR,"/",grY,"/",grG)
      b <- DT::formatStyle(b,ind2
                     ,backgroundImage=DT::styleEqual(c(grG,grY,grR,grRG,grRY,grYG,grRYG)
                           ,c(paste0("linear-gradient(to right,",clrLUT[grG],",",clrLUT[grG],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grY],",",clrLUT[grY],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grR],",",clrLUT[grR],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grR],",",clrLUT[grG],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grR],",",clrLUT[grY],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grY],",",clrLUT[grG],")")
                             ,paste0("linear-gradient(to right,",clrLUT[grR],",",clrLUT[grY],",",clrLUT[grG],")")
                             ))
                    # ,backgroundSize='95% 18pt'
                    # ,backgroundRepeat='no-repeat'
                    # ,backgroundPosition='center'
                     )
   }
   b <- DT::formatPercentage(b,indCover,1)
   if (hasNAI)
      b <- DT::formatPercentage(b,indNAI,1)
   b
}
'buttonsIndustry_deprecated' <- function() {
   cat(as.character(match.call())[1],":\n")
   lab1 <- ""
   lab2 <- ""
   ref1 <- "#annualCF"
  # ref2 <- if (is.null(rvAOI())) "#list" else "#crosstable"
  # if (length(input$cross_columns_selected)>0) { ## !exchange$domain
   if (isFALSE(exchange$domain)) {
      ref2 <- "#crosstable"
      col2 <- "cross"
   }
   else {
      ref2 <- "#list"
      col2 <- "list"
   }
   cf <- rvSelectCF()
   if (!is.null(cf))
      lab1 <- paste(dQuote(cf))
   industry <- rvSelectIndustry()
   if (!is.null(industry)) {
      industry <- industryCode(industry)
      lab2 <- paste(dQuote(industry))
      if (F & !exchange$domain) {
         b <- rvCrossTable()
         bname <- colnames(b)
         bname <- industryCode(bname)
        # plutil::timeHint(as.character(match(industry,bname)));Sys.sleep(1)
      }
   }
  # plutil::timeHint(industry);Sys.sleep(1)
   if (!nchar(lab1))
      b1 <- NULL # if (isTRUE(exchange$domain)) navButton("CF Details","#list","list",span=T) else NULL
   else
      b1 <- navButton(lab1,ref1,"annual",span=T)
   if (!nchar(lab2)) {
      b2 <- NULL # navButton("Industry Details",ref2,"grey",span=T)
   }
   else {
      b2 <- navButton(lab2,ref2,col2,span=T)
   }
  # HTML(paste("<p>",b1,b2,"</p>",collapse=" "))
   navButton(list(b1,b2))
}
'buttonsList' <- function() {
   cat(as.character(match.call())[1],":\n")
   industry <- rvSelectIndustry()
   cf <- rvSelectCF()
   isMultiple <- (length(cf)>1)||(length(industry)>1)
   isNone <- ((!length(cf))&&(!length(industry)))
   lab1 <- ""
   if (!is.null(industry)) {
      abbr <- industryCode(industry)
      lab1 <- paste(dQuote(abbr))
   }
   if ((isMultiple)||(!nchar(lab1))) {
      b1 <- NULL #navButton("Selected Area Details","#crosstable","cross")
      b4 <- NULL
   }
   else {
      b1 <- navButton(paste(lab1,"Details"),"#annualIndustry","annual",span=T)
      b4 <- navButton(paste(lab1,"Overview"),"#descIndustry","desc",span=T)
   }
   lab2 <- ""
   if (!is.null(cf))
      lab2 <- paste(dQuote(cf))
   if ((isMultiple)||(!nchar(lab2))) {
      b2 <- NULL # navButton("Selected Area Details","#crosstable","grey")
      b5 <- NULL
   }
   else {
      b2 <- navButton(paste(lab2,"Details"),"#annualCF","annual",span=T)
      b5 <- navButton(paste(lab2,"Overview"),"#descCF","desc",span=T)
   }
   if (isNone) {
      b9 <- navButton("Selected Area Details","#crosstable","cross",span=T)
      b7 <- navButton("Overview for ArcNet domain","#descRegion","desc",span=T)
   }
   else if (isMultiple) {
      b9 <- navButton("Selected Area Details","#crosstable","grey",span=T)
      b7 <- navButton("Overview for ArcNet domain","#descRegion","grey",span=T)
   }
   else {
     # b9 <- NULL
     # b7 <- NULL
      b9 <- navButton("Selected Area Details","#crosstable","cross",span=T)
      b7 <- navButton("Overview for ArcNet domain","#descRegion","desc",span=T)
   }
  # exchange$domain <- !is.null(b9)
  # b2 <- navButton("Conservation Features","#list","list",span=T)
   if ((!isMultiple)&&(nchar(lab1))&&(nchar(lab2)))
      b8 <- navButton(paste(gsub("\"","",paste0(lab1,"/",lab2)),"Discussion")
                     ,"#comment","comment",span=T)
   else
      b8 <- NULL
   b3 <- navButton("Home","#map",col="map",span=T)
   if (rvReviewComments()) {
      b3 <- b9 <- b7 <- b2 <- b1 <- b8 <- NULL
      b10 <- navButton("Admin","#admin",span=T)
   }
   else
      b10 <- NULL
   navButton(list(b3,b9,b7,b2,b5,b1,b4,b8,b10),border=T)
}
'buttonsCross' <- function() {
   cat(as.character(match.call())[1],":\n")
  # cf <- rvSelectCF()
   lab1 <- ""
   lab2 <- ""
   if (F) {
      indRow <- input$cross_rows_selected
      if (length(indRow)) {
         res <- rvCrossTable() ## move outside of condition
         cf <- rownames(res)[indRow]
         lab1 <- "assigned"
        # lab1 <- paste("Details for",dQuote(paste(rownames(res)[indRow],"-",res[indRow,1])),"conservation feature")
        # lab1 <- paste(dQuote(cf))
      }
   }
   else {
      cf <- rvSelectCF()
      if ((!is.null(cf))&&(length(cf)==1))
         lab1 <- "assigned"
     #    lab1 <- paste(dQuote(cf))
   }
   if (F) {
     # res <- rvCrossTable() ## move outside of condition
      indCol <- input$cross_columns_selected
      if (length(indCol)) {
         industry <- colnames(res)[indCol]
         if (industry %in% industryName(industries)) {
            ##~ lab2 <- paste("Details for ",dQuote(cname),"industry")
            industry <- industryCode(industry)
            lab2 <- "assigned"
           # lab2 <- paste(dQuote(industry),"Details")
         }
      }
   }
   else {
       industry <- rvSelectIndustry()
       if ((!is.null(industry))&&(length(industry)==1)) {
          industry <- industryCode(industry)
      # lab2 <- paste(dQuote(industry),"Details")
          lab2 <- "assigned"
       }
   }
   if (nchar(lab1)) {
      b4 <- navButton(paste(dQuote(cf),"Details"),"#annualCF","annual",span=T)
      b7 <- navButton(paste(dQuote(cf),"Overvew"),"#descCF","desc",span=T)
   }
   else {
      b4 <- NULL
      b7 <- NULL
   }
   if (nchar(lab2)) {
      b5 <- navButton(paste(dQuote(industry),"Details"),"#annualIndustry","annual",span=T)
      b6 <- navButton(paste(dQuote(industry),"Overview"),"#descIndustry","desc",span=T)
   }
   else {
      b5 <- NULL
      b6 <- NULL
   }
   #navButton(list(b1,b2))
   lab0 <- "Overview"
   aoi <- rvAOI()
   if (is.null(aoi))
      lab3 <- "Overview for ArcNet domain"
   else {
      if (length(regname <- rvRegionSF()[[input$region]]$region[aoi$id])>1)
         lab3 <- paste(lab0,"for selected regions")
      else
         lab3 <- paste(lab0,"for",dQuote(regname),"region")
   }
   b3 <- navButton(lab3,"#descRegion","desc",span=T)
   if (isTRUE(switchDomain())) {
      b1 <- navButton("CF list","#list","list",span=T)
      b2 <- navButton("Human Activities in the Arctic","#list","list",span=T)
      b8 <- NULL
   }
   else {
      b1 <- NULL
      b2 <- NULL # navButton("Lists...","#list","list",span=T)
      b8 <- navButton("Home","#map","map",span=T)
   }
   if ((rvIsComment())&&(!is.null(industry))&&(!is.null(cf))&&
      (length(industry)==1)&&(length(cf)==1))
      b9 <- navButton(paste(paste0(industry,"/",cf),"Discussion")
                     ,"#comment","comment",span=T)
   else
      b9 <- NULL
   if (rvReviewComments()) {
      b8 <- b1 <- b2 <- b3 <- b4 <- b5 <- NULL
      b10 <- navButton("Admin","#admin",span=T)
   }
   else
      b10 <- NULL
   navButton(list(b8,b1,b2,b3,b4,b7,b5,b6,b9,b10)) ## # ,b1,b2
}
'switchIndustry' <- function() {
   cat(as.character(match.call())[1],":\n")
   lab1 <- ""
  # indRow <- input$onlyCF_rows_selected
  # if (length(indRow)) {
  #    lab1 <- paste("Details for",dQuote(paste(rownames(res)[indRow],"-",res[indRow,1])),"conservation feature")
  # }
   industry <- rvSelectIndustry()
   if (!is.null(industry)) {
      abbr <- industryCode(industry)
      lab1 <- paste(dQuote(abbr))
   }
   if (!nchar(lab1))
      b1 <- NULL #navButton("Selected Area Details","#crosstable","cross")
   else
      b1 <- navButton(lab1,"#annualIndustry","annual",span=T)
   b2 <- navButton("Conservation Features","#list","list",span=T)
   b3 <- navButton("Home","#map",col="map",span=T)

  # HTML(paste("<p/><p>",b1,"</p>",collapse=" "))
   navButton(list(b3,b2,b1))
}
'switchCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   req(res <- rvCrossTable())
   lab1 <- ""
  # indRow <- input$onlyCF_rows_selected
  # if (length(indRow)) {
  #    lab1 <- paste("Details for",dQuote(paste(rownames(res)[indRow],"-",res[indRow,1])),"conservation feature")
  # }
    cf <- rvSelectCF()
    if (!is.null(cf))
      lab1 <- paste(dQuote(cf))
   if (!nchar(lab1))
      b1 <- NULL
     # b1 <- navButton("Selected Area Details","#crosstable","grey")
   else
      b1 <- navButton(lab1,"#annualCF","annual",span=T)
   b2 <- navButton("Human Activities in the Arctic","#list","list",span=T)
   b3 <- navButton("Home","#map",col="map",span=T)
   navButton(list(b3,b2,b1))
}
'industryDescription' <- function(simple=FALSE) {
   cat(as.character(match.call())[1],":\n")
   req(industry <- rvSelectIndustry())
  # activity <- names(industries)[sapply(industries,function(a) input$industry %in% a)]
   activity <- rvIndustryGroup()
   print(data.frame(activity=activity,industry=industry,simple=simple))
  # str(activity)
  # str(industry)
  # abbr <- industryCode(industry)
  # activity <- names(industries)[input$cfdata_rows_selected]
   if (T) {
      fileout <- "res1.html" # "res1.html" ## tempfile()
      fname <- file.path(root,"include/industries.md")
      a <- readLines(fname)
      ind <- grep("^(##|###)\\s",a)
      md <- data.frame(ind=ind,level=0,name=a[ind],from=NA,to=NA)
      md$level[grep("^##\\s",md$name)] <- 2L
      md$level[grep("^###\\s",md$name)] <- 3L
      md$name <- gsub("^(##|###)\\s","",md$name)
      md$from <- md$ind
      md$to <- tail(c(md$ind-1L,length(a)),-1)
      ind3 <- grep(paste0("^",industry),md$name)
      if (simple)
         a <- a[seq(md$from[ind3],md$to[ind3])]
      else {
         ind2 <- grep(paste0("^",activity),md$name)
         a <- a[c(seq(md$from[ind2],md$to[ind2]),seq(md$from[ind3],md$to[ind3]))]
      }
      filebib <- file.path(root,"include/industries.bib")
      filecsl <- file.path(root,"include/industries.csl")
      a <- c("---"
            ,"output: html_fragment"
            ,paste("bibliography:",filebib)
            ,paste("csl:",filecsl)
            ,"---"
            ,""
            ,a
            ,""
            ,"#### References:")
     # print(lapply(a,substr,1,64) |> do.call(c,args=_))
      filein <- tempfile(fileext=".Rmd") # "res1.Rmd" # "res1.html" ## tempfile()
      fileout <- tempfile(fileext=".html") # "res1.html" # "res1.html" ## tempfile()
      writeLines(a,filein)
      rmarkdown::render(filein
                       ,output_format=rmarkdown::html_fragment()
                      # ,output_format=rmarkdown::html_vignette(css=NULL)
                       ,output_file=fileout,quiet=TRUE
                      # ,params=list(prm=analysis(),kind=1L)
                       )
      ret <- scan(fileout,what=character(),encoding="UTF-8",quiet=TRUE)
      ret <- HTML(ret)
      file.remove(fileout)
   }
   else if (F) {
      fname <- paste0("include/industry-",activity,".Rmd")
      print(file.exists(fname))
      print(fname)
      if (F) {
         a1 <- knitr::knit(fname,quiet=FALSE)
         a2 <- markdown::markdownToHTML(a1,fragment.only=TRUE)
         ret <- HTML(a2)
         str(ret)
      }
      else {
        # wd <- setwd(dirname(fname))
         fileout <- "res1.html" # "res1.html" ## tempfile()
         rmarkdown::render(fname
                          ,output_format=rmarkdown::html_fragment()
                         # ,output_format=rmarkdown::html_vignette(css=NULL)
                          ,output_file=fileout,quiet=TRUE
                         # ,params=list(prm=analysis(),kind=1L)
                          )
         fileout <- file.path(dirname(fname),fileout)
         a0 <- read_xml(fileout)
         a1 <- as_list(a0)
         id <- paste0("i",digest::digest(unname(industry),"crc32"))
         ind <- which(!sapply(a1[[1]],function(b1) isTRUE(id==attr(b1,"id"))))
         for (i in rev(tail(ind,-1))) {
            xml_remove(xml_child(a0,search=i))
         }
        # fileout <- gsub("res1","res2",fileout)
         write_html(a0,fileout)
         ret <- scan(fileout,what=character(),encoding="UTF-8",quiet=TRUE)
         ret <- HTML(ret)
        # ret <- paste0(ret,collapse="\n")
        # str(ret)
        # ret <- fileout
         file.remove(fileout)
      }
   }
   else {
      fileout <- "res1.html" # "res1.html" ## tempfile()
      lut <- read.csv(file.path(root,"requisite/industry_conditions.csv"))
      iname <- "foo"
      if (length(ind <- match(industry,lut$abbr))) {
         if (is.character(lut$manual)) {
            if (nchar(lut$manual[ind])) {
               iname <- lut$manual[ind]
            }
         }
      }
      opW <- options(warn=1)
      a0 <- read_xml(file.path(root,"include/industries.html"),encoding="UTF-8",as_html=TRUE)
      options(opW)
      a1 <- as_list(a0)[[1]][[1]]
      a1 <- lapply(a1,function(a2) {
         if (!is.list(a2))
            return(NULL)
         if (is.null(a2$h2))
            return(NULL)
         h2 <- a2$h2[[1]][[1]]
         if (h2!=activity)
            return(NULL)
         c2 <- attributes(a2)
         a2 <- lapply(a2,function(a3) {
            if (!is.list(a3))
               return(NULL)
            if (is.null(a3$h3))
               return(if (simple) NULL else a3)
            h3 <- a3$h3[[1]][[1]]
            if (!grepl(paste0("^",industry),h3))
               if (T | h3!=iname)
                  return(NULL)
            a3
         })
         ind2 <- sapply(a2,function(x) !is.null(x))
         a2 <- a2[ind2]
         c2$names <- c2$names[ind2]
         attributes(a2) <- c2
        # str(a2)
         a2
      })
      a1 <- a1[sapply(a1,function(x) !is.null(x))]
      if (!length(a1)) {
         ret <- HTML("")
      }
      else {
         a0 <- as_xml_document(list(a1))
         write_html(a0,fileout)
         ret <- scan(fileout,what=character(),encoding="UTF-8",quiet=TRUE)
         ret <- HTML(ret)
         file.remove(fileout)
      }
   }
   ret
}
'mapViewer_deprecated' <- function() {
   return(rvConflictMap())
   cat(as.character(match.call())[1],":\n")
   aoi <- rvAOI()
   a <- rvActivityMap()
   if (is.null(a)) {
      m <- conflictMap(aoi=aoi)
      return(m)
   }
   str(a)
   print(a)
  # a <- rvActivity()$map
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
  # write_envi(d6,"c:/tmp/tmp_d6")
  # if (!is.null(aoi))
  #    saveRDS(aoi,"c:/tmp/tmp_d6.rds")
   m <- conflictMap(d6,aoi=aoi)
   m
}
'captionIndustry' <- function() {
   cat(as.character(match.call())[1],":\n")
  # ind <- input$cross_rows_selected
  # req(is.integer(ind))
   req(industry <- rvSelectIndustry())
   if (!nchar(industry))
      return(industry)
   abbr <- industryCode(industry)

  # b <- rvCrossTable()
  # ind <- match(cf,rownames(b))
  # tbl <- b[cf,,drop=TRUE]
  # paste0(ifelse(hlink,"<a href=#section-table>",""),cf
  #       ,ifelse(hlink,"</a>","")," - ",tbl[[1]])
   paste0(industryCode(industry),": ",industryName(industry))
}
'captionCF' <- function() {
   cat(as.character(match.call())[1],":\n")
  # ind <- input$cross_rows_selected
  # req(is.integer(ind))
   req(cf <- rvSelectCF())
   ind <- match(cf,scenarioCF[[indCFcode]])
   cfname <- scenarioCF[[indCFname]][ind]
  # b <- rvCrossTable()
  # ind <- match(cf,rownames(b))
  # tbl <- b[cf,,drop=TRUE]
   paste0(ifelse(hlink,"<a href=#section-table>",""),cf
         ,ifelse(hlink,"</a>","")," - ",cfname)
}
'switchDomain' <- function(action=NA) {
   cat(as.character(match.call())[1],":\n")
   cond <- (length(rvSelectCF())>1)||(length(rvSelectIndustry())>1)
   if (cond)
      showNotification(paste("Is ArcNet domain?",cond),duration=1)
   return(cond)
   ret <- exchange$domain ## NULL
   if (isTRUE(action)) {
      cond <- !isTRUE(exchange$domain)
   }
   else if (isFALSE(action)) {
      cond <- !isFALSE(exchange$domain)
   }
   else {
      cond <- FALSE # isTRUE(exchange$domain) ## was always FALSE
   }
   if (!cond) {
      cat("   cond is:",cond,", state is:",ret,"\n")
      return(ret)
   }
   cat("   action is:",action,"\n")
   exchange$domain <- action
   showNotification(paste("ArcNet domain is",exchange$domain),duration=1)
  # plutil::timeHint(paste("domain is",exchange$domain))
   NULL
}
'displayCF' <- function(cf) {
   cat(as.character(match.call())[1],":\n")
   if (F) {
     # req(input$industry %in% unlist(industries))
      ind <- input$listCF_rows_selected
      if (is.integer(ind)) {
         tbl <- rvCrossTable()[ind,,drop=FALSE]
        # ind2 <- ursa:::.sample(grep(tbl[[1]],cfmeta$CF_code))
         ind2 <- ursa:::.sample(grep(rownames(tbl),cfmeta$CF_code))
         req(length(ind2)>0)
         str(ind2)
         cf <- cfmeta$label[ind2]
         cf <- gsub("^(\\d{4}).*","\\1",cf) # input$cfcode
      }
      else {
         req(cf <- rvSelectCF())
      }
   }
   print("1111m")
  # cf <- "5080"
   if (missing(cf)) {
      print("missing CF; call 'rvSelectCF()'")
      req(cf <- rvSelectCF())
   }
  # req(length(input$cfcode)>0)
   ursa:::.elapsedTime("display -- start")
   str(cf)
   print("1111n")
   g1 <- session_grid()
   sp <- puvspr[puvspr$species %in% cf,]
   am <- pu[pu$ID %in% sp$pu,] |> spatial_transform(as.integer(input$epsg))
   spatial_data(am) <- sp["amount"]
   g2 <- regrid(spatial_grid(am),expand=1.5)
   session_grid(consistent_grid(g2,ref=c(520,520)))
   ret <- glance(am,fileout=ursa:::.maketmp(ext=".png")
                ,blank="white",coast.fill="#00000010"
                ,scale=1,retina=1,border=11)
   session_grid(g1)
   ursa:::.elapsedTime("display -- finish")
   list(src=ret,width="100%",'height'="100%",'max-width'="100%"
       ,'object-fit'="scale-down",class="propersize")#,display="inline-block")
}
'leafletCF' <- function(cf) {
 #  showNotification("leafletCF",duration=5)
   cat(as.character(match.call())[1],":\n")
  # cf <- "9038"
   if (missing(cf))
      req(cf <- rvSelectCF())
  # cf <- exchange$CF
  # ind <- input$industrydata_rows_selected
  # str(ind)
  # b <- rvHumanUseCF()
  # cf <- b[[1]][ind]
   m <- CFMap(cf,epsg=as.integer(input$epsg))
   m
}
'leafletRegion_deprecated' <- function(aoi) {
   cat(as.character(Sys.time()),as.character(match.call())[1],":\n")
   if (missing(aoi))
      aoi <- rvAOI()
   m <- regionMap_deprecated(aoi,showPAs=input$regionDesc)
   m
}
'displayIndustryConcern' <- function() { ## renderLeaflet
   cat(as.character(match.call())[1],":\n")
   req(industry <- rvSelectIndustry())
   a <- conditionMap(industry=industry,group=NULL)
  # saveRDS(a,"c:/tmp/industry-a.rds")
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
   m <- conflictMap(d6,epsg=input$epsg)
  # saveRDS(a,"c:/tmp/industry-m.rds")
   m
}
'displayIndustryAmount' <- function() { ## renderLeaflet
   cat(as.character(match.call())[1],":\n")
   req(industry <- rvSelectIndustry())
   a <- indexHumanUse(activity=industry,epoch=input$epoch)
   d6 <- map1_1d(a) ## higher level than 'd6 <- mapper(a)'
   m <- conflictMap(d6,epsg=input$epsg)
   m
}
'metadataCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   if (F) {
     # req(input$industry %in% unlist(industries))
      ind <- input$listCF_rows_selected
      if (is.integer(ind)) {
         tbl <- rvCrossTable()[ind,,drop=FALSE]
        # ind2 <- ursa:::.sample(grep(tbl[[1]],cfmeta$CF_code))
         ind2 <- ursa:::.sample(grep(rownames(tbl),cfmeta$CF_code))
         req(length(ind2)>0)
         cf <- cfmeta$label[ind2]
         cf <- gsub("^(\\d{4}).*","\\1",cf) # input$cfcode
      }
      else {
         req(cf <- rvSelectCF())
      }
   }
   req(cf <- rvSelectCF())
   md <- scenarioCF[scenarioCF[[indCFcode]] %in% cf,]
   md <- md[md[[indCFcode]] %in% cf,grep("(^$|^File_name|groupcode)"
                                   ,colnames(md),invert=TRUE)]
   if (T & hlink)
      md[[indCFcode]] <- paste0("[",md[[indCFcode]],"](#annualCF)")
  # da <- t(as.data.frame(md,check.names=FALSE))
   da <- data.frame(t(md))
   if (FALSE) {
      rname <- rownames(da)
      ind <- grep("source",rname,ignore.case=TRUE)
      rname[ind] <- paste0("<a href=#section-bibliography>",rname[ind],"</a>")
     # rname[ind] <- navButton(rname[ind],"#bibliography")
      rownames(da) <- rname
   }
  # saveRDS(da,"C:/tmp/interim.rds")
   if (T) {
      ret <- DT::datatable(da
                          ,colnames=c("")
                          ,rownames=TRUE,selection="none",escape=FALSE
                          ,options=list(dom="t",ordering=F,pageLength=nrow(da))
                          )
   }
   else {
      da <- cbind(left=rownames(da),da)
      colnames(da) <- c("","")
      ret <- da
   }
   ret
}
'indexMap_external' <- function(map,index) { ## client.R
   showAOI <- !is.null(aoi <- rvAOI())
   showEPA <- isTRUE(input$initEPA>0)
   showNAO <- isTRUE(input$actionNAO>0)
   showNAC <- isTRUE(input$actionNAC>0)
   showCAP <- isTRUE(input$actionCAP>0)
   showHU <- isTRUE(input$actionHU>0)
   ##~ if (missing(map)) {
      ##~ if (any(showEPA,showNAO,showNAC,showCAP,showHU))
         ##~ map <- proxyRegion
   ##~ }
   ##~ if ((missing(index))&&(showAOI)) {
      ##~ print("indexMap A")
      ##~ index <- "AOI"
   ##~ }
   ##~ if (missing(index)) {
      ##~ if (missing(map)) {
         ##~ print("indexMap B -- regionMap no index")
         ##~ return(regionMap(aoi=aoi))
      ##~ }
      ##~ print("indexMap C -- map")
      ##~ return(map)
   ##~ }
   if (missing(map)) {
      print("indexMap D -- regionMap no map")
      return(regionMap(index=index,aoi=aoi
                      ,showAOI=showAOI,showEPA=showEPA,showNAO=showNAO
                      ,showNAC=showNAC,showCAP=showCAP,showHU=showHU))
   }
   print("indexMap E -- regionMap")
   ret <- regionMap(map,index,aoi=aoi
            ,showAOI=showAOI,showEPA=showEPA,showNAO=showNAO
            ,showNAC=showNAC,showCAP=showCAP,showHU=showHU)
   exchange$region <- ret
   ret
}
'regionMap_devel' <- function(map,index,aoi,showAOI=!is.null(aoi)
                       ,showEPA=FALSE,showNAO=FALSE,showNAC=FALSE
                       ,showCAP=FALSE,showHU=FALSE) { ## for process.R
   NULL
}
'regionAddAOI' <- function(map,aoi=NULL
                          ,group="Selected Region(s)",col="#092B",layerID="layerAOI"
                          ,addPolygon=TRUE,addOverlay=TRUE,addLegend=TRUE
                         # ,showEPA=FALSE
                          ) {
   grAOI <- group
   colAOI <- col
   layerAOI <- layerID
   grBasemap <- as.list(args(conflictBasemap))$group
   ursa:::.elapsedTime("0904g")
   showAOI <- (!missing(aoi))&&(is_spatial(aoi))
  # prmEPA <- as.list(args(regionAddEPA))
  # grEPA <- prmEPA$group
   clearAOI <- !showAOI
   if (missing(map)) {
      if (showAOI) {
         ursa:::.elapsedTime("0904i1")
         map <- conflictBasemap(aoi,epsg=input$epsg)
      }
      else {
         ursa:::.elapsedTime("0904i2")
         map <- conflictBasemap(epsg=input$epsg)
      }
      ursa:::.elapsedTime("0904j")
   }
   if (FALSE & showAOI) {
      ursa:::.elapsedTime("0904a")
      aoi <- aoi |> puAOI() |> spatial_union() |> spatial_transform(4326)
      ursa:::.elapsedTime("0904b")
     # aoi <- spatial_transform(spatial_union(aoi),4326)
     # aoi <- spatial_transform(aoi,4326)
   }
   if (clearAOI) {
     # map <- clearControls(map)
      map <- removeControl(map,grAOI)
      map <- removeShape(map,layerAOI)
     # map <- clearGroup(map,grAOI)
      map <- hideGroup(map,grAOI) ## remove legend
     # map <- clearShapes(map) ## bad idea
   }
   if (showAOI & addPolygon) {
      ursa:::.elapsedTime("0904c3")
      map <- removeShape(map,layerAOI)
     # map <- clearShapes(map) ## bad idea
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
                      ,group=grAOI
                      ,layerId=layerAOI
                      )
      map <- showGroup(map,grAOI)
      ursa:::.elapsedTime("0904f3")
   }
   if (showAOI & addLegend) {
      ursa:::.elapsedTime("0904k")
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colAOI
                    ,opacity=0.2
                    ,labels=grAOI
                    ,group=grAOI
                    ,layerId=layerAOI
                    )
   }
   if (F & addOverlay) {
      ursa:::.elapsedTime("0904l")
      map <- removeLayersControl(map)
      map <- addLayersControl(map
                           ,overlayGroups=c(character()
                                           ,grBasemap
                                           ,if (showAOI) grAOI
                                          # ,if (showEPA) grEPA
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
   }
   else { ## addLayersControl to keep all indexes
      ursa:::.elapsedTime("0904m1")
      if (F & showAOI)
         map <- indexMap(map,index=ifelse(showAOI,"AOI","skip"),onlyControl=TRUE)
      ursa:::.elapsedTime("0904m2")
   }
   
  # if (F & !showAOI & !showEPA)
  #    map <- removeLayersControl(map)
  ##~ # if (T | showPAs)
  ##~ #    m <- hideGroup(m,grPAs)
   ursa:::.elapsedTime("0904h")
   map
}
'indexMap' <- function(map,index,onlyControl=FALSE) { ## client.R
   if (onlyControl)
      showAOI <- (!missing(index))&&(index=="AOI")
   else
      showAOI <- !is.null(aoi <- rvAOI())
   showEPA <- isTRUE(input$initEPA>0)
   showNAO <- isTRUE(input$actionNAO>0)
   showNAC <- isTRUE(input$actionNAC>0)
   showCAP <- isTRUE(input$actionCAP>0)
   showHU <- isTRUE(input$actionHU>0)
   if (!onlyControl) {
      initMap <- missing(map)
      if ((initMap)&&(!missing(aoi))) {
        # if (missing(index))
        #    return(NULL)
         ursa:::.elapsedTime("mapG -- MISSING MAP")
         if (showAOI) {
            ursa:::.elapsedTime("0904i1")
            map <- conflictBasemap(aoi,epsg=input$epsg)
         }
         else {
            ursa:::.elapsedTime("0904i2")
            map <- conflictBasemap(epsg=input$epsg)
         }
         ursa:::.elapsedTime("0904j")
      }
      if (missing(index))
         if (showAOI)
            index <- "AOI"
      if (missing(index)) {
         ursa:::.elapsedTime("mapH")
         count <<- count+100L ## GLOBAL ASSIGNMENT
         print(c('count.indexMap'=count))
         return(map)
      }
   }
   grAOI <- "Selected Region(s)" # as.list(args(regionAddAOI))$group
   grEPA <- "Existing Protected Areas" # as.list(args(regionAddEPA))$group
   grNAO <- "SC-P" ##"SR index"
   grNAC <- "OC-P" # "MNSR index"
   grCAP <- "OIP-P" # "CAPR index"
   grHU <- "Industrial Activities"
   if (grepl("AOI",index)) {
      gr <- grAOI
      toAdd <- showAOI
   }
   else if (grepl("humanuse",index)) {
      gr <- grHU
      toAdd <- showHU
   }
   else if (grepl("CAP",index)) {
      gr <- grCAP
      toAdd <- showCAP
   }
   else if (grepl("NAC",index)) {
      gr <- grNAC
      toAdd <- showNAC
   }
   else if (grepl("NAO",index)) {
      gr <- grNAO
      toAdd <- showNAO
   }
   else if (grepl("EPA",index)) {
      gr <- grEPA
      toAdd <- showEPA
   }
   else {
      gr <- "undefined"
      toAdd <- TRUE
   }
   if (!onlyControl) {
      ursa:::.elapsedTime("mapA")
      layerId <- digest::digest(gr,"crc32")
      print("========================================================\n")
      print(data.frame(initMap=initMap
                      ,index=index
                     # ,gr=dQuote(gr)
                      ,layerId=dQuote(layerId)
                      ,AOI=showAOI,EPA=showEPA,NAC=showNAC,NAO=showNAO
                      ,add=toAdd))
      print("========================================================\n")
      if (!toAdd) {
         map <- removeControl(map,layerId)
        # map <- removeControl(map,gr)
        # map <- clearGroup(map,layerId)
         map <- clearGroup(map,gr)
         map <- removeShape(map,layerId)
      }
   }
   if (T) {
      if (onlyControl)
         ursa:::.elapsedTime("0904n1")
      map <- removeLayersControl(map)
      map <- addLayersControl(map
                           ,overlayGroups=c(NULL
                                           ,"Arctic SDI"
                                           ,if (showAOI) grAOI
                                           ,if (showEPA) grEPA
                                           ,if (showNAO) grNAO
                                           ,if (showNAC) grNAC
                                           ,if (showCAP) grCAP
                                           ,if (showHU) grHU
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
      if (onlyControl)
         ursa:::.elapsedTime("0904n2")
   }
   if (onlyControl)
      return(map)
   if (index %in% "AOI") {
      if (!toAdd) {
         e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=70,value=0),crs=4326)
        # e <- spatial_transform(e,3575)
         bbox <- unname(spatial_bbox(e))
      }
      else {
         bbox <- unname(spatial_bbox(aoi))
      }
      map <- leaflet::fitBounds(map
                        ,lng1=bbox[1],lat1=bbox[2],lng2=bbox[3],lat2=bbox[4]
                        ,options=list(minZoom=2)
                        )
   }
   if (!toAdd) {
      ursa:::.elapsedTime("mapE")
      return(map)
   }
   if (isShiny) {
      showNotification(id=layerId,closeButton=FALSE,duration=120
                      ,if (staffOnly) paste("Preparing map for",dQuote(gr)) else "Preparing map..."
                      ,type="warning")
      on.exit(removeNotification(id=layerId))
   }
   if (T & index %in% "AOI") { ## implemented to 'regionAddAOI()'
      return(map)
      colAOI <- "#092B"
     # map <- clearShapes(map) ## bad idea
      map <- leaflet::addPolygons(map,data=spatial_union(aoi)
                      ,label=gr
                      ,color=colAOI
                     # ,weight=0
                     # ,popup=~gsub(";\\s*","\n",name)
                     # ,stroke=TRUE
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                # ,sendToBack=TRUE
                                                                # ,bringToFront=TRUE
                                                                 )
                      ,group=gr
                      ,layerId=layerId
                      )
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colAOI
                    ,opacity=0.2
                    ,labels=gr
                    ,group=gr
                    ,layerId=layerId
                    )
      ursa:::.elapsedTime("mapI")
      return(map)
   }
   else if (index %in% "EPA") {
      ursa:::.elapsedTime("0914c")
     # opW <- options(warn=1) ## sf::sf_extSoftVersion() old GDAL?
      colEPA <- "#992B"
      map <- leaflet::addPolygons(map,data=spatial_transform(PAs,4326)
                      ,label=gr
                      ,color=colEPA
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                 )
                      ,group=gr
                      ,layerId=layerId
                      )
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colEPA
                    ,opacity=0.2
                    ,labels=gr
                    ,group=gr
                    ,layerId=layerId
                    )
     # options(opW)
      ursa:::.elapsedTime("0914f")
      ursa:::.elapsedTime("mapF")
      return(map)
   }
   g0 <- session_grid()
   session_grid(dist2land)
  # a <- spatial_centroid(rvMetricsMap()[index]) |> allocate()
   if (index %in% c("NAOR","NACR")) {
      rebrend <- switch(index,NAOR=c("SR","SC-P")[1],NACR=c("MNSR","OC-P")[1],index)
      a <- rvMetricsMap()[rebrend]
      rebrend <- switch(index,NAOR=c("SR","SC-P")[2],NACR=c("MNSR","OC-P")[2],index)
      names(a) <- rebrend
   }
   else if (index %in% "CAPR") {
      a <- indexCAPR(ctable=rvCrossTable(),epoch=input$epoch)
   }
   else if (index %in% "humanuse") {
      a <- indexHumanUse(ctable=rvCrossTable(),epoch=input$epoch)
   }
   a <- c(name=mapper(a))
   ct <- ursa_colortable(a)
   v <- names(ct)
   v <- factor(v,levels=v,ordered=TRUE)
   b <- polygonize(a) |> spatial_transform(4326)
   session_grid(g0)
   pal <- leaflet::colorFactor(palette=as.character(ct),levels=v)
   cat("- 1019a -----------\n")
   str(ct)
   str(v)
   cat("- 1019b -----------\n")
   if (T) {
      ursa:::.elapsedTime("mapB")
     # saveRDS(b,"C:/tmp/interim.rds")
      if (TRUE) {
         d <- by(b,b$name,function(x) spatial_union(x)) #|> do.call(rbind,args=_)
         d <- d[sapply(d,is.list)]
         b <- sf::st_sf(name=names(d),geometry=sf::st_sfc(d,crs=spatial_crs(b)))
         rm(d)
      }
      else {
         d <- aggregate(b,by=list(b$name),head,1)[,-1]
      }
     # spatial_write(b,"C:/tmp/interim.sqlite")
      ursa:::.elapsedTime("mapC")
   }
   map <- addPolygons(map,data=b
                   ,color=~pal(name)
                   ,weight=0
                  # ,popup=~gsub(";\\s*","\n",name)
                   ,label=~name
                   ,stroke=TRUE
                   ,fillOpacity=0.7
                   ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.7
                                                             # ,sendToBack=TRUE
                                                             # ,bringToFront=TRUE
                                                              )
                   ,group=gr
                  # ,layerId=layerId ## if included, only highest bar of colorbar is shown
                   )
   map <- addLegend(map
                 ,position="topleft"
                 ,pal=pal
                 ,values=v
                 ,opacity=0.5
                 ,title=gr
                 ,group=gr
                 ,layerId=layerId
                 )
   ursa:::.elapsedTime("mapD")
   if (isShiny) {
      removeNotification(id=layerId)
      if (F)
         showNotification(id=layerId,closeButton=FALSE,duration=6
                         ,"Please wait for layer rendering. It will appear soon."
                         ,type="warning")
   }
   map
}
'iceConcTable' <- function() {
   cat(as.character(match.call())[1],":\n")
   icCover <- rvIceConcCover()
   icCover$concern
}
'iceConcRule' <- function() {
   cat(as.character(match.call())[1],":\n")
   rule <- rvIceConcCover()$rule
   rule$manual <- NULL
   rule$activity <- NULL
   rule$abbr <- NULL
   rule$unique <- NULL
   if (length(ind <- grep("industry",colnames(rule),ignore.case=TRUE))>0)
      colnames(rule)[ind] <- "Activity"
   colnames(rule)[indC1 <- grep("min.*coast",colnames(rule),ignore.case=TRUE)] <- "Min coast, km"
   colnames(rule)[indC2 <- grep("max.*coast",colnames(rule),ignore.case=TRUE)] <- "Max coast, km"
   colnames(rule)[indD1 <- grep("min.*depth",colnames(rule),ignore.case=TRUE)] <- "Min depth, m"
   colnames(rule)[indD2 <- grep("max.*depth",colnames(rule),ignore.case=TRUE)] <- "Max depth, m"
   colnames(rule)[indI <- grep("ice.*free",colnames(rule),ignore.case=TRUE)] <- "Ice free"
   cell <- ursa(blank,"cellsize")*1e-3
   rule[[indC1]] <- round(rule[[indC1]]*cell)
   rule[[indC2]] <- round(rule[[indC2]]*cell)
   ice <- as.character(rule[[indI]])
   ice[!rule[[indI]] %in% c(12,99)] <- "not required"
   ice[rule[[indI]] %in% c(99)] <- "required"
   ice[rule[[indI]] %in% c(12)] <- "required whole year"
   rule[[indI]] <- ice
   rule
}
'iceConcHuman' <- function() {
   cat(as.character(match.call())[1],":\n")
   icCover <- rvIceConcCover()
  # human <- icCover$assess["industry"]
   human <- icCover$human
   if (length(ind <- grep("industry",spatial_colnames(human),ignore.case=TRUE))>0)
      spatial_colnames(human)[ind] <- "Activity"
   ret <- glance(human,fileout=ursa:::.maketmp(ext=".png")
                ,resetGrid=TRUE,retina=1,blank="white",coast.fill="#00000010"
                ,legend="bottom",las=1
               # ,unit="Static industry limitations"
                )
   list(src=ret
       ,'width'="100%"
       ,'height'="100%"
       ,'max-width'="100%"
       ,'object-fit'="scale-down"
      # ,display="inline-block"
       )
}
'iceConcMonthly' <- function() {
   cat(as.character(match.call())[1],":\n")
   g0 <- session_grid()
   ice <- rvIceConcCover()$ice
   assess <- rvIceConcCover()$assess
   if (!is.null(ice)) {
      a <- spatial_centroid(assess[,ice]) |> allocate(resetGrid=TRUE) |>
         ursa_crop(border=2)
      rm(assess)
      ursa:::.gc(TRUE)
      ret <- display_homo(a,ratio=9/16,blank="white"
                    ,fileout=ursa:::.maketmp(ext=".png"),retina=1)
      ursa:::.gc(TRUE)
   }
   else {
      session_grid(blank)
      a <- spatial_centroid(assess[,"industry"]) |> allocate() |>
         ursa_crop(border=2)
      rm(assess)
      ursa:::.gc(TRUE)
      ret <- display_homo(a,blank="white",fileout=ursa:::.maketmp(ext=".png"),retina=1)
   }
   session_grid(g0)
   list(src=ret
       ,'width'="100%"
       ,'height'="100%"
       ,'max-width'="100%"
       ,'object-fit'="scale-down"
      # ,display="inline-block"
       )
}
'iceConcCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   g0 <- session_grid()
   icCover <- rvIceConcCover()
   assess <- icCover$assess
   subject <- assess[assess$amount>0,]["amount"]
  # save(subject,pu,file="C:/tmp/interim.Rdata")
   g1 <- c(800,600)
   session_grid(g1)
   compose_open(2,fileout=ursa:::.maketmp(ext=".png"),retina=1)
   session_grid(consistent_grid(spatial_grid(subject),ref=g1,expand=1.1))
   bbox <- ursa:::spatialize(session_bbox())
   ct1 <- compose_panel(subject,blank="white",coast.fill="#00000010")
   session_grid(consistent_grid(spatial_grid(pu),ref=g1,expand=0.9))
   compose_panel(subject,col=ursa_colortable(ct1)[[1]]
                ,blank="white",coast.fill="#00000010",plot.lwd=0)
   panel_plot(bbox,lwd=1,col="transparent",border="black")
   compose_legend(ct1,units="CF relative amount")
   ret <- compose_close()
  # session_grid(icCover$gPU)
  # a <- spatial_centroid(icCover$assess[,icCover$mname]) |> allocate() |>
  #    ursa_crop(border=2)
  # ret <- display(a,layout=c(3,4),blank="white"
  #               ,fileout=ursa:::.maketmp(ext=".png"),retina=1)
   session_grid(g0)
   list(src=ret
       ,'width'="100%"
       ,'height'="100%"
       ,'max-width'="100%"
       ,'object-fit'="scale-down"
      # ,display="inline-block"
       )
}
'commentList' <- function() {
   cat(as.character(match.call())[1],":\n")
   cf <- rvSelectCF()
   industry <- rvSelectIndustry()
   str(cf)
   str(industry)
   da <- rvHumanUseCF()
   da <- da[industryCode(da[[1]]) %in% industry,]
   da <- cbind(CF=paste0("<abbr title='",CFName(cf),"'>",cf,"</abbr>")
              ,Activity=paste0("<abbr title='",industry,"'>",rownames(da),"</abbr>")
              ,da[,-1])
   if (is.null(iceCover)) {
      da2 <- sprintf("%.2f",iceConcTable()$available)
      dim(da2) <- c(1,length(da2))
      colnames(da2) <- colnames(da)[seq(3L,ncol(da))]
      da2 <- data.frame(CF="",Activity="",da2)
      da <- rbind(da,da2)
   }
   m <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
   da0 <- unname(unlist(da[,m]))
   patt <- "(\\S+)<sup>(\\S+)</sup>"
   da1 <- gsub(patt,"\\1",da0)
   da2 <- gsub(patt,"\\2",da0)
   da3 <- paste0("<span class='hidden'>",da2,"</span>")
   # da0 <- gsub("<(/)*sup>","",da0)
   da0 <- paste0(da1,da3)
   da[,m] <-  da0
   ct <- c(clrLUT,'n/a'="grey90")
   ct <- data.frame(value=da0,color=ct[da2]) |> unique()
   ct <- DT::styleEqual(ct$value,ct$color)
   ct <- gsub("&gt;",">",ct)
   ct <- gsub("&lt;","<",ct)
  # saveRDS(da,"C:/tmp/interim.rds")
   b <- DT::datatable(da,rownames=!TRUE,escape=FALSE
                     ,selection="none"
                      # ,extensions="Scroller"
                       ,options=list(NULL
                                    ,ordering=F
                                   # ,scroller=T
                                   # ,scrollY="calc(100vh - 265px)"
                                    ,pageLength=nrow(da)
                                    ,dom="t"
                                    )
                       )
   b <- DT::formatStyle(b,colnames(da)
                     ,backgroundColor=ct
                     ,backgroundSize='95% 18pt'
                     ,backgroundRepeat='no-repeat'
                     ,backgroundPosition='center'  
                     )
  # b[] <- paste0(b[],"\n","AAA")
   b
}
'onlyCommentButton' <- function() {
   cat(as.character(match.call())[1],":\n")
   r <- input$allComments_rows_selected
   if (is.null(r))
      b8 <- NULL
   else {
      da <- rvAllComments()[r,]
      industry <- da$Industry
      cf <- da$CF
      b8 <- navButton(paste(paste0(industry,"/",cf),"Discussion")
                     ,"#comment","comment",span=F)
   }
   b8
}
