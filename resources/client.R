'verbose' <- function() 'cat(as.character(match.call())[1],":\n")'
'selectionTables' <- function() {
   cat(as.character(match.call())[1],":\n")
   cell <- input$cross_cells_selected
   isNULL <- !sum(dim(cell))
   a <- list(NULL
           # ,sRow=if (isNULL) NULL else cell[1,1]
           # ,sColumn=if (isNULL) NULL else cell[1,2]
            ,industrydata_rows_s=input$industrydata_rows_selected
           # ,industrydata_row_last_c=input$industrydata_row_last_clicked
            ,cfdata_rows_s=input$cfdata_rows_selected
           # ,cfdata_row_last_c=input$cfdata_row_last_clicked
            ,cross_columns_s=input$cross_columns_selected
            ,cross_rows_s=input$cross_rows_selected
           # ,cross_rows_c=input$cross_row_last_clicked
           # ,cross_columns_c=input$cross_column_last_clicked ## unimplemented
            ,choosen_CF=rvSelectCF()
            ,choosen_Industry=industryCode(rvSelectIndustry())
           # ,exchange_CF=exchange$CF
           # ,exchange_industry=industryCode(exchange$industry)
            )[-1]
   str(a)
}
'selectionRegion' <- function() {
   cat(as.character(match.call())[1]
      ,as.character(packageVersion("flexdashboard"))
      ,as.character(packageVersion("ursa"))
     # ,shinybrowser::get_browser()
     # ,shinybrowser::get_width()
     # ,shinybrowser::get_height()
      ,":\n"
      )
   s2 <- rvAOI()
   if (is.ursa(s2)) {
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
   }
   else if (is.null(s2))
      ret <- "Not selected"
   else
      ret <- s2
   if (is.character(ret))
      return(invisible(NULL))
   ret
}
'cellComment' <- function() {
   cat(as.character(match.call())[1],":\n")
   req(cf <- rvSelectCF())
   req(industry <- rvSelectIndustry())
   activity <- rvIndustryGroup()
   patt <- paste0(activity,sepRules,industry)
   cmt <- comments[comments$industry %in% patt & comments$CF_code %in% cf
                  ,c("Limitations","Comment")]
   cat("----------------\n")
   print(data.frame(industry=industryCode(industry),CF=cf))
   str(cmt)
   cat("----------------\n")
   cmt
}
'buttonsTemplate' <- function() {
   cat(as.character(match.call())[1],":\n")
   if (isFALSE(exchange$domain)) {
      b1 <- navButton("Selected Area Details","#crosstable","cross",span=T)
   }
   else
      b1 <- NULL
   if (isTRUE(exchange$domain)) {
      b6 <- navButton("All Human Uses","#list","list",span=T)
      b7 <- navButton("All Conservation Features","#list","list",span=T)
   }
   else {
      b6 <- NULL
      b7 <- NULL
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
   if ((input$comment)&&(!is.null(industry))&&(!is.null(cf)))
      b8 <- navButton(paste(dQuote(paste0(industry,"/",cf)),"Comments")
                     ,"#comment","comment",span=T)
   else
      b8 <- NULL
   list('crosstale'=b1,'industry_list'=b6,'cf_list'=b7
       ,'cf_details'=b2,'cf_overview'=b3
       ,'industry_details'=b4,'industry_overview'=b5
       ,'comment'=b8
       )
}
'buttonsCF' <- function() {
   cat("   ",as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
   b$cf_details <- NULL
   b$industry_list <- NULL
   navButton(b)
}
'buttonsIndustry' <- function() {
   cat("   ",as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
   b$industry_details <- NULL
   b$cf_list <- NULL
   navButton(b)
}
'buttonsComment' <- function() {
   cat("   ",as.character(match.call())[1],":\n")
   b <- buttonsTemplate()
  # b$industry_details <- NULL
  # b$cf_details <- NULL
   b$comment <- NULL
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
         cf <- rownames(rvActivityStat())[ind]
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
  # b <- DT::datatable(b)
  # return(DT::datatable(iris))
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
                     ,backgroundColor=DT::styleEqual(c('1','2','3'
                                                      ,kwdGreen,kwdYellow,kwdRed
                                                     )
                                           ,c(clrLUT,clrLUT))
                     ,backgroundSize='95% 18pt'
                     ,backgroundRepeat='no-repeat'
                     ,backgroundPosition='center'  
                     )
   b
}
'tableIndustryData' <- function() {
   cat(as.character(match.call())[1],":\n")
   ##~ industry <- rvSelectIndustry()
   ##~ cat("--- industry ----\n")
   ##~ str(industry)
   ##~ cat("--- industry ----\n")
   ##~ req(industry)
   da <- rvHumanUseIndustry() # human_use(industry)
   str(da)
   if (!nrow(da)) {
      return(DT::datatable(da))
   }
   if (hlink)
      da$'CF Code' <- paste0("[",da$'CF Code',"](#annualCF)")
  # b <- DT::datatable(b)
  # useJS <- TRUE
  # colW <- ifelse(useJS,68,999)
   b <- DT::datatable(da
                       ,rownames=""
                       ,escape=FALSE
                       ,selection="single"
                       ,extensions="Scroller"
                       ,options=list(NULL
                                    ,ordering=FALSE
                                    ,scroller=TRUE
                                    ,scrollY="calc(100vh - 280px)"
                                    ,scrollX=TRUE
                                    ,pageLength=nrow(da)
                                    ,dom="ift"
                                    ,columnDefs=list(list(targets=2
                                       ,render = if (T) JS(
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
                     ,backgroundColor=DT::styleEqual(c('1','2','3'
                                                      ,kwdGreen,kwdYellow,kwdRed
                                                     )
                                                     ,c(clrLUT,clrLUT))
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
   b
}
'tableOnlyIndustry' <- function() {
   cat(as.character(match.call())[1],":\n")
   switchDomain(TRUE)
  # req(res <- rvActivityStat())
   res <- industryAbbr[,c(1,2)] # read.csv("requisite/industry.csv")
   b <- DT::datatable(res,rownames=FALSE,escape=!FALSE
                ##~ ,class="display overcontent"
                ,extension=c("Scroller")
                ,selection=list(mode="single",target="row")
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
   switchDomain(TRUE)
   req(res <- rvActivityStat())
  # res <- data.frame('CF code'=as.integer(rownames(res)),'CF name'=res[[1]])
   res <- data.frame('CF code'=as.integer(rownames(res)),'CF name'=res[[1]],check.names=FALSE)
   b <- DT::datatable(res,rownames=FALSE,escape=!FALSE
                ##~ ,class="display overcontent"
                ,extension=c("Scroller")
                ,selection=list(mode="single",target="row")
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
   cat(as.character(match.call())[1],":\n")
   switchDomain(FALSE)
   cat("crosstable -> datatable\n")
   res <- rvActivityStat()
   req(res)
  # rname - colnames(res)
  # selectRows(proxyOnlyIndustry,NULL)
  # selectRows(proxyOnlyCF,NULL)
   ind <- match(colnames(res),industryAbbr$industry)
   ind2 <- which(!is.na(ind))
   res$'Cover' <- as.numeric(res$'Cover')/100
   hasNAI <- length(indNAI <- grep('^NA[CO]',colnames(res)))==2
   if (hasNAI) {
      res$'NAC' <- as.numeric(res$'NAC')/100
      res$'NAO' <- as.numeric(res$'NAO')/100
      colnames(res)[indNAI] <- paste0("<abbr title='",colnames(res)[indNAI]
                                     ," index achievement'>",colnames(res)[indNAI],"</abbr>")
   }
   colnames(res)[ind2] <- paste0("<abbr title='"
                                              ,industryAbbr$industry[na.omit(ind)],"'>"
                                              ,industryAbbr$abbr[na.omit(ind)],"</abbr>")
  # res$'CF name' <- substr(res$'CF name',1,12)
  # res <- input$industry
  # req(res %in% unlist(industries))
   b <- DT::datatable(res,rownames=TRUE,escape=FALSE
                ##~ ,class="display overcontent"
                ,extension=c("Scroller","FixedColumns","FixedHeader","Responsive"
                            ,"ColReorder")
                ,selection=list(mode="single",target=c("cell","row+column")[2])
                ,options=list(NULL
                             ##~ ,columnDefs=list(list(className='dt-right',targets=seq(1,ncol(res))))
                             ,scrollY="calc(100vh - 265px)"
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
   if (T)
      b <- DT::formatStyle(b,ind2
                     ,backgroundImage=DT::styleEqual(c('1','2','3','3/1','3/2','2/1')
                           ,c(paste0("linear-gradient(to right,",clrLUT['1'],",",clrLUT['1'],")")
                             ,paste0("linear-gradient(to right,",clrLUT['2'],",",clrLUT['2'],")")
                             ,paste0("linear-gradient(to right,",clrLUT['3'],",",clrLUT['3'],")")
                             ,paste0("linear-gradient(to right,",clrLUT['3'],",",clrLUT['1'],")")
                             ,paste0("linear-gradient(to right,",clrLUT['3'],",",clrLUT['2'],")")
                             ,paste0("linear-gradient(to right,",clrLUT['2'],",",clrLUT['1'],")")
                             ))
                    # ,backgroundSize='95% 18pt'
                    # ,backgroundRepeat='no-repeat'
                    # ,backgroundPosition='center'
                     )
   b <- DT::formatPercentage(b,'Cover',1)
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
         b <- rvActivityStat()
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
   lab1 <- ""
   industry <- rvSelectIndustry()
   if (!is.null(industry)) {
      abbr <- industryCode(industry)
      lab1 <- paste(dQuote(abbr))
   }
   if (!nchar(lab1)) {
      b1 <- NULL #navButton("Selected Area Details","#crosstable","cross")
      b4 <- NULL
   }
   else {
      b1 <- navButton(paste(lab1,"Details"),"#annualIndustry","annual",span=T)
      b4 <- navButton(paste(lab1,"Overview"),"#descIndustry","desc",span=T)
   }
   lab2 <- ""
   cf <- rvSelectCF()
   if (!is.null(cf))
      lab2 <- paste(dQuote(cf))
   if (!nchar(lab2)) {
      b2 <- NULL # navButton("Selected Area Details","#crosstable","grey")
      b5 <- NULL
   }
   else {
      b2 <- navButton(paste(lab2,"Details"),"#annualCF","annual",span=T)
      b5 <- navButton(paste(lab2,"Overview"),"#descCF","desc",span=T)
   }
  # b2 <- navButton("Conservation Features","#list","list",span=T)
   if ((nchar(lab1))&&(nchar(lab2)))
      b8 <- navButton(paste(lab1,"/",lab2,"Comments")
                     ,"#comment","comment",span=T)
   else
      b8 <- NULL
   b3 <- navButton("Spatial query","#map",col="map",span=T)
   navButton(list(b3,b2,b5,b1,b4,b8),border=T)
}
'buttonsCross' <- function() {
   cat(as.character(match.call())[1],":\n")
  # cf <- rvSelectCF()
   lab1 <- ""
   lab2 <- ""
   if (F) {
      indRow <- input$cross_rows_selected
      if (length(indRow)) {
         res <- rvActivityStat() ## move outside of condition
         cf <- rownames(res)[indRow]
         lab1 <- "assigned"
        # lab1 <- paste("Details for",dQuote(paste(rownames(res)[indRow],"-",res[indRow,1])),"conservation feature")
        # lab1 <- paste(dQuote(cf))
      }
   }
   else {
      cf <- rvSelectCF()
      if (!is.null(cf))
         lab1 <- "assigned"
     #    lab1 <- paste(dQuote(cf))
   }
   if (F) {
     # res <- rvActivityStat() ## move outside of condition
      indCol <- input$cross_columns_selected
      if (length(indCol)) {
         industry <- colnames(res)[indCol]
         if (industry %in% unlist(industries)) {
            ##~ lab2 <- paste("Details for ",dQuote(cname),"industry")
            industry <- industryCode(industry)
            lab2 <- "assigned"
           # lab2 <- paste(dQuote(industry),"Details")
         }
      }
   }
   else {
       industry <- rvSelectIndustry()
       if (!is.null(industry)) {
          industry <- industryCode(industry)
      # lab2 <- paste(dQuote(industry),"Details")
          lab2 <- "assigned"
       }
   }
   b1 <- navButton("CF list","#list","list",span=T)
   b2 <- navButton("Human Activities in the Arctic","#list","list",span=T)
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
      lab3 <- "Overview for full domain"
   else {
      if (length(regname <- regionSF[[input$region]]$region[aoi$id])>1)
         lab3 <- paste(lab0,"for selected regions")
      else
         lab3 <- paste(lab0,"for",dQuote(regname),"region")
   }
   b3 <- navButton(lab3,"#descRegion","desc",span=T)
   b8 <- navButton("Spatial query","#map","map",span=T)
   if ((input$comment)&&(!is.null(industry))&&(!is.null(cf)))
      b9 <- navButton(paste(dQuote(paste0(industry,"/",cf)),"Comments")
                     ,"#comment","comment",span=T)
   else
      b9 <- NULL
   navButton(list(b8,b3,b4,b7,b5,b6,b9)) ## # ,b1,b2
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
   b3 <- navButton("Spatial query","#map",col="map",span=T)

  # HTML(paste("<p/><p>",b1,"</p>",collapse=" "))
   navButton(list(b3,b2,b1))
}
'switchCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   req(res <- rvActivityStat())
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
   b3 <- navButton("Spatial query","#map",col="map",span=T)
   navButton(list(b3,b2,b1))
}
'industryDescription' <- function(simple=FALSE) {
   cat(as.character(match.call())[1],":\n")
   req(industry <- rvSelectIndustry())
  # activity <- names(industries)[sapply(industries,function(a) input$industry %in% a)]
   activity <- rvIndustryGroup()
  # abbr <- industryCode(industry)
  # activity <- names(industries)[input$cfdata_rows_selected]
   if (F) {
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
      lut <- read.csv("./requisite/industry.csv")
      if (length(ind <- match(industry,lut$industry))) {
         if (nchar(lut$manual[ind]))
            industry <- lut$manual[ind]
      }
      a0 <- read_xml("./include/industries.html",encoding="UTF-8",as_html=TRUE)
      a1 <- as_list(a0)[[1]][[1]]
      a1 <- lapply(a1,function(a2) {
         if (!is.list(a2))
            return(NULL)
         if (is.null(a2$h2))
            return(NULL)
         h2 <- a2$h2[[1]]
         if (h2!=activity)
            return(NULL)
         c2 <- attributes(a2)
         a2 <- lapply(a2,function(a3) {
            if (!is.list(a3))
               return(NULL)
            if (is.null(a3$h3))
               return(if (simple) NULL else a3)
            h3 <- a3$h3[[1]]
            if (h3!=industry)
               return(NULL)
            a3
         })
         ind2 <- sapply(a2,function(x) !is.null(x))
         a2 <- a2[ind2]
         c2$names <- c2$names[ind2]
         attributes(a2) <- c2
         str(a2)
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
'mapViewer' <- function() {
   cat(as.character(match.call())[1],":\n")
   a <- rvActivity()$map
   req(!is.null(a))
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
  # write_envi(d6,"c:/tmp/tmp_d6")
   aoi <- rvAOI()
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

  # b <- rvActivityStat()
  # ind <- match(cf,rownames(b))
  # tbl <- b[cf,,drop=TRUE]
  # paste0(ifelse(hlink,"<a href=#section-table>",""),cf
  #       ,ifelse(hlink,"</a>","")," - ",tbl[[1]])
   paste0(abbr,": ",industry)
}
'captionCF' <- function() {
   cat(as.character(match.call())[1],":\n")
  # ind <- input$cross_rows_selected
  # req(is.integer(ind))
   req(cf <- rvSelectCF())
   ind <- match(cf,scenarioCF$CF_code)
   cfname <- scenarioCF$CF_name[ind]
  # b <- rvActivityStat()
  # ind <- match(cf,rownames(b))
  # tbl <- b[cf,,drop=TRUE]
   paste0(ifelse(hlink,"<a href=#section-table>",""),cf
         ,ifelse(hlink,"</a>","")," - ",cfname)
}
'switchDomain' <- function(action=NA) {
   cat(as.character(match.call())[1],":\n")
   if (isTRUE(action)) {
      cond <- !isTRUE(exchange$domain)
   }
   else if (isFALSE(action)) {
      cond <- !isFALSE(exchange$domain)
   }
   else {
      cond <- FALSE
   }
   if (!cond) {
      cat("   cond is:",cond,"\n")
      return(NULL)
   }
   cat("   action is:",action,"\n")
   proxyOnlyCF %>% selectRows(NULL)
   proxyCross %>% selectRows(NULL)
   proxyCross %>% selectColumns(NULL)
   proxyIndustrydata %>% selectRows(NULL)
   proxyOnlyIndustry %>% selectRows(NULL)
   proxyCFdata %>% selectRows(NULL)
   exchange$domain <- action
   showNotification(paste("domain is",exchange$domain),duration=1)
  # plutil::timeHint(paste("domain is",exchange$domain))
   NULL
}
'displayCF' <- function(cf) {
   cat(as.character(match.call())[1],":\n")
   if (F) {
     # req(input$industry %in% unlist(industries))
      ind <- input$listCF_rows_selected
      if (is.integer(ind)) {
         tbl <- rvActivityStat()[ind,,drop=FALSE]
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
   if (missing(cf))
      req(cf <- rvSelectCF())
  # req(length(input$cfcode)>0)
   ursa:::.elapsedTime("display -- start")
   str(cf)
   g1 <- session_grid()
   sp <- puvspr[puvspr$species %in% cf,]
   am <- pu[pu$ID %in% sp$pu,]
   spatial_data(am) <- sp["amount"]
   g2 <- regrid(spatial_grid(am),expand=1.5)
   session_grid(consistent_grid(g2,ref=c(600,600)))
   ret <- glance(am,fileout=ursa:::.maketmp(ext=".png"),scale=1,retina=1,border=11)
   session_grid(g1)
   ursa:::.elapsedTime("display -- finish")
   list(src=ret,width="100%",'height'="100%",'max-width'="100%"
       ,'object-fit'="scale-down")#,display="inline-block")
}
'leafletCF' <- function(cf) {
 #  showNotification("leafletCF",duration=5)
   cat(as.character(Sys.time()),as.character(match.call())[1],":\n")
  # cf <- "9038"
   if (missing(cf))
      req(cf <- rvSelectCF())
  # cf <- exchange$CF
  # ind <- input$industrydata_rows_selected
  # str(ind)
  # b <- rvHumanUseCF()
  # cf <- b[[1]][ind]
   m <- CFMap(cf)
   m
}
'leafletRegion_deprecated' <- function(aoi) {
   cat(as.character(Sys.time()),as.character(match.call())[1],":\n")
   if (missing(aoi))
      aoi <- rvAOI()
   m <- regionMap(aoi,showPAs=input$regionDesc)
   m
}
'displayIndustry' <- function() { ## renderLEaflet
   cat(as.character(match.call())[1],":\n")
   req(industry <- rvSelectIndustry())
   a <- interimMap(industry=industry)
  # saveRDS(a,"c:/tmp/industry-a.rds")
   coloring <- names(methodList[match(input$coloring,methodList)])
   d6 <- map3_1d(a,kind=coloring,source=input$sheet)
   m <- conflictMap(d6)
  # saveRDS(a,"c:/tmp/industry-m.rds")
   m
}
'metadataCF' <- function() {
   cat(as.character(match.call())[1],":\n")
   if (F) {
     # req(input$industry %in% unlist(industries))
      ind <- input$listCF_rows_selected
      if (is.integer(ind)) {
         tbl <- rvActivityStat()[ind,,drop=FALSE]
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
   md <- scenarioCF[scenarioCF$CF_code %in% cf,]
   md <- md[md$CF_code %in% cf,grep("(^$|^File_name|groupcode)"
                                   ,colnames(md),invert=TRUE)]
   if (T & hlink)
      md$CF_code <- paste0("[",md$CF_code,"](#annualCF)")
  # da <- t(as.data.frame(md,check.names=FALSE))
   da <- data.frame(t(md))
   if (F) {
      ret <- DT::datatable(da,colnames=c("","")
                          ,rownames=TRUE,selection="none",escape=FALSE
                   ,extension="Scroller"
                   ,options=list(dom="t",ordering=F
                                ,scroller=T
                               # ,scrollY="300px"
                               # ,autoWidth=FALSE
                               # ,columnDefs=list(list(width='300px',targets=c(1,2)))
                                ))
   }
   else {
      da <- cbind(left=rownames(da),da)
      colnames(da) <- c("","")
      ret <- da
   }
   ret
}
'indexMap' <- function(map,index) {
   ursa:::.elapsedTime("mapA")
   g0 <- session_grid()
   session_grid(dist2land)
   a <- spatial_centroid(rvMetricsMap()[index]) |> allocate()
   a <- c(name=mapper(a))
   print(a)
   ct <- ursa_colortable(a)
   v <- names(ct)
   v <- factor(v,levels=v,ordered=TRUE)
   b <- polygonize(a) |> spatial_transform(4326)
   session_grid(g0)
   pal <- leaflet::colorFactor(palette=as.character(ct),levels=v)
   if (T) {
      ursa:::.elapsedTime("mapB")
      d <- by(b,b$name,function(x) spatial_union(x)) #|> do.call(rbind,args=_)
      b <- sf::st_sf(name=names(d),geometry=sf::st_sfc(d,crs=spatial_crs(b)))
      rm(d)
      ursa:::.elapsedTime("mapC")
   }
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
   if (grepl("NAC",index))
      gr <- grNAC
   else if (grepl("NAO",index))
      gr <- grNAO
   else
      gr <- "undefined"
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
                   )
   map <- addLegend(map
                 ,position="topleft"
                 ,pal=pal
                 ,values=v
                 ,opacity=0.5
                 ,title=gr
                 ,group=gr
                 )
   ##~ map <- addLayersControl(map
                        ##~ ,overlayGroups=c(NULL
                                        ##~ ,"Arctic SDI"
                                        ##~ ,if (showAOI) grAOI
                                        ##~ ,if (showPAs) grPAs
                                        ##~ ,if (showNAO) grNAO
                                        ##~ ,if (showNAC) grNAC
                                        ##~ )
                        ##~ ,options=layersControlOptions(collapsed=FALSE)
                        ##~ )
   ursa:::.elapsedTime("mapD")
   map
}
