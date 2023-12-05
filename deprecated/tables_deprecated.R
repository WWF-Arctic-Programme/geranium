if (FALSE & FALSE & FALSE) observe({ ## selectRows(proxyIndustrydata)
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
if (FALSE & FALSE & FALSE) { ## proxy select columns/rows NULL
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
if (FALSE & FALSE & FALSE) observeEvent(input$cross_columns_selected,{
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
if (FALSE & FALSE & FALSE) observe({ ## selectRows(proxyCFdata
   cname <- rvSelectIndustry()
   ind2 <- match(cname,industryAbbr$industry)
   cname <- industryAbbr$abbr[ind2]
   b <- rvHumanUseCF()
   ind3 <- match(cname,rownames(b))
   if (is.integer(ind3)) {
      selectRows(proxyCFdata,ind3)
   }
})
if (FALSE & FALSE & FALSE) observe({
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
      eDomain <- switchDomain()
      if (!eDomain) {
         selectRows(proxyCross,ind)
      }
      if (eDomain) {
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
      eDomain <- switchDomain()
      if (isFALSE(eDomain)) {
         b <- rvActivityStat()
         ind <- match(industry,colnames(b))
         selectColumns(proxyCross,ind)
      }
      if (isTRUE(eDomain)) {
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
if (FALSE & FALSE & FALSE) observeEvent(exchange$domain,{
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
if (FALSE & FALSE & FALSE) observe({
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
if (FALSE & FALSE & FALSE) observeEvent(input$cross_columns_selected,{
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
if (T & useExchange)  observe({ ## observeEvent(exchange$CF,{
   sleeping()
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
   sleeping()
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
if (FALSE & FALSE & FALSE) observe({ ## update 'input$industry'
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
if (FALSE & FALSE & FALSE) observe({ ## update 'input$cfcode'
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
