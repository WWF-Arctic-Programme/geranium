proxyCross <- DT::dataTableProxy("cross")
proxyOnlyIndustry <- DT::dataTableProxy("onlyIndustry")
proxyOnlyCF <- DT::dataTableProxy("onlyCF")
proxyIndustrydata <- DT::dataTableProxy("industrydata")
proxyCFdata <- DT::dataTableProxy("cfdata")
if (F) observeEvent(input$onlyCF_row_last_clicked,{ ## CFSelection
   sleeping()
   cat("synchro CF selection -- from 'onlyCF' to 'industrydata'\n")
  # ind3c <- input$onlyCF_row_last_clicked 
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
      cname <- rvSubsetCF()[ind3s]
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
if (F) observeEvent(input$industrydata_row_last_clicked,{
   sleeping()
  # prev <- exchange$CF
   cat(paste0(as.character(Sys.time())
             ," synchro CF selection -- from 'industrydata' to '"
             ,ifelse(!switchDomain(),"cross","onlyCF"),"'\n"))
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
         eDomain <- switchDomain()
         if (isFALSE(eDomain))
            DT::selectRows(proxyCross,NULL)
         if (isTRUE(eDomain))
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
         da <- rvCrossTable()
         ind1 <- match(cname,rownames(da)) ## abbr in rownames(b)
        # str(ind1)
        # eDomain <- switchDomain()
        # if (isFALSE(eDomain)) {
        #    DT::selectRows(proxyCross,ind1)
        # }
        # if (isTRUE(eDomain)) {
        #    DT::selectRows(proxyOnlyCF,ind1)
        # }
      }
   }
})
if (F) observeEvent(input$onlyIndustry_row_last_clicked,{ ## industrySelection
   sleeping()
   cat(as.character(Sys.time())
      ,"synchro industry selection -- from 'onlyIndustry' to 'cfdata'\n")
  # ind3c <- input$onlyIndustry_row_last_clicked
   ind3s <- input$onlyIndustry_rows_selected
   clear3 <- is.null(ind3s)
   if (clear3) {
      cat("   clear row\n")
      if (useExchange)
         exchange$industry <- NULL
      if (!useExchange) {
         DT::selectRows(proxyCFdata,NULL)
         DT::selectColumns(proxyCross,NULL)
      }
   }
   else {
      if (FALSE) {
         da <- rvCrossTable()
         cname <- colnames(da)[ind3s]
      }
      else {
         cname <- rvSubsetIndustry()[ind3s]
      }
      if (!length(cname))
         cname <- NULL
      else {
         if (useExchange)
            exchange$industry <- cname
      }
      if (!useExchange) {
         cat("   select row\n")
         ind2 <- match(cname,industryName(industries))
         DT::selectRows(proxyCFdata,ind2)
         if (!is.null(da <- rvCrossTable())) {
            if (!is.na(ind2 <- match(cname,colnames(da))))
               DT::selectColumns(proxyCross,NULL)
         }
      }
   }
})
if (F) observeEvent(input$cfdata_row_last_clicked,{
   sleeping()
   cat(paste0(as.character(Sys.time())
             ," synchro industry selection -- from 'cfdata' to '"
             ,ifelse(!switchDomain(),"cross","onlyIndustry"),"'\n"))
   ind2c <- input$cfdata_row_last_clicked
   ind2s <- input$cfdata_rows_selected
   clear2 <- is.null(ind2s)
  # showNotification(paste("observeEvent: input$table2_row_last_clicked:"
  #    ,if (clear2) "NULL" else as.character(ind2s)),duration=3)
   if (clear2) {
      if (useExchange)
         exchange$industry <- NULL
      eDomain <- switchDomain()
      if (!eDomain)
         cat("   clear column\n")
      if (eDomain)
         cat("   clear row\n")
      if (!useExchange) {
         if (!eDomain) {
            DT::selectColumns(proxyCross,NULL)
         }
         if (!eDomain) {
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
      eDomain <- switchDomain()
      if (!eDomain)
         cat("   select column\n")
      if (eDomain)
         cat("   select row\n")
      if (!useExchange) {
         if (!eDomain) {
            print("1110p")
            if (FALSE) {
               da <- rvCrossTable()
               ind1 <- match(b[[1]][ind2s],colnames(da)) ## abbr in rownames(b)
            }
            else {
               ind1 <- NULL
            }
            DT::selectColumns(proxyCross,ind1)
            print("1110q")
         }
         if (eDomain) {
           # da <- rvCrossTable()
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
if (F) observe({ ## selection from 'cross' to 'cfdata'
   if (!switchDomain()) {
      sleeping()
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
         da <- rvCrossTable()
         cname <- colnames(da)[ind1s]
         if (!length(cname))
            cname <- NULL
         else {
            cat("   select row\n")
           # updateCheckboxInput(session,"iceConcDetails",value=FALSE)
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
              # showNotification("need to select row refferred to selected CF",duration=3)
               DT::selectRows(proxyCFdata,ind2)
            }
         }
      }
   }
})
if (T) observe({ ## reset selection with change of AOI
   print("reset selection with change of AOI")
   aoi <- rvAOI()
   DT::selectColumns(proxyCross,NULL)
   DT::selectRows(proxyCross,NULL)
   DT::selectRows(proxyOnlyIndustry,NULL)
   DT::selectRows(proxyOnlyCF,NULL)
   DT::selectRows(proxyIndustrydata,NULL)
   DT::selectRows(proxyCFdata,NULL)
})
