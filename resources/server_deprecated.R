'rvSelectCF_skip' <- reactive({
   verbosing()
   cat("rvSelectCF():\n")
   if (useExchange)
      return(exchange$CF)
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
'rvSelectIndustry_skip' <- reactive({
   verbosing()
   cat("rvSelectIndustry():\n")
   if (useExchange)
      return(exchange$industry)
   if (length(ind <- input$cross_columns_selected)) {
      if (!is.null(b <- rvCrossTable())) {
         cname <- colnames(b)[ind]
         iname <- gsub("^[A-Z]([A-Z])+(:|\\s*-)*\\s","",industryName(industries))
         if (cname %in% iname) {
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
      if (!is.null(b <- rvSubsetIndustry())) {
         cname <- b[ind]
         ret <- cname
        # exchange$industry <- ret
         print("input$onlyIndustry_rows_selected")
         return(ret)
      }
      if ((FALSE)&&(!is.null(b <- rvCrossTable()))) {
        # print(colnames(b))
         cname <- colnames(b)
         cname <- cname[which(!is.na(match(cname,industryName(industries))))]
        # cname <- cname[cname %in% unlist(industries)]
         cname <- cname[ind]
        # cname <- colnames(b)[ind]
         if (cname %in% industryName(industries)) {
           # proxyCFdata %>% selectRows(NULL)
           # proxyCross %>% selectColumns(NULL)
            ret <- cname
           # exchange$industry <- ret
            print("input$onlyIndustry_rows_selected (deprecated)")
            return(ret)
         }
      }
   }
   if (T) {
      if (length(ind <- input$cfdata_rows_selected)) {
         print("input$cfdata_rows_selected")
         if (!is.null(b <- rvSubsetIndustry())) {
            cname <- b[ind]
            str(cname)
            ret <- industryCode(cname)
            str(ret)
           # exchange$industry <- ret
            return(ret)
         }
      }
   }
  # exchange$industry <- NULL
   NULL
})
if (F) { ## CFSelection
   observeEvent(input$onlyCF_row_last_clicked,{
      verbosing()
      cat("synchro CF selection -- from 'onlyCF' to 'industrydata'\n")
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
         if (FALSE) {
            da <- rvCrossTable()
            cname <- rownames(da)[ind3s]
         }
         else {
            cname <- rvGroupCF()[ind3s]
         }
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
            da <- rvCrossTable()
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
      verbosing()
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
            da <- rvCrossTable()
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
if (T) { ## industrySelection
   observeEvent(input$onlyIndustry_row_last_clicked,{
      verbosing()
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
         da <- rvCrossTable()
         cname <- colnames(da)[ind3s]
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
         }
      }
   })
   observe({
      if (isFALSE(exchange$domain)) {
         verbosing()
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
                 # str(ind2)
                 # showNotification("need to select row refferred to selected CF",duration=3)
                  DT::selectRows(proxyCFdata,ind2)
               }
            }
         }
      }
   })
   observeEvent(input$cfdata_row_last_clicked,{
      verbosing()
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
               da <- rvCrossTable()
               ind1 <- match(b[[1]][ind2s],colnames(da)) ## abbr in rownames(b)
               DT::selectColumns(proxyCross,ind1)
            }
            if (isTRUE(exchange$domain)) {
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
}
