wd_ <- setwd("..");on.exit(setwd(wd_))
source("resources/header.R")
ursa:::.elapsedTime("--- ready ---")
require(xml2)
require(shiny)
'industryDescription' <- function(industry="ICI",simple=FALSE) {
  # activity <- industryGroup(industry)
   activity <- lapply(industries,\(x) grep(industry,x))
   activity <- names(activity)[sapply(activity,\(x) length(x)>0)]
  # req(industry <- rvSelectIndustry())
  # activity <- names(industries)[sapply(industries,function(a) input$industry %in% a)]
  # activity <- rvIndustryGroup()
   print(data.frame(activity=activity,industry=industry,simple=simple))
  # str(activity)
  # str(industry)
  # abbr <- industryCode(industry)
  # activity <- names(industries)[input$cfdata_rows_selected]
   if (T) {
      fileout <- "res1.html" # "res1.html" ## tempfile()
      fname <- file.path(wd_,"industries-1.Rmd")
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
      filebib <- file.path(root,"industries/Activities_pl.bib")
      a <- c("---","output: html_fragment",paste("bibliography:",filebib),"---"
            ,a,"### References")
      print(lapply(a,substr,1,64) |> do.call(c,args=_))
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
      str(a1)
      q()
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
            if (!grepl(paste0("^",industry),h3)) {
               if (T & h3!=iname)
                  return(NULL)
            }
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
industryDescription()
