require(xml2)
fname <- "C:/tmp/Human use profiles.md"
# fname <- "../include/industry-Tourism.Rmd"
industry <- "Crab traps"
activity <- "Fishery"
lut <- read.csv("../requisite/industry.csv")
if (length(ind <- match(industry,lut$industry))) {
   if (nchar(lut$manual[ind]))
      industry <- lut$manual[ind]
}
fileout <- "../include/industries.html"
ursa:::.elapsedTime("A")
if (T & !file.exists(fileout)) {
   o <- rmarkdown::render(fname
                    ,output_format=rmarkdown::html_fragment()
                   # ,output_format=rmarkdown::html_vignette(css=NULL)
                    ,output_file=fileout,quiet=TRUE
                   # ,params=list(prm=analysis(),kind=1L)
                    )
   print(o)
} else {
   o <- fileout
  # o <- file.path(dirname(fname),fileout)
}
ursa:::.elapsedTime("B")
# o <- "../include/industry-Tourism.Rmd"
# o <- "C:/tmp/Human use profiles.html"
try({
   a0 <- read_xml(o,as_html=!TRUE,options=c("RECOVER","NOERROR","NOBLANKS"))
   a1x <- as_list(a0)
  # str(a1x)
})
a0 <- read_xml(o,encoding="UTF-8",as_html=TRUE)#,options=c("RECOVER","NOERROR","NOBLANKS"))
a1 <- as_list(a0)[[1]][[1]]
if (F) {
   patt <- "^\r\n$"
   a1 <- lapply(a1,function(b1) {
      c1 <- attributes(b1)
     # c1$names <- NULL
      if (is.list(b1)) {
         b1 <- lapply(b1,function(b2) {
            c2 <- attributes(b2)
           # c2$names <- NULL
            if (is.list(b2)) {
               b2 <- lapply(b2,function(b3) {
                  c3 <- attributes(b3)
                 # c3$names <- NULL
                  if (is.list(b3)) {
                     b3 <- lapply(b3,function(b4) {
                        if (is.list(b4)) {
                           b4 <- lapply(b4,function(b5) {
                              if (is.list(b5)) {
                                 b5 <- lapply(b5,function(b6) {
                                    if (is.list(b6))
                                       stop("b6")
                                    else
                                       b6 <- grep(patt,b6,inv=TRUE,value=TRUE)
                                    if (is.list(b6))
                                       b6 <- b6[sapply(b6,function(x) length(x)>0)]
                                    b6
                                 })
                              }
                              else
                                 b5 <- grep(patt,b5,inv=TRUE,value=TRUE)
                              if (is.list(b5))
                                 b5 <- b5[sapply(b5,function(x) length(x)>0)]
                              b5
                           })
                        }
                        else
                           b4 <- grep(patt,b4,inv=TRUE,value=TRUE)
                        if (is.list(b4))
                           b4 <- b4[sapply(b4,function(x) length(x)>0)]
                        b4
                     })
                  }
                  else
                     b3 <- grep(patt,b3,inv=TRUE,value=TRUE)
                  if (is.list(b3)) {
                     ind3 <- sapply(b3,function(x) length(x)>0)
                     b3 <- b3[ind3]
                     c3$names <- c3$names[ind3]
                  }
                 # attributes(b3) <- c3
                  b3
               })
            }
            else
               b2 <- grep(patt,b2,inv=TRUE,value=TRUE)
            if (is.list(b2)) {
               ind2 <- sapply(b2,function(x) length(x)>0)
               b2 <- b2[ind2]
               c2$names <- c2$names[ind2]
            }
            attributes(b2) <- c2
            b2
         })
      }
      else
         b1 <- grep(patt,b1,inv=TRUE,value=TRUE)
      if (is.list(b1)) {
         ind1 <- sapply(b1,function(x) length(x)>0)
         b1 <- b1[ind1]
         c1$names <- c1$names[ind1]
      }
      attributes(b1) <- c1
      b1
   })
}
a1 <- a1[sapply(a1,function(x) length(x)>0)]
if (F) {
   a0 <- as_xml_document(list(a1))
   write_html(a0,"res2.html")
}
ursa:::.elapsedTime("C")
#str(a1)
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
         return(a3)
      h3 <- a3$h3[[1]]
      if (h3!=industry)
         return(NULL)
      a3
   })
   ind2 <- sapply(a2,function(x) !is.null(x))
   a2 <- a2[ind2]
   c2$names <- c2$names[ind2]
   attributes(a2) <- c2
   a2
})
a1 <- a1[sapply(a1,function(x) !is.null(x))]
length(a1)
q()
a0 <- as_xml_document(list(a1))
write_html(a0,"res2.html")
ursa:::.elapsedTime("D")
browseURL("res2.html")
