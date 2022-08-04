#'
#+
##~ 'monther' <- function(s) {
   ##~ as.integer(format(as.Date(paste0("2021-",substr(s,1,3),"-15")
                    ##~ ,format="%Y-%b-%d"),"%m"))
##~ }
##~ mdname <- "external/compatibility assessment_all_2021-05-24-seasons.xlsx"
##~ kwdRed <- "Incompatible"
##~ kwdYellow <- c("Concessional","Conditional","Compatible under certain conditions")[2]
##~ kwdGreen <- c("Compatible","Compatible/not applicable")[2]
##~ kwdLUT <- c('0'=kwdGreen,'1'=kwdYellow,'2'=kwdRed)
source("colorer.R")
ursa:::.elapsedTime("A")
cf <- "5011"
md <- read.csv(dir(path="external",pattern="scenario.*\\.csv$"
                  ,ignore.case=TRUE,full.names=TRUE),check.names=FALSE)
md <- md[md$CF_code %in% cf,]
md <- md[md$CF_code %in% cf,grep("(^$|^File_name|groupcode)",colnames(md),invert=TRUE)]
da <- t(as.data.frame(md,check.names=FALSE))
#da <- da[!apply(da,1,is.na),]
knitr::kable(da)
if (F) {
   activity <- readxl::excel_sheets(mdname)
   #patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
   res <- lapply(activity,function(sheet) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")[,1:2]
      v
     # v[v$CF_code %in% id,]
   }) |> do.call(rbind,args=_) |> unique()
   ursa:::.elapsedTime("B")
   str(res)
   q()
   res$CF_name <- gsub("^\\b(\\w)","\\U\\1",res$CF_name,perl=TRUE)
   res <- unique(res)
   table(as.integer(table(res$CF_code)))
   res[res$CF_code %in% 6074,]
   q()
}
if (F) {
   da <- human_use('5011')
   str(da)
   #da <- data.frame(da)
   #str(da)
   #knitr::kable(da)
   b <- DT::datatable(da,rownames=FALSE)
   b <- DT::formatStyle(b,colnames(da)
                     ,backgroundColor=DT::styleEqual(c(
                                                      '0','1','2'
                                                      ,kwdGreen,kwdYellow,kwdRed
                                                     )
                                           ,c("palegreen","LemonChiffon","lightsalmon"
                                             ,"palegreen","LemonChiffon","lightsalmon"))
                     ,backgroundSize='95% 18pt'
                     ,backgroundRepeat='no-repeat'
                     ,backgroundPosition='center'  
                     )
   b
}
