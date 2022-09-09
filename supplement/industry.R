wd <- setwd("..")
source("resources/process.R")
activity <- readxl::excel_sheets(mdname)
res <- lapply(activity |> sample(),function(sheet) {
   output <- paste0("include/industry-",sheet,".Rmd")
   if (file.exists(output))
      return(NULL)
   print(sheet)
   v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")[,-seq(1,4)]
   cname <- grep("^comment",colnames(v),ignore.case=TRUE,invert=TRUE,value=TRUE)
   abbr <- industryAbbr$abbr[match(cname,industryAbbr$industry)]
   aname <- paste0(abbr,": ",cname)
   print(cname)
   res <- character()
   res <- c(res,"---","pagetitle: industry template","---")
   res <- c(res,"",paste0("## ",sheet))
   for (i in seq_along(cname)) {
      print(cname[i])
      section <- paste0("i",digest::digest(cname[i],"crc32"))
      res <- c(res,"","")
      res <- c(res,"",paste0("### ",aname[i]," {#",section,"}"))
      res <- c(res,"",readLines("https://loripsum.net/api/plaintext/long/2"))
   }
   writeLines(res,output)
})
setwd(wd)
