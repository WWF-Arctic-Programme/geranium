source("colorer.R")
activity <- readxl::excel_sheets(mdname)
res <- lapply(activity,function(sheet) {
   print(sheet)
   v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")[,-seq(1,4)]
   cname <- colnames(v)
   print(cname)
   res <- character()
   res <- c(res,"",paste0("# ",sheet))
   for (i in seq_along(cname)) {
      print(cname[i])
      section <- paste0("i",digest::digest(cname[i],"crc32"))
      res <- c(res,"","")
      res <- c(res,"",paste0("## ",cname[i]," {#",section,"}"))
      res <- c(res,"",readLines("https://loripsum.net/api/plaintext/long/2"))
   }
   writeLines(res,paste0("industry/industry-",sheet,".Rmd"))
})
