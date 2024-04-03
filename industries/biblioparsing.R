require(RefManageR)
'insert_link' <- function(a,rec,verbose=FALSE) {
   md <- comment(a)
   if (is.null(md))
      md <- character()
   if (verbose)
      print(rec)
   ref <- RefManageR::Citep(b,rec)
   if (verbose)
      print(ref)
   ref <- c(gsub("\\)$","\\\\)",gsub("^\\(","\\\\(",ref))
           ,gsub("\\)$",",\\\\s",gsub("^\\(","\\\\(",ref))
           ,gsub("\\)$","\\\\)",gsub("^\\(",",\\\\s",ref))
           ,gsub("\\)$",",\\\\s",gsub("^\\(",",\\\\s",ref))
           )
   replace <- c(paste0("[@",rec,"]"),paste0("[@",rec,", ")
               ,paste0(", @",rec,"]"),paste0(", @",rec,", "))
   if (verbose)
      print(ref)
   sP <- 0L
   for (j in seq_along(ref)) {
      ind <- gregexpr(ref[j],a)
      m <- regmatches(a,ind)
      s <- sapply(m,length) |> sum()
      if (verbose)
         print(s)
      if (s==0)
         next
      regmatches(a,ind) <- replace[j]
      sP <- sP+s
   }
   ref <- RefManageR::Citet(b,rec)
   ref <- gsub("\\)$","\\\\)",gsub("\\(","\\\\(",ref))
   if (verbose)
      print(ref)
   ind <- gregexpr(ref,a)
   m <- regmatches(a,ind)
   sT <- sapply(m,length) |> sum()
   if (verbose)
      print(sT)
   if (sT>0)
      regmatches(a,ind) <- paste0("@",rec,"")
   if (sP+sT==0) {
      md <- c(md,rec)
   }
   comment(a) <- md
   a
}
RefManageR::BibOptions(check.entries=FALSE
                      ,bib.style=c('authoryear','draft','numeric')[1]
                      ,cite.style=c('pandoc','authoryear','alphabetic')[2]
                      ,max.names=1
                      ,longnamesfirst=FALSE
                      ,style=c('markdown','citation')[1]
                      ,hyperlink=list(FALSE,'to.doc','to.bib')[[1]]
                      ,no.print.fields=c('isbn')
                      ,dashed=FALSE)
b <- RefManageR::ReadBib("Activities_pl.bib",check=FALSE)
a <- readLines("industries-0.md")
bname <- unname(names(b)) ## missed "RN7" "RN40" "RN21"
if (F) {
   print(length(bname))
   bname <- bname[grep("RN(7|21|40)",bname,invert=TRUE)]
   print(length(bname))
}
if (F) {
   a <- insert_link(a,"RN40",verbose=TRUE)
   q()
}
res <- character()
for (i in seq_along(bname) |> sample()) {
   a <- insert_link(a,bname[i])
}
writeLines(a,"industries-1.md")
bname <- comment(a)
print(bname)
bname <- bname[grep("RN(7|21|40)",bname,invert=TRUE)]
if (length(bname)) {
   comment(a) <- NULL
   for (i in seq_along(bname) |> sample(1)) {
      a <- insert_link(a,bname[i],verbose=TRUE)
   }
   bname <- comment(a)
   print(bname)
} else {
   cat("ok!\n")
}
