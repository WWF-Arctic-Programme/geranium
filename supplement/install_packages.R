repos <- "https://cloud.r-project.org"
pkglist <- c("rmarkdown","shiny","remotes","sf","ursa" ## 1:5
                  ,"leaflet","DT","mapedit","xml2" ## 6:9
                  ,"fasterize","formattable","readxl","interp" ## 10:13
                  ,"plotly","bookdown","rsconnect","pinp","rticles" ## 14:18
                  ,"tint","tufte","akima","xlsx","quarto", "rdrop2" ## 19:24
                  ,"shinybrowser","flexdashboard" ## [25:26]
                  )[1:24] ## [1:24]
ret <- sapply(pkglist,\(pkg) {
   if (!requireNamespace(pkg))
      return(pkg)
   ""
})
ret <- ret[nchar(ret)>0] |> unname()
install.packages(ret
                ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
                ,repos=repos)
if ((!requireNamespace("flexdashboard"))||(packageVersion("flexdashboard")>="0.6.0"))
   remotes::install_version("flexdashboard",version="0.5.2",upgrade="never",repos=repos)
