repos <- "https://cloud.r-project.org"
pkglist <- c("rmarkdown","shiny","remotes","sf","ursa" ## 1:5
                  ,"leaflet","shinydashboard","mapedit","xml2" ## 6:9
                  ,"fasterize","formattable","readxl","interp" ## 10:13
                  ,"plotly","bookdown","rsconnect","pinp","rticles" ## 14:18
                  ,"tint","tufte","akima","xlsx","quarto", "rdrop2" ## 19:24
                  ,"here","shinyjs","rjson","shinycssloaders" ## 25:28
                  ,"DT","flexdashboard","lobstr","lorem" ## 29:32
                  ,"markdown","qs" ## 33:34
                  )[31] ## [1:26]
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
# if ((!requireNamespace("DBI"))||(packageVersion("DBI")>"1.1.3"))
#    remotes::install_version("DBI",version="1.1.3",upgrade="never",repos=repos)
#if ((!requireNamespace("DT"))||(packageVersion("DT")>"0.28"))
#   remotes::install_version("DT",version="0.28",upgrade="never",repos=repos)
#if ((!requireNamespace("leaflet"))||(packageVersion("leaflet")>="2.1.1"))
#   remotes::install_version("leaflet",version="2.1.1",upgrade="never",repos=repos)
