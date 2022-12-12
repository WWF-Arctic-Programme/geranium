repos <- "https://cloud.r-project.org"
install.packages(c("rmarkdown","shiny","remotes","sf","ursa" ## 1:5
                  ,"leaflet","DT","mapedit","xml2" ## 6:9
                  ,"fasterize","formattable","readxl","interp" ## 10:13
                  ,"plotly","bookdown","rsconnect","pinp","rticles" ## 14:18
                  ,"tint","tufte","akima","xlsx" ## 19:22
                  )[21:22] 
                ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
                ,repos=repos)
#remotes::install_version("flexdashboard",version="0.5.2",upgrade="never",repos=repos)
