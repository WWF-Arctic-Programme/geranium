repos <- "https://cloud.r-project.org"
install.packages(c("rmarkdown","shiny","devtools","sf","ursa"
                  ,"leaflet","DT","mapedit","xml2"
                  ,"fasterize","formattable","readxl","interp") # [1:13]
                ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
                ,repos=repos)
devtools::install_version("flexdashboard",version="0.5.2",repos=repos)
