devel <- !grepl(file.path(gsub("^\\w:/","",Sys.getenv("RAS")),"shiny"),gsub("^\\w:/","",getwd()))
hlink <- FALSE
staffOnly <- nchar(Sys.getenv("MSOFFICE"))>0
isScroll <- TRUE
useExchange <- FALSE
# invisible(stats::runif(352))
seed <- sample(100:999,1)
set.seed(seed)
#if (!("shiny" %in% loadedNamespaces()))
#   require(shiny)
lib.loc <- if (staffOnly) "./packages" else NULL
if (!("ursa" %in% loadedNamespaces())) {
   require(ursa,lib.loc=lib.loc)
}
if (isShiny <- ursa:::.isShiny()) {
  # showModal(modalDialog(title="Initialization"
  #                      ,"Data preparing",size="s",easyClose = TRUE,footer = NULL))
   ursa:::.elapsedTime("require packages -- start")
   if (!("flexdashboard" %in% loadedNamespaces())) {
      require(flexdashboard,lib.loc=lib.loc)
   }
   require(leaflet)
   require(leaflet.extras)
   require(leafpm)
   require(fasterize)
   require(leafem)
   require(leafpop)
  # require(mapview)
   require(mapedit)
   require(DT)
   require(formattable)
   require(xml2)
   require(plotly)
  # require(shinycssloaders)
   ursa:::.elapsedTime("require packages -- end")
}
#md <- rmarkdown::metadata
#isShiny <- isTRUE(md$runtime=="shiny")
isReactive <- isShiny & F
source("resources/process.R")
source("resources/global.R",encoding="UTF-8")
# source("resources/header.R",encoding="UTF-8")
