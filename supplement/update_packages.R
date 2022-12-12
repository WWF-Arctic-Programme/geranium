repos <- "https://cloud.r-project.org"
need_updating <- data.frame(old.packages(repos=repos))
to_update <- need_updating[!need_updating$Package %in% "flexdashboard",]
if (length(to_update$Package)) {
   print(to_update$Package)
   install.packages(to_update$Package,repos=repos
                   ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
                   )
}
#remotes::update_packages(repos=repos
#                        ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
#                       # ,upgrade="default"
#                        )
#remotes::install_version("flexdashboard",version="0.5.2",upgrade="never",repos=repos)
