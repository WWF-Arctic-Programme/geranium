repos <- c("https://cloud.r-project.org","https://cran.asia")[1]
type <- if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
need_updating <- data.frame(old.packages(repos=repos,type=type))
to_update <- need_updating[!need_updating$Package %in% c("flexdashboard","DT")[1],]
if (length(to_update$Package)) {
   print(to_update$Package)
   install.packages(to_update$Package,repos=repos,type=type)
}
#install.packages()

#remotes::update_packages(repos=repos
#                        ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
#                       # ,upgrade="default"
#                        )
#remotes::install_version("flexdashboard",version="0.5.2",upgrade="never",repos=repos)
