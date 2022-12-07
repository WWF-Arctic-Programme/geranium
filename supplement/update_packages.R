repos <- "https://cloud.r-project.org"
remotes::update_packages(repos=repos
                        ,type=if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
                       # ,upgrade="default"
                        )
remotes::install_version("flexdashboard",version="0.5.2",upgrade="never",repos=repos)
