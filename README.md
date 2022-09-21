# Geranium

## Decision support and engagement prioritisation and visualisation tool for the Arctic Seas conservation planners

Spatial vulnerability between marine protection and human use in the Arctic

### How to use

1. If you need, install packages first using `source("supplement/install_packages.R")`.

2. In interactive R session use `rmarkdown::run("soiga.Rmd")`.

Note that downgradable version of `flexdashboard` (<0.6) is used for correct table displaying with "Scroller" extension. Example of downgrading: `devtools::install_version("flexdashboard", version = "0.5.2", repos = "https://cloud.r-project.org")`.
