invisible({
   if (length(grep("devel",R.Version()$status,ignore.case=TRUE)))
      stop("R-devel is not supported on shinyapps.io (seems to)")
   opW <- options(warn=10)
  # Sys.setlocale("LC_CTYPE",switch(.Platform$OS.type,windows="English","en_US.utf8"))
   appnameList <- c("soiga")
   appname <- commandArgs(TRUE)
   if (length(appname)!=1)
      appname <- tail(appnameList,1)
   else
      appname <- match.arg(appname,appnameList)
   wd <- setwd(".")
   require(rsconnect)
   # stop(getRversion())
  # options(rsconnect.http=c("rcurl","curl","internal")[2]
  #        ,rsconnect.check.certificate=FALSE
  #        ,rsconnect.http.verbose=FALSE)
   opShiny <- getOption(if (appname %in% c("geranium")) "rsconnectWWF" else "rsconnect")
   if (is.null(opShiny))
      stop("Authentification data are not receieved")
   res1 <- try(with(opShiny,setAccountInfo(name=name,token=token,secret=secret)))
   if (inherits(res1,"try-error"))
      str(res1)
  # print(appname)
  # print(getwd())
   appfiles <- dir(path=c("external","industry","predefined"),full.names=TRUE,recursive=TRUE)
   appfiles <- c(appfiles,"colorer.R","geranium.Rmd","news.Rmd","welcome.Rmd","geranium.css")
   res2 <- try(deployApp(appName=appname
                        ,appFiles=appfiles
                        ,appTitle="Geranium NG"
                        ,account=opShiny$name))
   if (inherits(res2,"try-error"))
      str(res2)
   setwd(wd)
   options(opW)
   plutil::timeHint("finished!")
})
warnings()
