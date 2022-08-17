invisible(Sys.setlocale("LC_TIME","C"))
##~ if (!file.exists("external/inventory.rds")) {
   ##~ list1 <- dir(path="output-overlap",pattern="interim\\.tif"
               ##~ ,full.names=TRUE,recursive=TRUE)
   ##~ md5 <- sapply(list1,digest::digest,"crc32")
   ##~ da <- data.frame(ref=md5,src=list1,dst=file.path("external",paste0(md5,".tif")))
   ##~ saveRDS(da,"external/inventory.rds")
   ##~ file.copy(da$src,da$dst,copy.date=TRUE,overwrite=FALSE)
##~ }
##~ if (check <- !TRUE) {
   ##~ inventory <- readRDS("external/inventory.rds")
   ##~ str(inventory)
   ##~ src <- unique(lapply(strsplit(inventory$src,split="/"),function(x) x[2]))
   ##~ str(src)
   ##~ q()
##~ }
require(ursa)
#plutil::ursula(3)
isShiny <- ursa:::.isShiny()
if (isShiny) {
   ursa:::.elapsedTime("require packages -- start")
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
   ursa:::.elapsedTime("require packages -- end")
}
'mapper' <- function(obj,pal) {
   devel2 <- FALSE
   if (missing(pal))
      pal <- cubehelix(5)
   if (band_blank(obj>0))
      return(colorize(obj,pal=pal))
   n <- length(pal)
   if (n==1) {
      d1 <- colorize(obj,pal=pal)
      return(d1)
   }
   mv <- band_max(obj)
   mul <- ifelse(mv>12,1,ifelse(mv>1.2,10,100))
   b1 <- round(mul*obj)
   b0 <- b1[b1==0]
   b2 <- b1[b1>0]
   d1 <- reclass(b2,stretch="eq",nbreak=n-2L)
  # print(as.table(d1))
  # print(names(ursa_colortable(d1)))
  # print(b1)
   cl1 <- c(0,unique(round(ursa:::.deintervale(ursa_colortable(d1)))))
   cl1 <- cl1[seq(n-1L)]
   cl1 <- c(na.omit(cl1)) ## patch
  # print(c('cl1:'=cl1))
   cl2 <- c(unname(band_min(b1))-1,cl1,round(unname(band_max(b1))))/mul
  # cl2 <- unique(cl2) ## patch
  # print(c('cl2:'=cl2))
   da <- data.frame(from=head(cl2,-1)+1/mul,to=tail(cl2,-1))
   da <- da[da$from<=da$to,] ## patch
  # print(da)
   if (tail(da$to,1)/tail(da$from,1)>100)
      da$to[nrow(da)] <- "..."
  # print(da)
   cname <- apply(da,1,function(x) {
      paste(unique(x),collapse=" - ")
   })
   cl1 <- cl1[seq(nrow(da)-1L)]
   if (devel2) {
      print(c('cname:'=cname))
      print(c('breakvalue:'=cl1/mul))
      print(c('pal:'=pal))
   }
   d1 <- colorize(obj,breakvalue=cl1/mul,name=cname,pal=pal)
   d1
}
'legender' <- function(obj,x=0.02,y=0.93,title="Values:") {
   ct1 <- ursa_colortable(obj)
   g0 <- session_grid()
   legend(x=with(g0,minx+x*(maxx-minx)),y=with(g0,miny+y*(maxy-miny))
         ,title=as.expression(substitute(bold(u),list(u=title)))
         ,legend=names(ct1)
         ,pt.bg=unname(ct1)
         ,pch=22,pt.cex=3,pt.lwd=0.5
        # ,fill="green",border="transparent"
         ,cex=1.5,box.lwd=0.1,bg="#FFFFFFCF",y.intersp=0.9,
         )
}
'decorer' <- function() {
  # ursa:::.ursaOptions()
  # panel_decor(scalebar.pos="topleft")
  # compose_coastline(spatial_read("crop-coast.sqlite"),fill="grey93",col="grey70")
   panel_decor(coast=if (exists("clf")) clf else FALSE,scalebar.pos="topleft")
  # panel_decor(coast.fill="grey93",scalebar.pos="topleft")
  # cl <- spatial_read("crop-coast.sqlite",engine="sf")
  # panel_coastline(cl,fill="grey93",verbose=TRUE)
  # panel_graticule()
  # panel_scalebar(pos="topleft")
}
'colorbar2d' <- function(pal=c("RdBu","RdYl"),dim=c(5,5)) {
   devel2 <- FALSE
   pal <- match.arg(pal)
   g0 <- session_grid();on.exit(session_grid(g0))
   dim2 <- dim
   dim2[dim2==1] <- 2
   a <- array(NA,dim=c(dim2,3))
   dima <- dim(a)
  # hdim <- (dima-1)/2+1
   a[dima[1],1,] <- c(1,1,1)*0.95 ## bottomleft
   if (F & all(dim==1)) {
      ret <- as.raster(a) ## inherits(a,"raster")
      return(ret)
   }
   if (all(dim2>1)) {
      if (pal %in% "RdBu")
         a[1,dima[2],] <- c(0.4,0,0.4) ## topright
      else if (pal %in% "RdYl")
         a[1,dima[2],] <- c(0.4,0.2,0) ## topright
      else
         stop("unknown value for 'topright'")
   }
   if (dim2[1]>1)
      a[1,1,] <- c(0.9,0.4,0.4) ## topleft
   if (dim2[2]>1) {
      if (pal %in% "RdBu")
         a[dima[1],dima[2],] <- c(0.4,0.4,0.9) ## bottomright
      else if (pal %in% "RdYl")
         a[dima[1],dima[2],] <- c(0.8,0.8,0.1) ## bottomright 0.9|0.9|0.4
      else
         stop("unknown value for 'bottomright'")
   }
   if (devel2) {
      print(dima)
      message("before interp:")
      print(a)
      saveRDS(a,"c:/tmp/interp1-before.rds")
   }
   #a[hdim[1],hdim[2],] <- c(1,1,0)
   #a[dima[1],hdim[2],] <- c(0,1,1)
   #a[hdim[1],dima[2],] <- c(1,1,0)
   a1 <- as.ursa(a,flip=!TRUE,permute=!TRUE)
   a2 <- as.data.frame(a1)
   a2$x <- a2$x+0.5
   a2$y <- a2$y+0.5
   if (devel2)
      print(a2)
   xo <- seq(1,dima[1]+1)
   yo <- seq(1,dima[2]+1)
   if (devel2)
      saveRDS(list(a2=a2,dima=dima),"c:/tmp/interp2-interim.rds")
   for (i in c(seq_len(dima[3]))) {
      if (FALSE) {
         b <- akima::interp(x=a2$y,y=a2$x,z=a2[,i+2L,drop=TRUE],nx=dima[2],ny=dima[1]
                           ,duplicate="mean",remove=!TRUE)$z
      }
      else {
         b <- interp::interp(x=a2$x,y=a2$y,z=a2[,i+2L,drop=TRUE]
                           ,xo=xo,yo=yo
                           ,method="linear")$z
         b <- b[seq(1,dima[1]),seq(1,dima[2])]
      }
      a[,,i]<- b
   }
   if (devel2) {
      message("after interp:")
      saveRDS(a,"c:/tmp/interp3-after.rds")
      print(a)
   }
   if (dim[1]==1)
      a <- a[2,,,drop=FALSE]
   if (dim[2]==1)
      a <- a[,1,,drop=FALSE]
   ret <- as.raster(a) ## inherits(a,"raster")
   ret
}
'interim' <- function(activity,group=NULL,aoi=NULL,season=list("max",NULL)[[1]]
                     ,simplify=c("none","map","stat")) {
   simplify <- match.arg(simplify)
   cat("interim():\n")
   str(list(activity=activity,group=group,aoi=aoi,simplify=simplify,season=season))
   isStat <- simplify %in% c("none","stat")
   isMap <- simplify %in% c("none","map")
   verb <- paste0("Data processing"
                 ,ifelse(!isStat," (layer)"
                 ,ifelse(!isMap," (stats)","")))
   gr <- gsub(pattRules,"\\1",activity)
   allUse <- gr==allActivity
   indRule <- if (allUse) seq_len(nrow(rules)) else which(rules$activity==activity)
   rule <- rules[indRule,] ## [25:30]
   if ((nrow(rule)==0)&&(!allUse))
      return(NULL)
  # act <- gsub(pattRules,"\\2",activity)
   act <- gsub(pattRules,"\\2",rule$activity)
   gr <- gsub(pattRules,"\\1",rule$activity)
   print(c('all human use'=allUse))
   if (isShiny)
      showNotification(id="interim",closeButton=FALSE,duration=120,paste0(verb,"...")
                      ,type="warning")
   ursa:::.elapsedTime(paste(verb,"-- start"))
   if ((T | !isShiny)&&(nrow(rule)==1))
      print(rule)
   session_grid(dist2land)
   mrule <- ursa(bandname=rule$activity)
   for (i in seq_len(nrow(rule))) {
      mrule[i] <- dist2land["dist"]>=(rule$min[i]*cell) &
                dist2land["dist"]<=(rule$max[i]*cell)
   }
   mrule[is.na(mrule)] <- 0
   if (isPAC <- !is.null(aoi)) {
      if (is.character(aoi)) {
         pacvalue <- as.integer(gsub("\\D","",aoi))
         pac2 <- pac0[pac0==pacvalue]
         m <- !is.na(pac2)
         pac1 <- ursa_crop(pac2,border=1)
      }
      else {
         pac1 <- aoi
         pac2 <- regrid(aoi,blank)
         m <- !is.na(pac2)
      }
      g0 <- ursa_grid(pac1)
     # msk <- regrid(msk)[!is.na(pac1)]
     # blank1 <- regrid(blank)
   }
  # else
  #    blank1 <- blank
   if (!isPAC) {
      if (simplify %in% "stat") {
         if (isShiny)
            removeNotification(id="interim")
         ursa:::.elapsedTime(paste(verb,"-- finish"))
         stat2 <- data.frame()
         return(stat2)
      }
   }
   vulner <- vector("list",nrow(rule))
   for (i in seq_along(vulner)) {
      v <- readxl::read_excel(mdname,sheet=gr[i],.name_repair="minimal")
      v <- v[,c("CF_code","CF_name","Limitations",act[i])[]]
      v <- v[!is.na(v$CF_code),]
      colnames(v)[length(colnames(v))] <- "value"
      v <- as.data.frame(v)
      if (!is.null(season)) {
         s0 <- monther(season)
         if (is.na(s0)) {
            if (grepl("^max|maximum|maximal",season)) {
               v <- aggregate(list(CF_name=v$CF_name,value=v$value)
                             ,by=list(CF_code=v$CF_code),max)
            }
         }
         else {
            patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
            n <- length(v$Limitations)
            res <- sapply(v$Limitations,function(s) {
               s1 <- monther(gsub(patt,"\\1",s))
               s2 <- monther(gsub(patt,"\\2",s))
               s3 <- if (s1<s2) seq(s1,s2) else s3 <- c(seq(s1,12),seq(1,s2))
               s0 %in% s3
            })
            v <- v[which(res),]
         }
      }
      vulner[[i]] <- v
   }
   cfcode <- lapply(vulner,function(v) v$CF_code) |> unlist() |> unique() |> sort()
   if (!isPAC)
      g0 <- ursa_grid(amountFile)
   session_grid(amountFile)
   amount <- open_envi(amountFile,cache=T | !isPAC)
   aname <- names(amount)
   if (is.null(group)) {
      groupList <- c("\\d",sort(unique(substr(aname,1,1))))
      group <- sample(groupList,1)
      if (!isShiny)
         print(c(group=group))
   }
   sname <- c("0","1","2")
   if (isMap) {
      a2 <- ablank <- ursa(0,bandname=sname,ignorevalue=1e4)
   }
   aname <- grep(paste0("^",group,"\\d{3}"),aname,value=TRUE)
  # print(c(gr=gr,act=act),quote=FALSE)
  # str(aname)
  # writeLines(jsonlite::toJSON(aname),"tmp_aname.json")
   n <- length(aname)
  # cf <- v$CF_code %/% 1000
  # print(table(cf))
   cb <- chunk_band(amount,1000)
   if (F) {
      if (isStat) {
         stat2 <- vector("list",length(sname))
         names(stat2) <- sname
      }
      msk <- mrule[1]
      v <- vulner[[1]]
      for (i in sname) { ## '0' '1' '2'
         verbose <- i=="z222"
         cf <- as.integer(v[v$value %in% i,"CF_code"])
         ind <- na.omit(match(cf,aname))
         if (length(ind)>0) {
            s <- aname[ind]
            indS <- match(s,aname)
            if (isStat)
               stat1 <- vector("list",length(cb))
            for (j in seq_along(cb)) {
               j2 <- cb[[j]]
               indJ <- j2[na.omit(match(indS,j2))]
               if (!length(indJ))
                  next
               nameJ <- aname[indJ]
               str(nameJ)
               am <- amount[nameJ]
               am[is.na(am)] <- 0
               a3 <- sum(am,cover=1e-6)
               if (isMap)
                  a2[i] <- a2[i]+a3
               if (isStat) { ## if (!((!isPAC)&&(i!='zzz0'))) {
                  br <- rep(1,length(am))
                  b1 <- b2 <- b3 <- br
                  if (isPAC) {
                     b1 <- apply(ursa_value(am[m]),2,sum,na.rm=TRUE)
                     b0 <- apply(ursa_value(am),2,sum,na.rm=TRUE)
                     b1 <- b1/b0
                  }
                  b <- data.frame(code=names(am)
                                 ,name=v$CF_name[match(names(am),v$CF_code)]
                                 ,value1=b1#,value2=b2,value3=b3
                                 ,flag=kwdLUT[i],row.names=NULL)
                  ind <- grep("^value",colnames(b))[1]
                  b <- b[!is.na(b[[ind]]) & b[[ind]]>0.001,]
                  stat1[[j]] <- b
               }
               rm(am)
               ursa:::.gc(F & isShiny)
            }
            if (isStat) {
               stat2[[i]] <- do.call(rbind,stat1)
            }
         }
      }
   }
   else {
      if (isStat) {
         stat2 <- data.frame()
        # stat1 <- vector("list",length(cb))
      }
      for (j in seq_along(cb)) {
         j2 <- cb[[j]]
        # j3 <- c(na.omit(match(aname[j2],v$CF_code)))
         j2 <- j2[na.omit(match(cfcode,aname[j2]))]
         am0 <- amount[j2]
         pb <- ursaProgressBar(vulner,silent=length(vulner)==1)
         for (i in seq_along(vulner)) {
            setUrsaProgressBar(pb)
            if (isStat) {
               stat3 <- vector("list",length(cb))
            }
            v <- vulner[[i]]
            if (isMap) {
               msk <- mrule[i]
               a4 <- ablank
            }
            for (s in sname) { ## '0' '1' '2'
               cf <- as.integer(v[v$value %in% s,"CF_code"])
               ind <- c(na.omit(match(cf,aname[j2])))
               if (!length(ind))
                  next
               am <- am0[ind]
               am[is.na(am)] <- 0
               if (isMap) {
                  a3 <- sum(am,cover=1e-6)
                  a4[s] <- a4[s]+a3
               }
               if (isStat) { ## if (!((!isPAC)&&(i!='zzz0'))) {
                  br <- rep(1,length(am))
                  b1 <- b2 <- b3 <- br
                  if (isPAC) {
                     b1 <- apply(ursa_value(am[m]),2,sum,na.rm=TRUE)
                     b0 <- apply(ursa_value(am),2,sum,na.rm=TRUE)
                     b1 <- b1/b0
                  }
                  b <- data.frame(code=names(am)
                                 ,name=v$CF_name[match(names(am),v$CF_code)]
                                 ,value1=b1#,value2=b2,value3=b3
                                 ,flag=kwdLUT[s],row.names=NULL)
                  ind <- grep("^value",colnames(b))[1]
                  b <- b[!is.na(b[[ind]]) & b[[ind]]>0.001,]
                  stat2 <- rbind(stat2,b)
                 # stat1[[j]] <- b
               }
              # if (isStat)
              #    stat2[[s]] <- do.call(rbind,stat1)
            }
            if (isMap) {
               a2 <- a2+a4*msk
            }
         }
         close(pb)
         rm(am0,am)
         ursa:::.gc(T & isShiny)
      }
   }
   close(amount)
   if (isMap) {
      a2 <- a2[blank]
   }
   if (F & isMap)
      write_envi(a2,"c:/tmp/tmp_a2")
   if (F & isStat)
      saveRDS(stat2,"c:/tmp/tmp_stat2.rds")
   session_grid(g0)
   if (isStat) {
      if (!is.data.frame(stat2))
         stat2 <- do.call(rbind,stat2)
      if (is.null(stat2))
         stat2 <- data.frame()
      if (T & nrow(stat2)>1)
         stat2 <- stat2[with(stat2,order(-match(flag,kwdLUT),-value1)),]
      rownames(stat2) <- NULL
   }
   if (isStat & !isShiny)
      stat2$name <- NULL
   if (isShiny)
      removeNotification(id="interim")
   ursa:::.elapsedTime(paste(verb,"-- finish"))
   if (simplify %in% "map")
      return(a2)
   if (simplify %in% "stat")
      return(stat2)
   list(map=a2,stat=stat2)
}
'zoomToAOI' <- function(pac) {
   if (is.character(pac)) {
      pac1 <- regionU[[names(pac)]]
      pac1 <- pac1[pac1==pac]
      pac1 <- ursa_crop(pac1,border=1)
     # g0 <- ursa_grid(pac1)
   }
   else if (is_ursa(pac))
      pac1 <- ursa_crop(pac,border=1)
   else if (is.null(pac)) {
      pac1 <- blank
   }
   session_grid(pac1)
   pac1
}
'map1' <- function(a) {
   NULL
}
'map2' <- function(a,grid=NULL,pac=NULL) {
   b3 <- a["2"]
   d2 <- mapper(b3[b3>=0.5],pal=pRd)
  # print(band_blank(d2))
   d5 <- mapper(b3<0.5,pal="#2f5faf3f")
   names(ursa_colortable(d5)) <- "No full ban zone"
   if (band_blank(d2)) {
      d2 <- d5
   }
   else {
      ursa_value(d2) <- ursa_value(d2)+1L
      ind <- !is.na(ursa_value(d5))
      ursa_value(d2)[ind] <- ursa_value(d5)[ind]
      ignorevalue(d2) <- ignorevalue(d2)+1L
      ursa_colortable(d2) <- c(ursa_colortable(d5),ursa_colortable(d2))
   }
   compose_open(height=height*retina,retina=retina,legend=NULL)
   panel_new("white")
   ct2 <- panel_raster(d2)
   decorer()
   if (!is.null(pac))
      panel_plot(pac,pal="#FFFFFFA0",border="blue",lwd=3)
   legender(ct2,x=0.02,title="Threat index #2")
   compose_close()
}
'map3' <- function(a,grid=NULL,pac=NULL) {
   if (!is.null(grid))
      session_grid(grid)
   d3 <- mapper(a['1'],pal=pYl)
   d4 <- mapper(a['2'],pal=pRd)
   blank3 <- band_blank(d3>0)
   blank4 <- band_blank(d4>0)
   compose_open(height=height*retina,retina=retina,legend=NULL)
   panel_new("white")
   if (!blank3)
      ct3 <- panel_raster(d3)
   if (!blank4)
      ct4 <- panel_raster(d4)
   decorer()
   if (!is.null(pac))
      panel_plot(pac,pal="#FFFFFFA0",border="blue",lwd=3)
   if (!blank3)
      legender(ct3,x=0.02,title="Concession index #3")
   if (!blank4)
      legender(ct4,x=0.22,title="Threat index #3")
   ret <- compose_close(bpp=8)
   NULL
}
'map3_2d_DEPRECATED' <- function(a,grid=NULL,pac=NULL) {
   if (!is.null(grid))
      session_grid(grid)
   g0 <- session_grid()
   session_grid(a)
   d3 <- mapper(a['1'])
   d4 <- mapper(a['2'])
   if (F & !isShiny) {
      print(d4)
      display(d4)
      display(a['2'])
      q()
   }
   dima <- c(length(ursa_colortable(d4)),length(ursa_colortable(d3)))
   cb <- colorbar2d("RdYl",dim=dima)
   if (F & !isShiny) {
      print(cb)
      Fout <- "tmp_colorbar2d.svg"
      svglite::svglite(Fout)
      plot(as.raster(cb),interpolate=FALSE)
      dev.off()
     # browseURL(Fout)
      q()
   }
  # print(cb)
   ursa_colortable(d3)[] <- cb[nrow(cb),]
   ursa_colortable(d4)[] <- rev(cb[,1])
  # plot(as.raster(cb),interpolate=FALSE)
   cb2 <- cb[rev(seq(dim(cb)[1])),]
   cb3 <- t(cb2)
   ##~ cb2 <- cb[,rev(seq(dim(cb)[2]))]
   lut <- data.frame(d3=c(row(cb3))-1L,d4=c(col(cb3))-1L,pal=c(cb2),ind=seq_along(c(cb2))
                    ,val=-1,desc="")
   lut$val <- lut$d3*nrow(cb)+lut$d4
  # d3 <- mapper(a['1'],pal=cb[nrow(cb),])
  # d4 <- mapper(a['2'],pal=rev(cb[,1]))
   ##~ str(ursa_colortable(d3))
   ##~ str(ursa_colortable(d4))
   ##~ q()
   n3 <- names(ursa_colortable(d3))
   n4 <- names(ursa_colortable(d4))
   ind3 <- seq_along(n3)-1L
   ind4 <- seq_along(n4)-1L
  # lut <- lut[lut$d3 %in% ind3 & lut$d4 %in% ind4,]
   lut$desc <- paste0(kwdYellow,": ",n3[lut$d3+1L],", ",kwdRed,": " ,n4[lut$d4+1L])
   d5 <- c(overlap=d3*length(ursa_colortable(d4))+d4)
   if (F) {
      ta <- as.table(d5)
      ind <- match(as.integer(names(ta)),lut$val)
      lut <- lut[ind,]
   }
   d6 <- c(name=colorize(d5,value=lut$val,pal=lut$pal,name=lut$desc))
   if (devel <- FALSE) {
     # str(d6)
      opW <- options(warn=1)
      if (T) {
         write_envi(d6,"map3")
        # source("map3.R")
      }
      options(opW)
      if (F) {
        # write_gdal(d6,"map3.tif")
         d7 <- c(color=colorize(d5,value=lut$val,pal=lut$pal,name=lut$pal))
         d8 <- c(value=colorize(d5,value=lut$val,pal=lut$pal,name=lut$val))
         print(ursa_colortable(d6))
         print(as.table(d6))
         e6 <- polygonize(list(d8,d6,d7))
         spatial_write(e6,"map3.geojson")
      }
      compose_open(1,legend=NULL
                 # ,fileout=output4
                 # ,pointsize=8
                 # ,dpi=300
                 # ,width=1800
                  ,height=height*retina,retina=retina
                  )
      panel_new("white")
      panel_raster(d6)
      decorer()
      if (!is.null(pac))
         panel_plot(pac,pal="#FFFFFFA0",border="blue",lwd=3)
      g0 <- session_grid()
      if (T)
         with(g0,{
            panel_annotation(x=minx+0.17*(maxx-minx),y=miny+0.80*(maxy-miny),label="   "
                            ,fill="#FFFFFFAF",fg="white",cex=11,bg="transparent"
                           # ,buffer=1,border="green"
                            )
         })
      if (T) {
         fig <- par()$fig
         opA <- par(fig=c(fig[1]+0.13*(fig[2]-fig[1])
                         ,fig[1]+0.33*(fig[2]-fig[1])
                         ,fig[3]+0.75*(fig[4]-fig[3])
                         ,fig[3]+0.95*(fig[4]-fig[3])
                         ),mar=c(2,2,0,0),new=TRUE)
         plot(cb,interpolate=FALSE,xaxs="r",yaxs="r",asp=1)
         mtext(1,at=seq(0,4)+0.5,text=names(ursa_colortable(d3)),las=2)
         mtext(2,at=seq(0,4)+0.5,text=names(ursa_colortable(d4)),las=1)
        # mtext(3,text=expression(bold("traffic")),col="black")
        # mtext(4,text=expression(bold("map #2")),col="black")
         mtext(1,text=expression(bold("Concession index #3"))
              ,at=0
              ,line=5.5
              ,adj=0.5
              ,col="black")
         mtext(2,text=expression(bold("Threat index #3"))
              ,at=0
              ,line=6
              ,adj=0.6
              ,col="black"
              )
         par(opA)
      }
      ret <- compose_close(bpp=8)# render=!batch)
      print("fig for report -- done!\n")
      NULL
   }
   session_grid(g0)
   return(d6)
}
'map3_1d' <- function(a,grid=NULL,kind=c("overlap","mix","threat","source")
                     ,source="zzz") {
   kind <- match.arg(kind)
   if (!is.null(grid))
      session_grid(grid)
   g0 <- session_grid()
   session_grid(a)
   if (kind %in% "threat") {
      th <- 0.5
      b3 <- a['2']
     # b3 <- b3[b3>=th]-th-1e-6
      d2 <- mapper(b3,pal=pRd)
     # d5 <- mapper(b3<0.5,pal="#2f5faf3f")
      dima <- c(length(ursa_colortable(d2)),8)
      print("0706f")
      print(dima)
      cb <- colorbar2d("RdBu",dim=dima)
      print("0706g")
     # ursa_colortable(d5)[] <- cb[nrow(cb),2]
      ursa_colortable(d2)[-1] <- head(rev(cb[,1]),-1)
      ursa_colortable(d2)[1] <- cb[nrow(cb),2]
      names(ursa_colortable(d2))[1] <- "No full ban zone"
      names(d2) <- "Threat index"
      return(d2)
   }
   else {
      if (kind %in% "source")
         d3 <- mapper(ursa_read(file.path("requisite",paste0(source,".tif"))),pal=pYl)
      else
         d3 <- mapper(a['1'],pal=pYl)
      d4 <- mapper(a['2'],pal=pRd)
   }
   dima <- c(length(ursa_colortable(d4)),length(ursa_colortable(d3)))
   cb <- colorbar2d(ifelse(kind %in% "source","RdBu","RdYl"),dim=dima)
   ursa_colortable(d3)[] <- cb[nrow(cb),]
   ursa_colortable(d4)[] <- rev(cb[,1])
   cb2 <- cb[rev(seq(dim(cb)[1])),]
   cb3 <- t(cb2)
   lut <- data.frame(d3=c(row(cb3))-1L,d4=c(col(cb3))-1L,pal=c(cb2)
                    ,ind=seq_along(c(cb2)),val=-1,desc="")
   lut$val <- lut$d3*nrow(cb)+lut$d4
   n3 <- names(ursa_colortable(d3))
   n4 <- names(ursa_colortable(d4))
   kwdY <- kwdRed
   kwdX <- ifelse(kind %in% "source","Source",kwdYellow)
   lut$desc <- paste0(kwdX,": ",n3[lut$d3+1L],", ",kwdY,": " ,n4[lut$d4+1L])
   d4a <- d4*1L
   d3a <- d3*length(ursa_colortable(d4))
   if (kind %in% c("mix","source")) {
      d5 <- d3a+d4a
   }
   else if (kind %in% c("overlap","threat")) {
      d5 <- d4a
      ind <- d5==0
      d5[ind] <- d3a[ind]
   }
   else if (kind %in% c("threat")) {
   }
   names(d5) <- kind
   if (F) {
      ta <- as.table(d5)
      ind <- match(as.integer(names(ta)),lut$val)
      lut <- lut[ind,]
      i4 <- which(lut$d3==0)
      p4 <- ursa_colortable(d4)[lut$d4[i4]+1L]
      lut$pal[i4] <- as.character(p4)
      lut$desc[i4] <- paste0(kwdRed,": ",names(p4))
      i3 <- which(lut$d4==0 & lut$d3>0)
      p3 <- ursa_colortable(d3)[lut$d3[i3]+1L]
      lut$pal[i3] <- as.character(p3)
      lut$desc[i3] <- paste0(kwdYellow,": ",names(p3))
   }
   d6 <- c(name=colorize(d5,value=lut$val,pal=lut$pal,name=lut$desc,stretch="category"))
   if (F) {
      d7 <- c(name=reclass(d5,src=lut$ind-1L,dst=lut$val))
      d7 <- colorize(d7,pal=lut$pal,name=lut$desc)
      cat("----------------\n")
      print(as.table(d5))
      cat("----------------\n")
      print(as.table(d6))
      cat("----------------\n")
      print(as.table(d7))
      cat("----------------\n")
      t5 <- sort(unname(as.table(d5)))
      t6 <- sort(unname(as.table(d6)))
      t7 <- sort(unname(as.table(d7)))
      print(identical(t5,t6))
      print(identical(t5,t7))
   }
   if (F) {
      print(as.table(d5))
      pac <- "PAC 7"
      p <- zoomToAOI("PAC 7")>0
      d5 <- regrid(d5,p)
      print(as.table(d5))
      d3 <- regrid(d3,p)
      d4 <- regrid(d4,p)
      display(list(d3,d4,d5,d6),las=1,layout=c(2,NA),bpp=8)
      print(lut)
      q()
   }
   session_grid(g0)
   return(d6)
}
'conflictBasemap' <- function(b) {
   if (missing(b)) {
     # b <- sf::st_cast(ursa:::spatialize(c(50,45,50,135),crs=4326),"POLYGON")
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=50,value=0),crs=4326)
      e <- spatial_transform(e,3575)
   }
   else {
      b <- spatial_transform(b,3575) ## 6931 of 'a'-rectangle is slight rotated to 3575 of 'b'
      u <- spatial_union(b)
      d <- spatial_buffer(spatial_centroid(u),sqrt(spatial_area(u)/pi)*1)
      xy <- spatial_coordinates(d)#[[1]][[1]]
      repeat({
         if (!is.list(xy))
            break
         xy <- xy[[1]]
      })
      xy <- xy[round(c(1,c(0.25,0.5,0.75)*nrow(xy))),]
      xy <- data.frame(xy[c(2,4),])
      colnames(xy) <- c("x","y")
      e <- ursa:::spatialize(xy,crs=spatial_crs(b))
   }
   #spatial_data(e) <- data.frame(id=seq(nrow(xy2)))
   m <- ursa:::polarmap(spatial_geometry(e),style="sdi",addFeature=F,opacity=0.7
                      # ,group="Basemap"
                       )
  # rm(u,d,e,xy)
   m
}
'conflictMap' <- function(a,aoi=NULL) {
   if (isShiny) {
      showNotification(id="leaflet",closeButton=FALSE,duration=120
                      ,"Rendering map...",type="warning")
      ursa:::.elapsedTime("render leaflet -- start")
   }
   if (!is.null(aoi))
      zoomToAOI(aoi)
   else
      session_grid(amountFile)
   g0 <- session_grid()
   if (onlyBasemap <- missing(a))
      b0 <- b <- polygonize(session_bbox(),engine="sf")
   else {
      aname <- names(a)
      names(a) <- "name"
      ct <- ursa_colortable(a)
      b <- polygonize(a,engine="sf")
      b0 <- polygonize(regrid(a),engine="sf")
      ##~ kwdX <- kwdYellow # "Conflict"
      ##~ kwdY <- kwdRed # "Incompatibility"
      pattKwd <- "^(\\S.+\\S):.+,\\s*(\\S.+\\S):.+$"
      kwdX <- unique(gsub(pattKwd,"\\1",names(ct)))
      kwdY <- unique(gsub(pattKwd,"\\2",names(ct)))
      ##~ cat("pattKwd:\n")
      ##~ str(pattKwd)
      ##~ cat("kwdX:\n")
      ##~ str(kwdX)
      ##~ cat("kwdY:\n")
      ##~ str(kwdY)
      ##~ cat("names(ct):\n")
      ##~ str(names(ct))
      if (length(kwdY)==1) {
         indX <- grep(paste0(kwdY,"\\w*(\\s|:\\s*)0(\\s|,|;|$)"),names(ct))
         ctX <- ct[indX]
         ##~ cat("ctX:\n")
         ##~ str(ctX)
         nameX <- gsub(paste0(".*",kwdX,"\\w*(\\s|:\\s*)(\\d.*)(,.+|;.+)"),"\\2",names(ctX))
         ##~ cat("nameX:\n")
         ##~ str(nameX)
      }
      else {
         ctX <- character()
         nameX <- character()
      }
      if (length(kwdY)==1) {
         indY <- grep(paste0(kwdX,"\\w*(\\s|:\\s*)0(\\s|,|;|$)"),names(ct))
         ctY <- ct[indY]
         ##~ cat("ctY:\n")
         ##~ str(ctY)
         nameY <- gsub(paste0(".*",kwdY,"\\w*(\\s|:\\s*)(\\d.*)($|,.+|;.+)"),"\\2",names(ctY))
         ##~ cat("nameY:\n")
         ##~ str(nameY)
      }
      else {
         ctY <- character()
         nameY <- character()
      }
      lX <- nameX
      nameX <- as.factor(nameX)
      nameX <- nameX[order(nameX)]
      levels(nameX) <- lX
      lY <- nameY
      nameY <- as.factor(nameY)
      nameY <- nameY[order(nameY)]
      levels(nameY) <- lY
      palX <- leaflet::colorFactor(palette=as.character(ctX),domain=nameX)
      palY <- leaflet::colorFactor(palette=as.character(ctY),domain=nameY)
      nameZ <- as.factor(names(ct))
      nameZ <- nameZ[order(nameZ)]
      levels(nameZ) <- names(ct)
     # nameZ <- factor(names(ct),ordered=TRUE)
     # str(nameZ)
     # print(nameZ)
     # q()
      palZ <- leaflet::colorFactor(palette=as.character(ct),domain=nameZ)
      b$ind <- match(b$name,names(ct))
      if (T) {
         if (T | !isShiny)
            ursa:::.elapsedTime("aggregate -- start")
         ongoing <- "aggregate -- start"
         b <- aggregate(b,by=list(b$ind),head,1)[,-1]
         if (T | !isShiny)
            ursa:::.elapsedTime("aggregate -- finish")
         ongoing <- "aggregate -- finish"
      }
      pal <- leaflet::colorFactor(palette=as.character(ct)
                                  ,domain=seq(length(ct)))
   }
   m <- conflictBasemap(b0)
   if (onlyBasemap)
      return(m)
   b <- spatial_transform(b,4326)
   gr <- "Map 3"
   m <- leaflet::addPolygons(m,data=b
                   ,color=~pal(ind)
                   ,weight=0
                  # ,popup=~gsub(";\\s*","\n",name)
                   ,label=~name
                   ,stroke=TRUE
                   ,fillOpacity=0.7
                   ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.7
                                                             # ,sendToBack=TRUE
                                                             # ,bringToFront=TRUE
                                                              )
                   ,group=gr
                   )
   if (length(nameX))
      m <- leaflet::addLegend(m
                    ,position="topleft"
                    ,pal=palX
                    ,values=nameX
                    ,opacity=0.6
                    ,title=kwdX
                    ,group=gr
                    )
   if (length(nameY))
      m <- leaflet::addLegend(m
                    ,position="topleft"
                    ,pal=palY
                    ,values=nameY
                    ,opacity=0.6
                    ,title=kwdY
                    ,group=gr
                    )
   if ((!length(nameX))&&(!length(nameY))) {
      m <- leaflet::addLegend(m
                    ,position="topleft"
                    ,pal=palZ
                    ,values=nameZ
                    ,opacity=0.6
                    ,title=aname
                    ,group=gr
                    )
   }
   if (!is.null(aoi)) {
      if (is.character(aoi)) {
        # pacvalue <- as.integer(gsub("\\D","",aoi))
         pac1 <- pac0[pac0==pacvalue]
         pac1 <- polygonize(pac1)
         pac1 <- spatial_union(pac1)
         
         pac1 <- regionU[[names(pac)]]
         pac1 <- pac1[pac1==pac]
         pac1 <- ursa_crop(pac1,border=1)
      }
      else if (is_ursa(aoi)) {
         pac1 <- polygonize(aoi)
         pac1 <- spatial_union(pac1)
         ta <- as.integer(names(as.table(aoi)))
         if ((length(ta)==1)&&(ta<95))
            aoi <- paste("PAC (raster value)",ta)
         else
            aoi <- "Custom selection"
      }
      if ((is_spatial(pac1))&&(is.character(aoi))) {
         pac1 <- spatial_transform(pac1,4326)
         m <- leaflet::addPolygons(m,data=pac1
                         ,color="#03F"
                         ,fillColor="transparent"
                         ,fill=FALSE
                         ,opacity=0.5
                         ,weight=3
                         ,popup=aoi
                        # ,label=~name
                         ,stroke=TRUE
                         ,fillOpacity=0.5
                         ,highlightOptions=leaflet::highlightOptions(weight=5
                                                                    ,opacity=0.75
                                                                    ,fill=!FALSE
                                                                    ,sendToBack=TRUE
                                                                    ,bringToFront=TRUE
                                                                    )
                         ,group=gr
                         )
      }
   }
   if (isShiny) {
      removeNotification(id="leaflet")
      ursa:::.elapsedTime("render leaflet -- finish")
   }
   m
}
'monther' <- function(s) {
   as.integer(format(as.Date(paste0("2021-",substr(s,1,3),"-15")
                    ,format="%Y-%b-%d"),"%m"))
}
'regroup' <- function(ta,kwd,digits=3) {
   apply(ta[,grep(kwd,colnames(ta),ignore.case=TRUE)],1,function(x) {
      y <- unique(unname(x))
      if (length(y)==1)
         return(sprintf(paste0("%.",digits-2,"f%%"),y*100))
      pref <- formattable::formatter("sup",style=function(x)
         formattable::style('padding-right'="0.1em"))(y[2])
     # sprintf(paste0("%.",digits,"f^%.",digits,"f^"),y[1],y[2])
      sprintf(paste0("^[%.",digits-2,"f%%]{style=\"padding-right:0.4em;color:#999;\"}^"
                    ,"%.",digits-2,"f%%"),y[2]*100,y[1]*100)
      
   })
}
'reformat' <- function(color="yellow",...) {
   formattable::formatter("span", style = function(x) {
     # str(strsplit(as.character(x),split="\\s*(\\(|\\)|\\^)\\s*"))
      y <- sapply(x,function(z) {
         if (z>1)
            z <- 1
         z
      })
      formattable::style('display'="inline-block"
           ,'direction'="rtl"
           ,'border-radius'="4px"
           ,'padding-right'="2px"
           ,'background-color'=formattable::csscolor(color)
           ,'text-align'="right"
           ,'width'=formattable::percent(y,1)
      )
   })
}
'human_use' <- function(cf=5011) {
   id <- as.integer(cf)
   activity <- readxl::excel_sheets(mdname)
   patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
   res <- lapply(activity,function(sheet) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      v[v$CF_code %in% id,]
   })
   if (F) {
      cname <- do.call(c,lapply(res,colnames))
      cname <- cname[!duplicated(cname)] |> tail(-4)
   }
   else
      cname <- unname(unlist(industries))
   da <- array(NA,dim=c(length(cname),12),dimnames=list(cname,month.abb))
   for (i in seq_along(res)) {
      v <- res[[i]]
      ind <- match(colnames(v),cname)
      j <- c(na.omit(ind))
      k <- which(!is.na(ind))
      mon <- lapply(v$Limitations,function(s) {
         s1 <- monther(gsub(patt,"\\1",s))
         s2 <- monther(gsub(patt,"\\2",s))
         s3 <- if (s1<s2) seq(s1,s2) else s3 <- c(seq(s1,12),seq(1,s2))
         s3
      })
      for (l in seq_along(mon)) {
         m <- mon[[l]]
         d <- t(v[l,k])
        # mode(d) <- "integer"
         if (T) {
            if (T)
               d2 <- as.character(d)
            else
               d2 <- kwdLUT[as.character(d)]
            dim(d2) <- dim(d)
            da[j,m] <- d2
         }
         else
            da[j,m] <- d
      }
   }
   ret <- cbind('Industry'=rownames(da),data.frame(da))
  # print(as.table(c(da)))
   ret
}

# mdname <- "./compatibility assessment_all_2021-04-05-fixed.xlsx"
mdname <- "requisite/compatibility assessment_all_2021-05-24-seasons.xlsx"
pGrYl <- cubehelix(5,light=91,dark=221,weak=220,rich=165,hue=2)
pYlRd <- cubehelix(5,light=91,dark=221,weak=110,rich=165,hue=2,inv=TRUE)
pRd <- cubehelix(5,light=233,dark=91,weak=110,rotate=0,hue=2)
pBl <- cubehelix(5,light=233,dark=91,weak=45,rotate=0,hue=0.2)
p4 <- cubehelix(5,light=221,dark=91,weak=110,rotate=0,hue=2)
p6 <- cubehelix(5,light=221,dark=91,weak=45,rotate=0,hue=2)
pRdT <- paste0(p4,format(as.hexmode(round(seq(15,255,length=length(p4)))),width=2,upper.case=TRUE))
pBlT <- paste0(p6,format(as.hexmode(round(seq(15,255,length=length(p6)))),width=2,upper.case=TRUE))
p5 <- cubehelix(5,light=221,dark=91,weak=165,rich=165,hue=2)
pYlT <- paste0(p5,format(as.hexmode(round(seq(15,255,length=length(p5)))),width=2,upper.case=TRUE))
#pGrYlRd <- c(pGrYl,tail(pYlRd,-1))
pBase <- c('green'="#068400",'yellow'="#fdfc00",'red'="#af0401",'extra'=c("#362978","#000000")[2])
pGrYlRd <- colorRampPalette(pBase[1:3])(9)
pRd <- paste0(pBase[3],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pBl <- paste0(pBase[4],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pGr <- paste0(pBase[1],format(as.hexmode(round(seq(15,255,length=5))),width=2,upper.case=TRUE))
pRd2 <- paste0(pBase[3],format(as.hexmode(round(seq(15,151,length=5))),width=2,upper.case=TRUE))
pBl2 <- paste0(pBase[4],format(as.hexmode(round(seq(15,151,length=5))),width=2,upper.case=TRUE))
pYlBase <- colorRampPalette(c(pBase[2],"#af9f00"))(5)
pYl <- paste0(pYlBase,format(as.hexmode(round(seq(15,255,length=length(pYlBase))))
                            ,width=2,upper.case=TRUE))
pYlRd <- pGrYlRd[c(5,8)]
height <- 600
retina <- 2
kwdRed <- "Incompatible"
kwdYellow <- c("Concessional","Conditional","Compatible under certain conditions")[2]
kwdGreen <- c("Compatible","Compatible/not applicable")[2]
allActivity <- "All human use"
##~ kwdRed <- "'2'"
##~ kwdYellow <- "'1'"
##~ kwdGreen <- "'0'"
kwdLUT <- c('0'=kwdGreen,'1'=kwdYellow,'2'=kwdRed)
listPD <- list.dirs(path="predefined",recursive=FALSE,full.names=TRUE)
basename(listPD)
regionSF <- vector("list",length(listPD))
names(regionSF) <- basename(listPD)
regionU <- regionSF
for (i in seq_along(listPD)) {
   reg <- ursa:::spatialize(spatial_dir(path=listPD[i]),engine="sf",crs=4326)
   if (length(ind <- which(sapply(as.list(spatial_data(reg)),class) %in% "character"))) {
      reg <- reg[spatial_fields(reg)[ind[1]]]
   }
   else {
      reg <- reg[spatial_fields(reg)[1]]
      if (length(ind <- grep("^PAC",colnames(reg)))) {
         reg[[ind]] <- paste0("PAC ",reg[[ind]])
      }
   }
   spatial_fields(reg) <- "region"
   reg <- reg[!is.na(reg$region),]
   regionSF[[i]] <- reg
}
session_grid(NULL)
dist2land <- ursa_read("requisite/dist2land-f.tif")
blank <- (!is.na(dist2land["ID"]))-1L
cell <- ursa(dist2land["dist"],"cell")*1e-3
rules <- jsonlite::fromJSON("requisite/buffer-rules.json")
sepRules <- " » "
sepRules <- iconv(sepRules,to="UTF-8")
pattRules <- paste0("^(.+\\S)",gsub("\\s","\\\\s",sepRules),"(\\S.+)$")
if (F)
   clf <- compose_coastline(spatial_transform(spatial_read("crop-coast.geojson"),6931)
                           ,fill="grey90",col="grey70")
# pac0 <- ursa:::.fasterize(pacSF) #"external/PACs33_Gridded_IDs.shp")
# ursa:::.elapsedTime("fasterize -- start")
for (i in seq_along(listPD)) {
   fileout <- file.path("predefined",names(regionSF)[i],"region")
   if (!envi_exists(fileout)) {
      regionU[[i]] <- ursa:::.fasterize(regionSF[[i]])
      ursa_write(regionU[[i]],fileout)
   }
   else
      regionU[[i]] <- read_envi(fileout)
}
# ursa:::.elapsedTime("fasterize -- finish")
#pacname
#regionSF[["PACs"]][[1]]
ongoing <- "This is verbatim info"
amountFile <- "requisite/amount"
cfmeta <- lapply(readxl::excel_sheets(mdname),function(sheet) {
   readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")[,1:2]
}) |> do.call(rbind,args=_) |> unique()
cfmeta <- cfmeta[with(cfmeta,order(CF_code,CF_name)),]
cfmeta$label <- paste0(cfmeta[["CF_code"]]," - ",cfmeta[["CF_name"]])
'industries' <- {
   activity <- readxl::excel_sheets(mdname)
   patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
   res <- lapply(activity,function(sheet) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      colnames(v)[-seq(1,4)]
   })
   names(res) <- activity
   res
}
# options(warn=10)
if (!isShiny) {
   aoi <- spatial_read("predefined/PACs/PACs33_Gridded_IDs.shp")[22,]
   aoi <- ursa:::.fasterize(aoi)
   a2 <- interim(activity=c("All human use","Tourism » Mass tourism")[2]
                ,aoi=aoi
                ,group="\\d",simplify="none")
   print(a2$map)
   str(a2$stat)
   print(summary(a2$stat$value1))
   print(table(a2$stat$flag))
  # display(a2$map)
}
