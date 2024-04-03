'quickload' <- function() { ## DEPRECATED
   home <- "."
   if (T) {
      source(file.path(home,"resources/global.R"))
      return(FALSE)
   }
   md5fname <- file.path(home,"quickload/_md5.csv")
   toWrite <- TRUE
   list1 <- dir(path=file.path(home,c("include","predefined","requisite","trafficlights"))
               ,pattern=".*",full.names=TRUE,recursive=TRUE)
   list1 <- c(list1,file.path(home,"resources/global.R"))
   list1 <- c(list1,file.path(home,"resources/process.R"))
   new <- tools::md5sum(list1)
   new <- data.frame(md5=unname(new),file=names(new))
   if (file.exists(md5fname)) {
      old <- read.csv(md5fname)
      if ((identical(new$file,old$file))&&(identical(new$md5,old$md5)))
         toWrite <- FALSE
   }
   if (toWrite) {
      source(file.path(home,"resources/global.R"))
      save.image(file=dummy_session_File)
      write.csv(new,md5fname,quote=FALSE,row.names=FALSE)

   }
   !toWrite
}
'mapper' <- function(obj,pal=NULL,ncolor=NULL) {
   devel2 <- FALSE
   if (is.null(pal)) {
     # pal <- cubehelix(5)
      pal <- pYlRd
      if (is.null(ncolor)) {
         if (isTRUE(ursa:::.is.integer(config$ncolor)))
            ncolor <- config$ncolor
         else
            ncolor <- 5
      }
      pal <- colorRampPalette(pal)(ncolor)
   }
   if (length(obj)>1)
      obj <- obj[1]
   if (band_blank(obj>0)) {
      return(colorize(obj,pal=pal))
   }
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
   tab2 <- as.table(b2)
   if ((n2 <- length(tab2))<n) {
      d1 <- colorize(obj,value=c(0,tab2)/mul,name=c(0,names(tab2)),pal=pal)
      return(d1)
   }
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
  # write_envi(d1,"C:/tmp/interim")
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
'zoomToAOI' <- function(pac) {
   if (F) {
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
   }
   else {
      return(NULL)
   }
   session_grid(pac1)
   pac1
}
'puAOI' <- function(aoi=NULL) {
   if (is.null(aoi))
      return(pu["ID"])
   crs1 <- spatial_crs(aoi)
   crs2 <- spatial_crs(pu)
  # print(list(crs1=crs1,crs2=crs2))
   if (identical(crs1,crs2)) {
      opW <- options(warn=-1)
      sf::st_crs(aoi) <- sf::st_crs(pu)
      options(opW)
   }
   else {
      aoi <- spatial_transform(aoi,pu)
   }
 #  sf::st_agr(aoi) <- "constant"
  # print(identical(sf::st_crs(aoi),sf::st_crs(pu)))
   b <- spatial_intersection(aoi,pu["ID"])["ID"]
   b <- b[spatial_area(b)>=half,]
   b <- pu[pu$ID %in% b$ID,"ID"]
   b
}
'map1_1d' <- function(a,grid=NULL,kind="devel",ncolor=NULL) {
   kind <- match.arg(kind)
   if (!is.null(grid))
      session_grid(grid)
   g0 <- session_grid()
   session_grid(a)
   d6 <- mapper(a,ncolor=ncolor)
   session_grid(g0)
   d6
}
'map2' <- function(a,grid=NULL,pac=NULL) {
   b3 <- a["3"]
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
   d3 <- mapper(a['2'],pal=pYl)
   d4 <- mapper(a['3'],pal=pRd)
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
   d3 <- mapper(a['2'])
   d4 <- mapper(a['3'])
   if (F & !isShiny) {
      print(d4)
      display(d4)
      display(a['3'])
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
  # d3 <- mapper(a['2'],pal=cb[nrow(cb),])
  # d4 <- mapper(a['3'],pal=rev(cb[,1]))
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
   if (devel3 <- FALSE) {
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
      b3 <- a['3']
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
         d3 <- mapper(a['2'],pal=pYl)
      d4 <- mapper(a['3'],pal=pRd)
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
'conflictBasemap' <- function(b,epsg=3575,group="Basemap") {
   crs <- as.integer(epsg) # sf::st_crs(3575)$proj4string  ## 6931 of 'a'-rectangle is slight rotated to 3575 of 'b'
   noAOI <- (missing(b))||(is.null(b))
   if (noAOI) {
     # b <- sf::st_cast(ursa:::spatialize(c(50,45,50,135),crs=4326),"POLYGON")
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=60,value=0),crs=4326)
      e <- spatial_transform(e,crs)
   }
   else {
      b <- spatial_transform(b,crs)
      if (F) {
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
      }
      else {
         xy <- matrix(sf::st_bbox(b),ncol=2,nrow=2
                     ,dimnames=list(NULL,c("x","y")),byrow=TRUE) |> as.data.frame()
      }
      colnames(xy) <- c("x","y")
      e <- ursa:::spatialize(xy,crs=spatial_crs(b))
   }
  # spatial_data(e) <- data.frame(id=seq(nrow(xy2)))
  # spatial_write(e,"C:/tmp/conflict.sqlite")
   m <- ursa:::polarmap(spatial_geometry(e),style="sdi",addFeature=F,opacity=0.7
                       ,group="Initial zoom"
                       )
  # rm(u,d,e,xy)
   m
}
'conflictMap' <- function(a,epsg=3575,aoi=NULL) {
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
      if (FALSE) {
         ctname <- names(ct)
         ctname <- gsub(kwdYellow,"yellow",ctname)
         ctname <- gsub(kwdRed,"red",ctname)
         ctname <- gsub(kwdGreen,"green",ctname)
         names(ct) <- ctname
      }
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
   m <- conflictBasemap(b0,epsg=epsg)
   if (onlyBasemap) {
      if (isShiny) {
         removeNotification(id="leaflet")
         ursa:::.elapsedTime("render leaflet -- finish")
      }
      return(m)
   }
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
      else if (is_spatial(aoi)) {
         pac1 <- spatial_union(aoi)
         aoi <- "AOI"
      }
      else {
         stop("'pac1' is undefined")
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
'CFMap' <- function(cf,epsg=3575) {
   if (missing(cf))
      return(conflictBasemap(epsg=epsg))
   if (is.character(cf)) {
      sp <- puvspr[puvspr$species %in% cf,]
      am <- pu[pu$ID %in% sp$pu,]
      spatial_data(am) <- sp["amount"]
   }
   else if (is_spatial(cf))
      am <- cf
   else {
     # return(conflictBasemap(am))
      stop(paste("Unhandled: class of 'cf'-argument is",class(cf)))
   }
   am <- spatial_transform(am,4326)
   ct <- colorize(am$amount,value=seq(0,1,by=0.2))
   am$value <- names(ct$colortable)[ct$index] |> factor()
   ct <- ursa_colortable(ct)
   ta <- table(am$value)
   pal <- colorFactor(palette=as.character(ct)
                     ,domain=names(ct)
                     )
   ursa:::.elapsedTime(paste0("conflictBasemap(",cf,")"))
   m <- conflictBasemap(am,epsg=epsg)
   m <- addPolygons(m,data=spatial_transform(am,4326)
                   ,color=~pal(value)
                   ,weight=0
                  # ,popup=~sum
                   ,label=~value
                   ,stroke=TRUE
                  # ,weight=0.5
                   ,fillOpacity=0.7
                  # ,highlightOptions=highlightOptions(fillOpacity=0)
                   ,group="CF overlay"
                   )
   m <- addLegend(m
                 ,position="topleft"
                 ,pal=pal
                 ,values=names(ct)
                 ,opacity=0.7
                 ,title="Amount"
                 ,group="CF overlay"
                 )
  # spatial_write(am,"c:/tmp/am.sqlite")
   if (F) {
      m <- addFeatures(m,am
                      ,label=cf
                      ,color="#092B"
                     # ,layerId=~seq(spatial_count(regionSF[[input$region]]))
                      ,group="CF overlay"
                      )
   }
   m <- addLayersControl(m
                        ,overlayGroups=c(NULL
                                        ,c("Basemap","CF overlay")
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
  # saveRDS(m,"c:/tmp/leafletCF.rds")
   m
}
'regionMap_deprecated' <- function(aoi=NULL,showPAs=FALSE) {
  # showPAs <- FALSE
   if (missing(aoi))
      return(conflictBasemap(stop("epsg?")))
   showAOI <- is_spatial(aoi)
  # if (is.null(aoi))
  #    return(conflictBasemap())
   if (showAOI) {
      ursa:::.elapsedTime("0904a1")
      aoi <- aoi |> puAOI() |> spatial_union() |> spatial_transform(4326)
      ursa:::.elapsedTime("0904b1")
     # aoi <- spatial_transform(spatial_union(aoi),4326)
     # aoi <- spatial_transform(aoi,4326)
      m <- conflictBasemap(aoi,stop("epsg?"))
   }
   else
      m <- conflictBasemap(stop("epsg?"))
   ursa:::.elapsedTime("0904c1")
   grAOI <- "Selected Region(s)"
   grPAs <- "Existing Protected Areas"
   colAOI <- "#092B"
   colPAs <- "#992B"
   if (showAOI) {
      m <- leaflet::addPolygons(m,data=aoi
                      ,label=grAOI
                      ,color=colAOI
                     # ,weight=0
                     # ,popup=~gsub(";\\s*","\n",name)
                     # ,stroke=TRUE
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                # ,sendToBack=TRUE
                                                                # ,bringToFront=TRUE
                                                                 )
                      ,group=grAOI
                      )
      ursa:::.elapsedTime("0904f1")
   }
   if (showPAs) {
      opW <- options(warn=1) ## sf::sf_extSoftVersion() - old GDAL?
      m <- leaflet::addPolygons(m,data=PAs
                      ,label=grPAs
                      ,color=colPAs
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                 )
                      ,group=grPAs
                      )
      options(opW)
      ursa:::.elapsedTime("0904g1")
   }
   if (showAOI)
      m <- leaflet::addLegend(m
                    ,position="bottomleft"
                    ,colors=colAOI
                    ,opacity=0.2
                    ,labels=grAOI
                    ,group=grAOI
                    )
   if (showPAs)
      m <- leaflet::addLegend(m
                    ,position="bottomleft"
                    ,colors=colPAs
                    ,opacity=0.2
                    ,labels=grPAs
                    ,group=grPAs
                    )
   m <- addLayersControl(m
                        ,overlayGroups=c(NULL
                                        ,"Basemap"
                                        ,if (showPAs) grPAs
                                        ,if (showAOI) grAOI
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
  # if (T | showPAs)
  #    m <- hideGroup(m,grPAs)
   ursa:::.elapsedTime("0904h1")
   m
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
'.human_use.deprecated' <- function(cut=5011,verbose=FALSE) {
   opW <- options(warn=-1)
   id <- as.integer(gsub("^(\\d{4}).*","\\1",cut))
   options(opW)
   isCF <- !is.na(id)
   isIndustry <- !isCF
   cat(paste0("human_use(",sQuote(cut),"):\n"))
   if (verbose)
      print(c(CF=isCF,industry=isIndustry))
   if (isCF) {
      activity <- readxl::excel_sheets(mdname)
   }
   else {
      activity <- unlist(industries)[match(cut,unlist(industries))]
      activity <- gsub("\\d+$","",names(activity))
   }
   patt <- "(^\\w+)\\s*-\\s*(\\w+$)"
   res <- lapply(activity,function(sheet) {
      v <- readxl::read_excel(mdname,sheet=sheet,.name_repair="minimal")
      v <- v[!is.na(v$CF_code),]
      if (isCF)
         return(v[v$CF_code %in% id,])
      vname <- colnames(v)
      ind1 <- which(!vname %in% unlist(industries))
      ind2 <- which(vname %in% cut)
      ind <- c(ind1,ind2)
      v[,ind]
   })
  # str(res)
   if (isCF) {
      cname <- unname(unlist(industries))
   }
   else {
      r <- res[[1]]
      cname <- paste0(r$CF_code,sepRules,r$CF_name)
   }
   da <- array(NA,dim=c(length(cname),12),dimnames=list(cname,month.abb))
   for (i in seq_along(res)) {
      v <- res[[i]]
      if (isCF)
         ind <- match(colnames(v),cname)
      else
         ind <- match(colnames(v),cut)
      j <- c(na.omit(ind))
      k <- which(!is.na(ind))
      mon <- lapply(v$Limitations,monthList)
      for (l in seq_along(mon)) {
         m <- mon[[l]]
         ##~ cat("----------\n")
         ##~ str(j)
         ##~ str(k)
         ##~ str(l)
         ##~ str(m)
         d <- t(v[l,k])
        # mode(d) <- "integer"
         if (T) {
            if (T)
               d2 <- as.character(d)
            else
               d2 <- kwdLUT[as.character(d)]
            d2[is.na(d2)] <- "n/a"
            dim(d2) <- dim(d)
            if (isCF)
               da[j,m] <- d2
            else
               da[l,m] <- d2
         }
         else
            da[j,m] <- d
      }
   }
   if (isCF) {
      cname <- rownames(da)
      abbr <- industryAbbr$abbr[match(cname,industryAbbr$industry)]
      ret <- cbind('Industry'=cname,data.frame(da))
      rownames(ret) <- abbr
   }
   else {
      CF_code <- gsub(pattRules,"\\1",rownames(da))
      CF_name <- gsub(pattRules,"\\2",rownames(da))
      ret2 <- by(da,CF_code,simplify=FALSE,function(x) {
         x2 <- apply(x,2,function(x) {
            y <- na.omit(x)
            if (!length(y))
               return(rep(NA_character_,1))
            if (length(y)>1)
               print(x)
            c(y)
         })
         as.data.frame(t(x2))
      })
      ret2 <- do.call(rbind,ret2)
      ind <- match(rownames(ret2),CF_code)
      ret <- cbind('CF Code'=CF_code[ind],'CF Name'=CF_name[ind],ret2)
      rownames(ret) <- NULL
   }
  # print(as.table(c(da)))
   ret
}
'human_use' <- function(cut=5011,verbose=FALSE) {
   opW <- options(warn=-1)
   id <- as.integer(gsub("^(\\d{4}).*","\\1",cut))
   options(opW)
   isCF <- !is.na(id)
   isIndustry <- !isCF
   cat(paste0("human_use(",sQuote(cut),"):\n"))
   if (verbose)
      print(c(CF=isCF,industry=isIndustry))
   if (isCF) {
      da <- concern[concern$CF_code==cut,]
      da <- by(da,list(da$industry,da$month),\(x) trafficValue(x$value))[]
   }
   else {
      da <- concern[concern$industry==industryCode(cut),]
      da <- by(da,list(da$CF_code,da$month),\(x) trafficValue(x$value))[]
   }
   if (isCF) {
      if (!is.null(iceCover)) {
         ice <- iceCover[iceCover$CF==id,3:ncol(iceCover)] |> as.matrix()
         ice[] <- sprintf("%0.2f",ice)
      }
      else if (F) {
         ice <- lapply(rownames(da),\(industry) {
           # print(data.frame(CF=id,industry=industry))
            iceConcCover(CF=id,industry=industry)$concern$available
         }) |> do.call("rbind",args=_)
         ice[] <- sprintf("%0.2f",ice)
         str(ice)
      }
   }
   else {
      if (!is.null(iceCover)) {
         ice <- iceCover[iceCover$industry==industryCode(cut)
                        ,3:ncol(iceCover)] |> as.matrix()
         ice[] <- sprintf("%0.2f",ice)
      }
   }
   da[] <- as.character(da)
   da[is.na(da)] <- "n/a"
   if (!is.null(iceCover))
      da[] <- paste0(ice,"<sup>",as.character(da),"</sup>")
   colnames(da) <- month.abb[as.integer(colnames(da))]
   if (isCF) {
      cname <- rownames(da)
      ret <- cbind('Industry'=industryName(rownames(da)),as.data.frame(da))
   }
   else {
      aname <- rownames(da)
      ret <- cbind('CF Code'=CFCode(aname),'CF Name'=CFName(aname),as.data.frame(da))
      rownames(ret) <- NULL
   }
   ret
}
'groupCF' <- function(group) {
   if (missing(group))
      res <- taxonCF$CF_code
   else if (is.null(group))
      res <- taxonCF$CF_code
   else if (any(ind <- group %in% taxonCF$CF_code))
      res <- group[ind]
   else if (any(!(group %in% nameAllCF))) {
      ind <- lapply(grep("^group",colnames(taxonCF)),\(g) {
         which(taxonCF[[g]] %in% group)
      })
     # ind <- ind |> unlist() |> unique()
      ind2 <- which(sapply(ind,\(x) length(x)>0))[1]
      ind <- ind[[ind2]]
      res <- taxonCF$CF_code[ind]
   }
   else #if (any(group %in% nameAllCF))
      res <- taxonCF$CF_code
   as.integer(res[res %in% CFCode()])
}
'group3ListCF' <- function() {
   c(nameAllCF['3'],unique(taxonCF$group3[taxonCF$CF_code %in% unique(concern$CF_code)]))
}
'crossTable' <- function(aoi=NULL,group=NULL,activity=NULL,season=NULL
                        ,epoch=NULL,minCover=0,CAP=!isShiny,verbose=FALSE) {
   if (isShiny)
      cat("crossTable:\n")
  # if (T & is.null(aoi))
  #    return(NULL)
   #aoi <- NULL
   listCF <- groupCF(group)
   if (any(nameAllCF['3'] %in% group))
      group <- NULL
   if (is.null(group))
      group <- as.character(groupCF())
   else if (is.numeric(group))
      group <- as.character(group)
   b <- puAOI(aoi)
   if (!isShiny & verbose) {
      cat("-crosstable----------\n")
      str(aoi)
      str(b)
      cat("-crosstable----------\n")
   }
   am <- puvspr[puvspr$pu %in% b$ID,]
   cf1 <- am$species |> sort() |> unique()
   cf2 <- concern$CF_code  |> sort() |> unique()
   am2 <- by(am,am$species,function(x) {
      data.frame(cf=x$species[1],amount=sum(x$amount))
   }) |> do.call(rbind,args=_)
   am2$amount <- am2$amount/spec[spec$cf %in% am2$cf,"amount"]*100
   if (length(group)) {
      ind <- which(am2$cf %in% groupCF(group))
      if (length(ind)) {
         am2 <- am2[ind,]
      }
      else {
         ng <- unique(nchar(group))
         stopifnot('Mixed groups are not supported'=length(ng)==1)
         gr2 <- substr(as.character(am2$cf),1,ng)
         ind <- lapply(group,function(patt) grep(patt,gr2)) |> do.call(c,args=_) |> unique()
         am2 <- am2[ind,]
      }
   }
   am2 <- am2[am2$cf %in% unique(concern$'CF_code'),]
   if (!nrow(am2))
      return(data.frame())
   am2 <- am2[order(am2$amount,decreasing=TRUE),]
   if (!isShiny & verbose)
      print(head(am2,12),digits=3)
   if (F) { ## deprecated
      vuln2 <- vulner[vulner$CF_code %in% am2$cf,]
      rule2 <- rules
      if ((length(activity))&&(all(activity %in% names(industries)))) {
         act2 <- gsub(pattRules,"\\1",vuln2$industry)
         vuln2 <- vuln2[which(act2 %in% activity),]
         act2 <- gsub(pattRules,"\\1",rule2$unique)
         rule2 <- rule2[which(act2 %in% activity),]
      }
      rname <- as.character(am2$cf)
      res <- lapply(rule2$activity |> sample(),function(industry) {
         vuln3 <- vuln2[vuln2$industry %in% industry,]
         if (!nrow(vuln3))
            return(NULL)
         b <- by(vuln3$value,vuln3$CF_code,function(x) {
            ret <- paste(sort(trafficValue(unique(x)),decreasing=TRUE),collapse="/")
            ret[!nchar(ret)] <- "n/a"
            ret
           # max(x)
         }) |> c()
         ret <- rep(NA,length(am2$cf))
         names(ret) <- rname
         ret[match(names(b),rname)] <- b
         dim(ret) <- c(1,length(ret))
         rownames(ret) <- industry
         ret
      }) |> do.call(rbind,args=_) |> t()
      rownames(res) <- rname
      res <- res[!apply(res,1,function(x) all(is.na(x))),]
   }
   else {
      con2 <- concern
      con2 <- con2[con2$CF_code %in% am2$cf,]
      if (!is.null(season)) {
         if (is.numeric(season))
            m <- as.integer(season)
         else {
            m <- na.omit(match(season,tail(seasonList,-1)))
            if (!length(m))
               m <- na.omit(match(substr(season,1,3),substr(tail(seasonList,-1),1,3)))
         }
         if (length(m))
            con2 <- con2[con2$month %in% m,]
      }
      if (!is.null(activity)) {
         iname <- character()
         ind1 <- match(activity,industryAbbr$industry) |> na.omit() |> c()
         ind2 <- match(activity,industryAbbr$abbr) |> na.omit() |> c()
         ind3 <- match(activity,names(industries)) |> na.omit() |> c()
         if (length(ind1))
            iname <- c(iname,industryCode(activity))
         if (length(ind2))
            iname <- c(iname,activity)
         if (length(ind3))
            iname <- c(iname,industryCode(unlist(industries[activity])))
         if (length(iname))
            con2 <- con2[con2$industry %in% iname,]
      }
      con2 <- by(con2,list(CF_code=con2$CF_code,industry=as.character(con2$industry)),\(x) {
         y <- na.omit(x$value)
         ret <- paste(sort(trafficValue(unique(y)),decreasing=TRUE),collapse="/")
         ret[!nchar(ret)] <- "n/a"
         ret
      })
      res <- unclass(con2) #|> data.frame()
     # print(res[])
     # print(am2$cf)
      ind <- match(am2$cf,rownames(res)) |> na.omit()
      colnames(res) <- industryName(colnames(res))
      res <- res[ind,,drop=FALSE]
   }
   res <- data.frame(res,check.names=FALSE)
   cname <- gsub(pattRules,"\\2",colnames(res)) |> substr(1,300) #|> abbreviate(24)
   ind <- match(industryAbbr$industry,cname) |> na.omit() |> c()
   cname <- cname[ind]
   res <- res[,ind,drop=FALSE]
   colnames(res) <- cname
   indName <- match(rownames(res),scenarioCF[[indCFcode]])
   indCF <- match(rownames(res),as.character(am2$cf))
   res <- cbind('CF name'=scenarioCF[[indCFname]][indName]
               ,'Cover'=am2$amount[indCF]
               ,'CAP'=NA
               ,'NAO'=NA
               ,'NAC'=NA
               ,res)
   res <- res[res$Cover>=minCover,]
   if (is.null(season))
      season <- tail(seasonList,-1)
   attr(res,"industry") <- industryCode(colnames(res)) |> na.omit() |> c()
  # attr(res,"group") <- if (T | is.null(group)) rownames(res) else group
   attr(res,"group") <- listCF
   attr(res,"season") <- if (is.numeric(season)) as.integer(season) else season
   con2 <- concernSubset(concern,ctable=res)
   ic <- concernIndex(con2,aoi=NULL) ## concern con2
   sumNAC <- ic$sumNAC
   sumNAO <- ic$sumNAO
   indNAI <- match(rownames(res),names(sumNAC))
   res$NAO <- sumNAO[indNAI]*100
   res$NAC <- sumNAC[indNAI]*100
  # a <- sapply(ls(),\(x) object.size(get(x)))
   rm(con2)
   ursa:::.gc(isShiny)
   if (CAP) {
      ##~ actionPriority <- regionActionPriority(aoi=aoi
                         ##~ ,ctable=res # res[,-grep("CAPR",colnames(res))]
                         ##~ ,epoch=epoch,maxNAC=FALSE)
      actionPriority <- regionActivityIndices(aoi=aoi
                         ,ctable=res # res[,-grep("CAPR",colnames(res))]
                         ,epoch=epoch)
      metrics <- 100*actionPriority$CAA$capCF/actionPriority$CAB$capCF
      metrics[is.na(metrics)] <- 0
      ind <- match(rownames(res),names(metrics))
      res$'CAP'[!is.na(ind)] <- metrics[na.omit(ind)]
   }
   res$'NAO'[is.na(res$'NAO')] <- 0
   res$'NAC'[is.na(res$'NAC')] <- 0
   res[is.na(res)] <- "n/a"
  # print("crossTable is OK")
   res
}
'conditionMap' <- function(industry="Mass tourism",group="Cetacean",season="max"
                          ,economy=NULL,epoch=NULL) {
   verbose <- !FALSE
   sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%B")
   byMonths <- (length(season))&&(all(season %in% sname))
   if (!byMonths)
      season <- sname
   if (any(industry %in% names(industries)))
      industry <- unlist(na.omit(industries[industry]))
   industry <- industryCode(industry)
   rule <- rules[rules$abbr %in% industry,]
   rule$manual <- rule$unique <- rule$activity <- rule$industry <- NULL
  # rule$industry <- substr(rule$industry,1,16)
   r <- rule
   colnames(r)[grep("^abbr",colnames(r))] <- "industry"
  # print(r)
   prm <- list(industry=sort(unname(industry))
             # ,group=sort(unname(group))
              ,group=sort(groupCF(group))
             # ,season=sname[sort(match(season,sname))]
              ,season=sort(match(season,sname))
              ,economy=sort(unname(economy))
              ,epoch=epoch
              )
   str(prm)
  # stop("0331")
   fname <- file.path(root,"requests",paste0("u",digest::digest(prm,"crc32"),c(".rds",".tif")))
   if (file.exists(fname[2])) {
      cat(paste0("conditionMap: read from cache ",basename(fname[2]),"\n"))
      session_grid(NULL)
      return(ursa_read(fname[2]))
   }
   else {
      if (T & staffOnly)
         saveRDS(prm,fname[1])
      cat(paste0("conditionMap: new request ",basename(fname[2]),"\n"))
   }
   if (T) {
      if (isVulner <- exists("vulner"))
         nchunk <- 1L
      else
         nchunk <- length(vulnerFile)
      isSubreg <- isTRUE(economy %in% c("nacr","naor","mnsr","sr"))
      if (!isTRUE(economy %in% c("capr","humanuse"))) {
         if (isShiny)
            shiny::showNotification(id="concernMap",closeButton=FALSE,duration=120
                            ,if (!staffOnly) "Processing and rendering" else 
                             paste("'concernmap' request"
                                  ,dQuote(gsub("\\..+$","",basename(fname[2])))
                                  ,"is at first time."
                                  ,"Please wait his finalizing."
                                  ,"It will be processed faster next time...")
                            ,type="warning")
         v <- with(concern,aggregate(list(value=value)
                            ,by=list(species=CF_code,industry=industry),function(x) {
            x <- na.omit(x)
            if (!length(x))
               return(NA)
            max(x)
         }))
         res3 <- lapply(seq(nchunk),\(i) {
            if (!isVulner)
               res2 <- readRDS(vulnerFile[i])
            else
               res2 <- vulner
            n1 <- nrow(res2)
            if (length(prm$industry)<nrow(rules))
               res2 <- res2[res2$industry %in% prm$industry,]
            if (!is.null(group))
               res2 <- res2[res2$species %in% prm$group,]
            ind <- match(with(res2,paste0(species,industry))
                        ,with(v,paste0(species,industry)))
            res2$value <- v$value[ind]
            if (isSubreg) {
               if (nrow(res2)==n1)
                  return(NULL)
               return(with(res2,paste0(pu,species)))
            }
            by(res2,list(pu=res2$pu,value=res2$value),\(x) sum(x$amount))[]
         })
         ursa:::.gc(TRUE)
         if (isSubreg) {
            res3 <- lapply(res3,\(x) x) |> do.call(c,args=_)
         }
         else
            res3 <- lapply(res3,\(x) x) |> do.call(rbind,args=_)
         if (isShiny)
            removeNotification(id="concernMap")
      }
   }
   else { ## to deprecate
      jname <- file.path(root,"requests",paste0("j",digest::digest(prm,"crc32"),".rds"))
      prm_download(jname)
      if (file.exists(jname)) {
         res2 <- readRDS(jname)
      }
      else {
         if (isShiny)
            shiny::showNotification(id="concernMap",closeButton=FALSE,duration=120
                            ,if (!staffOnly) "Processing and rendering" else
                             paste("'concernmap' request"
                                  ,dQuote(gsub("\\..+$","",basename(fname[2])))
                                  ,"is at first time."
                                  ,"Please wait his finalizing."
                                  ,"It will be processed faster next time...")
                            ,type="warning")
         if (verbose) {
            cat("r:\n")
            str(r)
         }
         v <- concernSubset(concern,activity=prm$industry,group=prm$group,season=prm$season)
         v <- v[!is.na(v$value),]
         v <- aggregate(list(value=v$value)
                       ,by=list(CF_code=v$CF_code
                               ,industry=v$industry
                              # ,value=v$value
                              # ,month=v$month
                               )
                       ,function(x) {
            x <- na.omit(x)
            if (!length(x))
               return(NA)
            max(x)
         })
         colnames(v)[grep("^CF",colnames(v))] <- "species"
         v$industry <- factor(v$industry,levels=levels(concern$industry))
         if (verbose) {
            cat("v:\n")
            str(v)
         }
         if (F) {
            ind <- match(v$industry,rule$abbr)
            v$minCoast <- rule$minCoast[ind]
            v$maxCoast <- rule$maxCoast[ind]
            v$minDepth <- rule$minDepth[ind]
            v$maxDepth <- rule$maxDepth[ind]
            str(v)
         }
         w <- spatial_data(pu)
         colnames(w)[grep("^ID",colnames(w))] <- "pu"
         w <- w[,c("pu","coast","depth")]
         if (verbose) {
            cat("w:\n")
            str(w)
         }
         u <- puvspr[,c("species","pu","amount")]
         if (verbose) {
            cat("u:\n")
            str(u)
         }
         on.exit(ursa:::.elapsedTime("complete"))
         ursa:::.elapsedTime("w+r")
         res1 <- merge(w,r)
         rm(w,r)
         ursa:::.gc(TRUE)
         print("0701a")
        # str(res1)
        # print(object.size(res1))
         if (TRUE) ## `FALSE` should be identical to `interimMap()`
            res1 <- res1[res1$depth<=res1$maxDepth & res1$depth>=res1$minDepth &
                         res1$coast>=res1$minCoast*cell & res1$coast<=res1$maxCoast*cell,]
         print("0701b")
        # str(res1)
        # print(object.size(res1))
        # res1 <- res1[,grep("^(min|max)",colnames(res1),invert=TRUE)]
         print("0701c")
         str(res1)
         res1 <- res1[,c("pu","industry")]
         print("0701d")
         res1$industry <- factor(res1$industry,levels=levels(concern$industry))
         print("0701e")
        # cat("res1:\n")
        # str(res1)
        # print(object.size(res1))
         ursa:::.elapsedTime("u+v")
         print("0701f")
         res2 <- merge(u,v)
         rm(u,v)
         ursa:::.gc(TRUE)
         ursa:::.elapsedTime("u+v merged")
         res2 <- res2[with(res2,paste0(industry,pu)) %in% with(res1,paste0(industry,pu)),]
         print("0701g 'merge(u,v)'")
         rm(res1)
         ursa:::.elapsedTime("u+w+v+r -- done")
         saveRDS(res2,jname)
         if (file.exists(jname))
            prm_upload(jname)
         if (isShiny)
            removeNotification(id="concernMap")
      }
     # res2$concern <- mulNA[res2$value]
      cat("res2:\n")
      str(res2)
      res3 <- res2
   }
   if (isTRUE(economy %in% c("trafficlight")))
      economy <- NULL
   if (!is.null(economy)) {
     # subreg <- with(vulner,paste0(pu,species))
      if (isTRUE(economy=="capr")) {
         ret <- indexCAPR(group=group,activity=industry,season=season,epoch=epoch)
         names(ret)[1] <- "OIP-P"
      }
      else if (isTRUE(economy %in% c("mnsr","nacr"))) {
         cat("0331a --------------\n")
         str(res3)
         cat("0331b --------------\n")
        # str(list(group=group,activity=industry,season=season))
         ret <- indexNACR(group=group,activity=industry,season=season,subreg=res3)
         names(ret)[1] <- "OC-P"
      }
      else if (isTRUE(economy %in% c("sr","naor"))) {
         ret <- indexNAOR(group=group,activity=industry,season=season,subreg=res3)
         names(ret)[1] <- "SC-P"
      }
      else if (isTRUE(economy=="humanuse")) {
         ret <- indexHumanUse(group=group,activity=industry,season=season,epoch=epoch)
         names(ret) <- "Amount"
      }
      else {
         stop(paste("unknown 'economy':",dQuote(economy)))
      }
      opW <- options(warn=-1)
      ursa_write(ret,fname[2])
      options(opW)
      if (file.exists(fname[2]))
         prm_upload(fname[2])
     # print(ret)
     # display(ret,stretch="eq")
     # q()
      return(ret)
   }
  # print(object.size(res2))
   ursa:::.elapsedTime("sum amount")
   ursa:::.gc(TRUE)
   print("1023l")
   ursa:::.elapsedTime("assigned value")
  # str(pu)
   ind <- match(rownames(res3),pu$ID)
   res4 <- pu[ind,]
   res3[is.na(res3)] <- 0
   spatial_data(res4) <- data.frame(res3,check.names=FALSE)
  # res4$pu <- pu$ID[ind]
   str(res4)
  # session_grid(blank)
   res5 <- allocate(spatial_centroid(res4),resetGrid=TRUE)
   m <- (sum(res5,cover=0)>0)-1
   res5[is.na(res5)] <- m
   opW <- options(warn=-1)
   ursa_write(res5,fname[2])
   options(opW)
   if (file.exists(fname[2]))
      prm_upload(fname[2])
   res5
}
'interimMap_deprecated' <- function(industry="Mass tourism",group="\\d",season="max"
                                   ,economy=NULL,epoch=NULL) {
   sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%B")
   byMonths <- (length(season))&&(all(season %in% sname))
   if ((group[1]=="\\d")&&(grepl("max",season[1],ignore.case=TRUE))&&(is.null(economy))) {
      fname <- file.path("trafficlights",paste0("t",digest::digest(unname(industry),"crc32")))
      if (isShiny)
         print(basename(fname))
      if (envi_exists(fname)) {
         session_grid(NULL)
         return(read_envi(fname))
      }
   }
   cat("-- interimMap -- begin --------------\n")
   cat("-- interimMap -- end ----------------\n") |> on.exit()
   if (any(industry %in% names(industries)))
      industry <- unlist(na.omit(industries[industry]))
   industry <- industryCode(industry)
   prm <- list(industry=sort(unname(industry))
              ,group=sort(unname(group))
              ,season=sname[sort(match(season,sname))]
              ,economy=sort(unname(economy)))
   str(prm)
   fname <- file.path(root,"requests",paste0("t",digest::digest(prm,"crc32"),".tif"))
   prm_download(fname)
   if (file.exists(fname)) {
      cat(paste0("interimMap: read from cache ",basename(fname),"\n"))
      session_grid(NULL)
      return(ursa_read(fname))
   }
   else
      cat(paste0("interimMap: new request ",basename(fname),"\n"))
   if (isShiny)
      shiny::showNotification(id="trafficMap",closeButton=FALSE,duration=120
                      ,if (!staffOnly) "Processing and rendering" else
                       paste("'trafficlight' request"
                            ,dQuote(gsub("\\..+$","",basename(fname)))
                            ,"is at first time."
                            ,"Please wait his finalizing."
                            ,"It will be processed faster next time...")
                      ,type="warning")
   ursa:::.elapsedTime("aggregation -- start")
   if (T) {
      cat("via concern:\n")
      v <- concern
      v <- v[lapply(group,grep,substr(as.character(v$CF_code),1,1)) |>
         do.call(c,args=_) |> sort(),]
      v <- v[v$industry %in% industry,]
      if (nrow(v)) {
         if (byMonths)
            v <- v[v$month %in% match(season,sname),]
         if (!((byMonths)&&(length(season)==1))) {
           # ursa:::.elapsedTime("aggregate -- start") ## it's quick
            v <- aggregate(list(value=v$value)
                          ,by=list(CF_code=v$CF_code
                                  ,industry=v$industry
                                 # ,value=v$value
                                 # ,month=v$month
                                  )
                          ,function(x) {
               x <- na.omit(x)
               if (!length(x))
                  return(NA)
               max(x)
            })
           # ursa:::.elapsedTime("aggregate -- finish")
         }
         s <- aggregate(list(count=v$CF_code),by=list(CF_code=v$CF_code,value=v$value),length)
      }
      else {
         s <- NULL
      }
   }
   else { ## deprecated
      cat("via vulner:\n")
     # print(industry)
     # activity <- names(industries)[sapply(industries,function(a) industry %in% a)]
     # industry <- paste0(activity,sepRules,industry)
     # v <- v[grep("^Tourism",v$industry),]
     # v <- v[v$industry %in% "Tourism » Mass tourism",]
     # v <- vulner[grep(industry,vulner$industry,ignore.case=TRUE),]
      v <- vulner[which(!is.na(match(gsub(pattRules,"\\2",vulner$industry),industry))),]
      v <- aggregate(list(value=v$value)
                    ,by=list(CF_code=v$CF_code,industry=v$industry),max)
      s <- aggregate(list(count=v$CF_code),by=list(CF_code=v$CF_code,value=v$value),length)
   }
   ursa:::.elapsedTime("aggregation -- finish")
   g0 <- session_grid("requisite/amount")
   if (isTRUE(economy=="capr")) {
      res2 <- indexCAPR(group=group,activity=industry,season=season,epoch=epoch)
      print(res2)
      names(res2) <- "OIP-P"
      res2 <- polygonize(res2)
   }
   else if (isTRUE(economy=="nacr")) {
      str(list(group=group,activity=industry,season=season))
      res2 <- indexNACR(group=group,activity=industry,season=season)
      names(res2) <- "OC-P"
      res2 <- polygonize(res2)
   }
   else if (isTRUE(economy=="naor")) {
      res2 <- indexNAOR(group=group,activity=industry,season=season)
      names(res2) <- "SC-P"
      res2 <- polygonize(res2)
   }
   else if (isTRUE(economy=="humanuse")) {
      res2 <- indexHumanUse(group=group,activity=industry,season=season)
      res2 <- polygonize(res2)
   }
   else if (is.null(s)) {
      ind <- match(economy,spatial_basename(economyList))
      if (is.na(ind))
         stop(paste("cannot identify file name for",economy))
      feconomy <- economyList[ind]
      print(feconomy)
      res2 <- spatial_read(file.path(".",feconomy))
   }
   else {
      rule <- rules[rules$abbr %in% industry,]
      ##~ rule$unique <- NULL
      ##~ rule$manual <- NULL
      ##~ rule$industry <- substr(rule$industry,1,16)
      ##~ print(rule)
      ursa:::.elapsedTime("incompatibility -- start")
     # pu2 <- pu[pu$depth<=rule$maxDepth & pu$depth>=rule$minDepth &
     #           pu$coast>=rule$minCoast*cell & pu$coast<=rule$maxCoast*cell,]
      pu2 <- pu
      rel <- puvspr[puvspr$pu %in% pu2$ID,]
      res <- by(rel,rel$pu,function(x) {
         ret <- c('1'=0,'2'=0,'3'=0)
         ind <- match(s$CF_code,x$species)
         if (!length(na.omit(ind)))
            return(ret)
         s2 <- s[!is.na(ind),]
         s2$amount <- x$amount[na.omit(ind)]
         y <- by(s2,s2$value,simplify=!FALSE,function(x) sum(x$count*x$amount))
         ret[names(y)] <- y
         ret
      }) |> do.call(rbind,args=_) |> data.frame(check.names=FALSE)
      ursa:::.elapsedTime("incompatibility -- finish")
     # session_grid("requisite/amount")
     # pu <- spatial_read("requisite/pulayer")
      if (F) {
         res2 <- pu
         spatial_data(res2) <- data.frame('1'=0,'2'=0,'3'=0,check.names=FALSE)
         ind1 <- match(rownames(res),pu$ID)
         print(summary(ind1))
         ind2 <- match(pu$ID,rownames(res))
         print(summary(ind2))
         spatial_data(res2[ind1,]) <- res
      }
      else {
         res2 <- pu[which(!is.na(match(as.character(pu$ID),rownames(res)))),]
         spatial_data(res2) <- res
      }
   }
   ret <- allocate(spatial_centroid(res2))
  # display(ret,decor=FALSE,stretch="eq")
   opW <- options(warn=-1)
   ursa_write(ret,fname)
   options(opW)
   prm_upload(fname)
   if (isShiny)
      removeNotification(id="trafficMap")
   ret
}
'tooltip' <- function(width=9) {
   body <- c("function(data, type, row, meta) {"
            ,paste0("return type === 'display' && data.length > ",width," ?")
            ,paste0("'<span title=\"' + data + '\">' + data.substr(0, ,",width,") + '...</span>' : data;")
            ,"}")
   paste(body,collapse=" ")
}
'navButton' <- function(label="Link to...",ref="#dummy",color="brown"
                       ,span=FALSE,border=FALSE) {
   internal <- !grepl("^https",ref)
   if (is.list(label)) {
      txt <- do.call(paste,label[!sapply(label,is.null)])
   }
   else {
      txt <- paste0("<span class=\"linkbutton ",color
                        ,"\"><a href=",ifelse(internal,"#section-","")
                        ,gsub("^#","",ref),ifelse(internal,""," target=\"_blank\"")
                        ,">",label,"</a></span>")
      if (span)
         return(txt)
   }
   shiny::HTML(paste0("<p",ifelse(border," class=\"areabutton\""," class=\"linebutton\""),">",txt,"</p>"))
}

'monther' <- function(s) {
   as.integer(format(as.Date(paste0("2021-",substr(s,1,3),"-15")
                    ,format="%Y-%b-%d"),"%m"))
}
'monthList' <- function(s) {
   s1 <- gsub(pattMonthSeq,"\\1",s)
   s2 <- gsub(pattMonthSeq,"\\2",s)
  # s1 <- monther(gsub(pattMonthSeq,"\\1",s))
  # s2 <- monther(gsub(pattMonthSeq,"\\2",s))
   opW <- options(warn=-1)
   v <- as.integer(c(s1,s2))
   options(opW)
   if (all(is.na(v))) {
      s1 <- monther(s1)
      s2 <- monther(s2)
   }
   else {
      ind <- which(v>=1 & v<=12)
      n <- length(ind)
      if (n==2) {
         s1 <- v[1]
         s2 <- v[2]
      }
      else if (n==0) {
         return(integer())
      }
      else {
         return(v[ind])
      }
   }
   s3 <- if (s1==s2) s1 else if (s1<s2) seq(s1,s2) else s3 <- c(seq(s1,12),seq(1,s2))
   s3
}
'industryCode' <- function(cname) {
   if (missing(cname))
      return(industryAbbr$abbr)
   if (is.null(cname)) {
     # return(cname)
      return(industryAbbr$abbr)
   }
   if (!length(cname))
      return(cname)
   if (!nchar(cname[1]))
      return(cname)
   if (all(cname %in% industryAbbr$abbr))
      return(cname)
   if (length(ind <- which(industryAbbr$activity %in% cname))>0)
      return(industryAbbr$abbr[ind])
   cname <- gsub("^[A-Z]([A-Z])+(:|\\s*-)*\\s","",cname)
   industryAbbr$abbr[match(gsub(pattRules,"\\2",cname),industryAbbr$industry)]
}
'industryName' <- function(abbr) {
   if (missing(abbr))
      return(industryAbbr$industry)
   if (is.list(abbr)) {
      return(gsub("^[A-Z]([A-Z])+(:|\\s*-)*\\s","",unlist(abbr)))
   }
   abbr <- gsub("^([A-Z]([A-Z])+)(:|\\s*-)*\\s.*","\\1",abbr)
   industryAbbr$industry[match(abbr,industryAbbr$abbr)]
}
'industryCodeName' <- function(name) {
   if (is.list(name))
      return(name)
   if (grepl("^[A-Z]{2,3}\\:\\s\\S.+$",name))
      return(name)
   isList <- is.list(name)
   ret <- lapply(name,\(x) paste0(industryCode(x),": ",industryName(x)))
   if (is.list(name))
      return(ret)
   unlist(ret)
}
'CFCode' <- function(cfname) {
   if (missing(cfname)) {
     # return(scenarioCF$CF_code)
      return(concern$CF_code |> unique() |> sort())
   }
   if (length(grep("\\d{4}",cfname)))
      return(gsub(".*(\\d{4}).*","\\1",cfname))
   scenarioCF[[indCFcode]][match(cfname,scenarioCF[[indCFname]])]
}
'CFName' <- function(cfcode) {
   if (missing(cfcode)) {
     # return(scenarioCF$CF_name)
      return(scenarioCF[[indCFname]][match(CFCode(),scenarioCF[[indCFcode]])])
   }
   scenarioCF[[indCFname]][match(cfcode,scenarioCF[[indCFcode]])]
}
'regionStats' <- function(aoi,ctable=NULL,group=NULL,activity=NULL,season=NULL
                         ,isPA=FALSE,raw=FALSE) {
   oldBehav <- !TRUE
   if (missing(aoi))
      aoi <- NULL
   isAOI <- !is.null(aoi)
   if (isShiny)
      cat("regionStats():\n")
   ursa:::.elapsedTime("0305a")
   puaoi <- puAOI(aoi)
   ursa:::.elapsedTime("0305b")
   coverland <- pu$Coverland[match(puaoi$ID,pu$ID)]
   if (isPA) {
     # str(PAs)
      if (!isAOI) { ## full domain
         ursa:::.elapsedTime("regE")
         aoi <- spatial_union(pu) |> spatial_transform(4326)
         spatial_data(aoi) <- data.frame(ID=0L)
      }
      ursa:::.elapsedTime("0305c")
     # PAs <- ursa:::.spatial_repair(PAs,verbose=TRUE)
      sf::st_agr(aoi) <- "constant"
     # PAs <- sf::st_cast(PAs,"POLYGON")
     # ind <- spatial_valid(PAs,each=TRUE)
     # print(c(aoi=spatial_crs(aoi),PAs=spatial_crs(PAs)))
     # if (spatial_crs(PAs)!=spatial_crs(aoi))
     #    PAs <- spatial_transform(PAs,aoi)
     # epa <- sf::st_intersection(aoi,PAs)
      opW <- options(warn=1) ## old GDAL `sf::sf_extSoftVersion()`?
      ursa:::.elapsedTime("0305d")
      epa <- spatial_intersection(aoi,spatial_transform(PAs,aoi))
      ursa:::.elapsedTime("0305e")
      options(opW)
   }
   if (is.null(ctable)) {
      if (isAOI) {
         ursa:::.elapsedTime("crosstable -- begin")
         b <- crossTable(puaoi)
         ursa:::.elapsedTime("crosstable -- end")
      }
      else {
         b <- data.frame(foo=rep("bar",length(group)))
         rownames(b) <- group
         attr(b,"group") <- group
         attr(b,"season") <- season
         attr(b,"industry") <- activity
      }
   }
   else
      b <- ctable
   if (is.null(season))
      season <- attr(b,"season")
   if (F & !is.null(season)) {
      ind <- na.omit(match(substr(season,1,3),substr(tail(seasonList,-1),1,3)))
      if (!length(ind))
         season <- NULL
   }
   if (is.null(group))
      group <- if (isAOI) rownames(b) else as.character(attr(b,"group"))
   result <- list()
   nPU <- spatial_count(puaoi)
   mul <- ifelse(useNACR,nPU,1)
  # cname <- industryCode(colnames(b))
  # listI <- c(na.omit(cname))
   listI <- attr(b,"industry")
   if (T)
      listCF <- rownames(b)
   else { ## gives error in shiny
      listCF <- attr(b,"group")
      if (any(nchar(listCF)!=4)) {
         list1 <- unique(concern$CF_code)
         listCF <- lapply(listCF,\(x) grep(x,list1,value=TRUE)) |> do.call(c,args=_) |> sort()
      }
   }
   ursa:::.elapsedTime("0305f")
   emptyCF <- !nrow(b)
   if (!emptyCF) {
      ursa:::.elapsedTime("0305g")
     # mul <- nPU
      if (F) {
         con2 <- concernSubset(concern,ctable=b)
      }
      else {
         batt <- attributes(b)
         con2 <- concernSubset(concern,group=group,activity=batt$industry,season=season)
      }
      ursa:::.elapsedTime("0305h")
      ic <- concernIndex(con2,aoi=aoi)
     # str(lapply(con2,unique))
      ursa:::.elapsedTime("0305i")
      ic2 <- ic[grep("^sumNAC",names(ic))]
     # str(lapply(ic2,sum))
      if (T & length(listCF)) ## All CFs are need for full domain
         con2 <- con2[con2$CF_code %in% listCF,]
      maxVal <- config$concern[1]
      if (isAOI)
         cvr <- b[,"Cover",drop=FALSE]*0.01
      else
         cvr <- data.frame(Cover=rep(1,length(attr(b,"group"))),row.names=attr(b,"group"))
      if (!oldBehav) {
         indCvr <- match(rownames(ic$concernNAC),rownames(cvr))
         ic$concernNAO <- ic$concernNAO*cvr$'Cover'[indCvr]
         ic$concernNAC <- ic$concernNAC*cvr$'Cover'[indCvr]
         ic$concernB <- ic$concernB*cvr$'Cover'[indCvr]
        ## ic$industryNAC and Coverage are not applicable
        # print(identical(rownames(ic$seasonNAC),rownames(ic$concernNAC)))
         ic$seasonNAC <- ic$seasonNAC*cvr$'Cover'[indCvr]
         ic$seasonB <- ic$seasonB*cvr$'Cover'[indCvr]
         result$'coverage' <- cvr$'Cover'[indCvr]
         names(result$'coverage') <- rownames(ic$concernNAC)
         result$'industryNAO' <- colSums(ic$concernNAO)
         result$'industryNAOB' <- colSums(ic$concernNAO)/colSums(ic$concernB)
         result$'industryNAC' <- colSums(ic$concernNAC)
         result$'industryNACB' <- colSums(ic$concernNAC)/colSums(ic$concernB)
         result$'industryNACB_' <- rowSums(ic$industryNAC)/rowSums(ic$industryB) ## doubt
         result$'CFNAC' <- rowSums(ic$concernNAC)
         result$'CFNACB' <- rowSums(ic$concernNAC)/rowSums(ic$concernB)
         result$'CFNACB_' <- rowSums(ic$seasonNAC)/rowSums(ic$seasonB) ## the same
         result$'seasonNAC' <- colSums(ic$seasonNAC)
         result$'seasonNACB' <- colSums(ic$seasonNAC)/colSums(ic$seasonB)
         result$'seasonNACB_' <- colSums(ic$industryNAC)/colSums(ic$industryB) ## doubt
         result$'NAO' <- sum(rowSums(ic$concernNAO))/nPU
         result$'NAOB' <- sum(rowSums(ic$concernNAO))/sum(rowSums(ic$concernB))
         result$'NAC' <- sum(rowSums(ic$concernNAC))/nPU
         result$'NACB' <- sum(rowSums(ic$concernNAC))/sum(rowSums(ic$concernB))
         result$'NACB_1' <- sum(rowSums(ic$seasonNAC))/sum(rowSums(ic$seasonB))
         result$'NACB_2' <- sum(rowSums(ic$seasonNAC))/sum(rowSums(ic$seasonB))
         result$'NACB_3' <- sum(colSums(ic$concernNAC))/sum(colSums(ic$concernB))
         result$'NACB_4' <- sum(rowSums(ic$industryNAC))/sum(rowSums(ic$industryB)) ## doubt
         result$'NACB_5' <- sum(colSums(ic$seasonNAC))/sum(colSums(ic$seasonB))
         result$'NACB_6' <- sum(colSums(ic$industryNAC))/sum(colSums(ic$industryB)) ## doubt
         result$'group' <- sort(attr(b,"group"))
         class(result) <- "Concern"
        # str(result)
         if (F) {
            print(lapply(result,summary))
            str(lapply(result,sum))
            q()
         }
      }
      if (oldBehav) { ## deprecated
         cat("-------------------------------------\n")
         result <- list()
         if (T & ignoreMissing) {
           # print(length(unique(con2$month)))
            if (F) {
               resM <- by(con2,list(con2$CF_code,con2$industry),\(x) {
                  nrow(x[!is.na(x$value),])
               })
               resM <- (resM>0)*length(unique(con2$month))
            }
            else
               resM <- ic$concernB
         }
         ##~ resI <- aggregate(con2$value
                          ##~ ,by=list(CF_code=con2$CF_code,industry=con2$industry)
                          ##~ ,\(x) length(na.omit(x)))
        # print(cvr[match(rownames(resI),rownames(cvr)),,drop=FALSE])
        # cvr$Cover <- 1
        # resI <- maxVal*ic$concernB*cvr$'Cover'[match(rownames(resM),rownames(cvr))]
         resI <- ic$concernNAC*cvr$'Cover'[match(rownames(resM),rownames(cvr))]
         if (F)
            resS <- by(con2,list(con2$CF_code,con2$month),\(x) {
               nrow(x[!is.na(x$value),])
            })
         else {
           # resS <- ic$seasonB
            resS <- ic$seasonNAC
         }
         resS <- resS[]*cvr$'Cover'[match(rownames(resS),rownames(cvr))]*maxVal
         result$'industryNAC' <- colSums(resI,na.rm=TRUE)/mul
         result$'CFNAC' <- rowSums(resI,na.rm=TRUE)/mul
         d <- colSums(resS,na.rm=TRUE)/mul
         names(d) <- format(as.Date(paste0("2021-",names(d),"-15")),"%b")
         result$'seasonNAC' <- d
        # mul <- sum(spatial_area(aoi)*1e-6)
        # result$'cover' <- cvr
         d <- ic$concernNAO/ic$concernB
        # result$'maxNAO' <- max(d,na.rm=TRUE)/mul*sum(cvr) ## cumulative for 12 months
         result$'maxNAO' <- maxVal/mul*sum(cvr)*length(result$'seasonNAC') ## monthly 
         dNAO <- d[rownames(d) %in% listCF,colnames(d) %in% listI,drop=FALSE]
         dNAO <- dNAO[match(rownames(cvr),rownames(dNAO)),,drop=FALSE]
         result$'NAOR' <- sum(rowSums(dNAO,na.rm=TRUE)*t(cvr))/nPU
        # result$'NAO' <- sum(rowSums(dNAO,na.rm=TRUE)*t(cvr[match(rownames(dNAO),rownames(cvr)),]))
         d <- ic$concernNAC/ic$concernB
         if (F & !is.null(season)) {
            print(c('season:'=season))
         }
         result$'maxNAC' <- max(d,na.rm=TRUE)/mul*sum(cvr) ## cumulative for 12 months
        # result$'maxNAC' <- maxVal/mul*sum(cvr)*length(result$'seasonNAC') ## monthly 
         dNAC <- d[rownames(d) %in% listCF,colnames(d) %in% listI,drop=FALSE]
         dNAC <- dNAC[match(rownames(cvr),rownames(dNAC)),,drop=FALSE]
         if (F) {
            dNAC <- dNAC/result$'maxNAC'*100
         }
         dNAM <- as.data.frame(maxVal*resM[])
         dNAM <- dNAM[match(rownames(cvr),rownames(dNAM)),,drop=FALSE]
        # str(dNAM)
        # print(rHU)
         result$'NACR' <- sum(rowSums(dNAC,na.rm=TRUE)*t(cvr))/nPU
        # result$'NAC' <- sum(rowSums(dNAC,na.rm=TRUE)*t(cvr[match(rownames(dNAC),rownames(cvr)),]))
         result$'meanNAOR' <- ic$meanNAOR
         result$'meanNACR' <- ic$meanNACR
         result$'NAO' <- result$'NAOR'*nPU
         result$'NAC' <- result$'NACR'*nPU
         maxMNS <- rowSums(dNAM,na.rm=TRUE)*cvr
        # print(sum(sum(rowSums(dNAC,na.rm=TRUE)*cvr)))
        # print(sum(MNS))
         result$'MNSR' <- sum(rowSums(dNAC,na.rm=TRUE)*cvr)/sum(maxMNS)
         result$'SR' <- sum(rowSums(dNAO,na.rm=TRUE)*cvr)/sum(maxMNS)
         if (F)
            d <- colSums(dNAC*t(cvr),na.rm=TRUE)/mul
         else {
            d <- ic$concernNAC
            B <- ic$concernB
            cover <- t(cvr[match(rownames(d),rownames(cvr)),,drop=FALSE])
            d <- colSums(d*cover)/colSums(B*cover)/maxVal
            d[is.infinite(d)] <- 0
            d[is.nan(d)] <- 0
            print(summary(d))
         }
         ##~ print(dNAC)
         ##~ print(cvr)
         ##~ print(dNAC*cvr)
         ##~ print(colSums(dNAC*cvr,na.rm=TRUE))
         ##~ str(result)
         if (length(unique(names(d)))==1)
            names(d) <- colnames(dNAC)
         result$'IND' <- d
         d0 <- sort(unique(d))
         thLo <- head(d0,3) |> tail(1)
         thHi <- tail(d0,3) |> head(1)
         res <- d[d>=thHi]
         if (length(res)>3)
            res <- res[res>=quantile(res,1-3/length(res))]
         result$'TOP IND' <- res
         res <- d[d<=thLo]
         if (length(res)>3)
            res <- res[res<=quantile(res,3/length(res))]
         result$'LC IND' <- res
         if (FALSE) {
            d <- concern[concern$CF_code %in% listCF & concern$industry %in% listI &
                         concern$value %in% c(1,2,3),]
            d$cover <- cvr[match(d$CF_code,rownames(cvr)),]
            d$value[d$value==1] <- mulNA[3]+10000
            d$value[d$value==2] <- mulNA[2]+10000
            d$value[d$value==3] <- mulNA[1]+10000
            ind <- d$value>10000
            d$value[ind] <- d$value[ind]-10000
            if ((FALSE)&&(staffOnly)&&(!isShiny))
               write.csv(d,"c:/tmp/interim.csv",row.names=FALSE,quote=FALSE)
            d$score <- d$value*d$cover
            d <- by(d,d$month,function(x) sum(x$score))/mul
            d <- d |> unclass() |> t() |>
               as.data.frame(x=_,check.names=F) |> unlist()
            names(d) <- format(as.Date(paste0("2021-",names(d),"-15")),"%b")
            if (!is.null(season)) {
              # print(match(substr(names(d),1,3),substr(season,1,3)))
               ind <- match(substr(season,1,3),substr(names(d),1,3))
               if (length(ind))
                  d <- d[ind]
            }
            str(d)
         }
         else {
            d <- ic$sumNACs
            names(d) <- format(as.Date(paste0("2021-",names(d),"-15")),"%b")
            print(summary(d))
         }
         result$'SC0' <- d
         d0 <- sort(unique(d))
         thHi <- tail(d0,3) |> head(1)
         indSc <- which(d>=thHi)
        # d1 <- d[indSc,,drop=F]
         res <- d[indSc]
         if (length(res)>3)
            res <- res[res>=quantile(res,1-3/length(res))]
         result$'SC' <- res
         if (F) {
            ref <- concernSubset(concern
                                ,season=names(result$'SC0')
                               # ,ctable=ctable
                               # ,season=md$season
                               # ,activity=md$industry
                               # ,group=md$group
                                )
            ref <- concernIndex(ref,aoi=NULL)
           # result$'meanNAOR' <- ic$meanNAOR
            result$'NACR' <- mean(unlist(ic$concernNAC))/mean(unlist(ic$concernB))
            result$'NAOR' <- mean(unlist(ic$concernNAO))/mean(unlist(ic$concernB))
           # result$'NAO' <- result$'NAOR'*nPU
            result$'meanNAOR' <- mean(unlist(ref$concernNAO))/mean(unlist(ref$concernB))
            result$'meanNACR' <- mean(unlist(ref$concernNAC))/mean(unlist(ref$concernB))
           # print(mean(ref$concernNAC)/mean(ref$concernB))
           # print(mean(ref$concernNAC)/mean(ref$concernB))
            str(mean(unlist(ic$concernNAC/ic$concernB),na.rm=TRUE))
            str(mean(unlist(ref$concernNAC/ref$concernB),na.rm=TRUE))
            str(mean(unlist(ic$concernNAC),na.rm=TRUE))
            str(mean(unlist(ref$concernNAC),na.rm=TRUE))
         }
      }
   }
   else {
      cvr <- NULL
      dNAO <- NULL
      dNAC <- NULL
      dNAM <- NULL
   }
   result$'nPU' <- nPU
   result$'nCF' <- nrow(b)
   result$'area_src' <- sum(spatial_area(aoi)*1e-6)
   result$'area_pu' <- sum(spatial_area(puaoi)*1e-6)
   result$'puLand' <- sum(coverland)*2*half*1e-6
   result$'puMarine' <- (nPU-sum(coverland))*2*half*1e-6
   if (isPA) {
      ursa:::.elapsedTime("0305j")
      if (spatial_count(epa))
         result$'ePA' <- sum(spatial_area(epa)*1e-6)
      else
         result$'ePA' <- 0
       ursa:::.elapsedTime("0305k")
   }
   class(result) <- "Concern"
   if (!raw)
      return(result)
   if (oldBehav)
      return(list(result=result,cover=cvr,NAO=dNAO,NAC=dNAC,NAM=dNAM))
   list(result=result,cover=cvr,NAO=ic$concernNAO,NAC=ic$concernNAC,NAM=ic$concernB)
}
'regionTable' <- function(metrics) { ## metrics=regionStats()
   if (metrics$nCF==0)
      return(DT::datatable(data.frame('Missed CFs'="bar",check.names=FALSE)[integer(),,drop=FALSE]
                          ,options=list(dom="t",ordering=F),class="compact"
                          ))
   res <- metrics[grep("(nPU|nCF|ePA|^(area|pu))",names(metrics))] |> as.data.frame()
   if (all(res[[3]]==0))
      res[[3]] <- res[[4]]
   cname <- colnames(res)
   indRound <- grep("^(pu|area|ePA)",colnames(res))
   dname <- rep("",ncol(res))
   ver <- 2
   dname[1] <- c("Number of Planning Units in Selected Region(s)"
                ,"No of PUs")[ver]
   dname[2] <- c("Number of Conservation Features"
                ,"No of CFs")[ver]
   dname[3] <- c("Area of Selected Region(s), sq.km"
                ,"Total Area, km<sup>2</sup>")[ver]
   dname[4] <- c("Area of Planning Units in Selected Region(s), sq.km"
                ,"Total Area, PUs, km<sup>2</sup>")[ver]
   dname[5] <- c("Terrestrial Area in Selected Region(s), sq.km"
                ,"Terrestrial Area, PUs, km<sup>2</sup>")[ver]
   dname[6] <- c("Marine Area in Selected Region(s), sq.km"
                ,"Marine Area, PUs, km<sup>2</sup>")[ver]
   if (length(dname)>=7)
      dname[7] <- c("Existing Protected Areas in Selected Region(s)
                   , sq.km","Existing Protected Areas, PUs, km<sup>2</sup>")[ver]
   cname[1] <- paste0("<abbr title='",dname[1],"'>","PUs","</abbr>")
   cname[2] <- paste0("<abbr title='",dname[2],"'>","CFs","</abbr>")
   cname[3] <- paste0("<abbr title='",dname[2],"'>","srcArea","</abbr>")
   cname[4] <- paste0("<abbr title='",dname[4],"'>","puArea","</abbr>")
   cname[5] <- paste0("<abbr title='",dname[5],"'>","puLand","</abbr>")
   cname[6] <- paste0("<abbr title='",dname[6],"'>","puMarine","</abbr>")
   if (length(cname)>=7)
      cname[7] <- paste0("<abbr title='",dname[7],"'>","EPA","</abbr>")
   colnames(res) <- dname
   da <- DT::datatable(res,escape=F,rownames="",selection="none"
                      ,options=list(dom="t",ordering=F),class="compact")
   DT::formatRound(da,indRound,1)
}
'regionPlotSeasonConcern' <- function(metrics,relative=TRUE,plotly=TRUE) {
   relative <- FALSE
   maxVal <- config$concern[1]
   if (FALSE) {
      sc <- metrics[['SC0']]*maxVal
      if (relative) {
        # sc <- sc/metrics$'maxNAC'*100
         sc <- sc/metrics$'seasonNAC'*100
      }
   }
   else {
      sc <- metrics$seasonNACB*100
      month <- as.integer(names(sc))
      if (all(!is.na(month)))
         names(sc) <- format(as.Date(paste0("2021-",month,"-15")),"%b")
   }
   sc <- data.frame(month=names(sc),value=sc)
   sc$month <- factor(sc$month,levels=sc$month,ordered=TRUE)
   ylabel <- ifelse(T | relative,"%",ifelse(useNACR,"Relative Indexes","Absolute indexes"))
   if (!plotly) {
      p <- ggplot(sc,aes(month,value))+geom_col()+labs(x="",y=ylabel)
      return(p)
   }
   if (!isNamespaceLoaded("plotly"))
      suppressMessages(require(plotly))
   p <- plot_ly(sc,x=~month,y=~value,type="bar",orientation="v") %>%
            layout(NULL
                  ,xaxis=list(title="")
                  ,yaxis=list(title=ylabel)
                 # ,margin=list(l=0,r=0,t=0)
                  )
   p
}
'regionPlotSeasonActivity' <- function(metrics,relative=TRUE,plotly=TRUE) {
   if (FALSE) {
      sc <- sum(metrics$capCF)
      m <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
      sc <- data.frame(month=m,value=sc/length(m))
      sc$month <- factor(sc$month,levels=sc$month,ordered=TRUE)
      ylabel <- "Index in raw values"
   }
   else {
      sc <- 100*metrics$CAA$capS/metrics$CAB$capS
      sc[is.na(sc)] <- 0
      m <- names(sc)
      if (all(!is.na(as.integer(m))))
         m <- format(as.Date(paste0("2021-",m,"-15")),"%b")
      m <- factor(m,levels=m,ordered=TRUE)
      sc <- data.frame(month=m,value=sc)
      ylabel <- "%"
   }
   if (!plotly) {
      p <- ggplot(sc,aes(month,value))+geom_col()+labs(x="",y=ylabel)
      return(p)
   }
   if (!isNamespaceLoaded("plotly"))
      suppressMessages(require(plotly))
   p <- plot_ly(sc,x=~month,y=~value,type="bar",orientation="v"
               ,marker=list(color='rgb(158,202,225)')) %>%
            layout(NULL
                  ,xaxis=list(title="")
                  ,yaxis=list(title=ylabel)
                 # ,margin=list(l=0,r=0,t=0)
                  )
   p
}
'regionPlotIndustryActivity' <- function(metrics,relative=FALSE,plotly=TRUE) {
   if (isBundle <- "CAA" %in% names(metrics)) {
     # qs::qsave(metrics,"C:/tmp/interim.qs")
     # metrics <- qs::qread("C:/tmp/interim.qs")
     # str(metrics)
      value <- 100*metrics$CAA$capIA/metrics$CAB$capIA
      value[is.na(value)] <- 0
      ref <- 100*metrics$CAA$capIA0/metrics$CAB$capIA0
      mvalue <- mean(ref,na.rm=TRUE)
      ref[is.na(ref)] <- 0
      IND <- data.frame(industry=names(value),value=value,ref=ref,name="A")
      IND <- IND[order(IND$value,IND$industry,decreasing=c(TRUE,FALSE),method="radix"),]
      IND$industry <- factor(IND$industry,levels=IND$industry,ordered=TRUE)
      IND$name <- industryName(IND$industry)
   }
   else {
      capCAA <- metrics$capIA#/metrics$nPU
      capCAA0 <- metrics$capIA0#/metrics$nPU0
      IND <- data.frame(industry=names(capCAA0),name=industryName(names(capCAA0)))
      if (T)
         IND <- cbind(IND,value=capCAA,ref=capCAA0)
      if (isBundle)
         IND <- cbind(IND,value1=100*capCAA/capCAB,ref1=100*capCAA0/capCAB0)
      IND$value[is.na(IND$value)] <- 0
   }
   iname <- substr(IND$industry,1,1)
   mcol <- ctIndustry[substr(IND$industry,1,1)]
   ylabel <- "%" # "Conservation Action Priority Level by Activity"
   range <- c(0.5,0.15*max(c(IND$value,IND$ref))) |> log()
   if (!plotly) {
      p <- ggplot(IND,aes(industry,value))+geom_col()+labs(x="",y=ylabel)
      return(p)
   }
   if (!isNamespaceLoaded("plotly"))
      suppressMessages(require(plotly))
   p <- plot_ly(IND,x=~industry,y=~value,type="bar",orientation="v"
               ,marker=list(color=mcol),name='AOI'
               ,text=~name
               ,textfont=list(color="transparent")
              # ,hoverinfo="text"
               ,hovertemplate=paste0("%{text}<br>%{x}: %{y:.1f}",ifelse(relative,"%",""))
               ) %>%
        add_trace(y=~ref,name='ArcNet domain',marker=list(color='rgb(204,204,204)')
                 ,visible="legendonly") %>%
        layout(NULL
              ,xaxis=list(title="")
              ,yaxis=list(title=ylabel#,type="log"
                        # ,range=range
                        # ,rangemode='tozero'
                         )
             # ,title="Conservation Concern Level by Activity"
             # ,showlegend=FALSE
              ,legend=list(x=0.5,y=0.9,orientation='h')
              ,shapes=if (F) NULL else list(list(x0=~head(industry,1)
                         ,x1=~tail(industry,1),y0=mvalue,y1=mvalue
                         ,type="line",line=list(color="#8888",dash="dot")))
             # ,margin=list(l=0,r=0,t=0)
              )
   p
}
'regionPlotIndustryConcern' <- function(metrics,relative=FALSE,plotly=TRUE) {
   relative <- FALSE
  # str(metrics)
  # saveRDS(metrics,"C:/tmp/interim.rds")
  # maxVal <- config$concern[1]
  # result <- metrics$'IND'
   result <- metrics$industryNACB*100
   if (isTRUE(relative)) { ## maxNAC
      print(result)
      print(metrics$'industryNAC')
      result <- result/metrics$'industryNAC'*100
   }
   else if (FALSE)
      result <- result*maxVal
  # ctable <- rvCrossTable()
  # d <- concernSubset(concern,ctable=ctable)
   d <- concern
   d <- concernSubset(d,season=names(metrics$'SC0'))#,verbose=T)
   if (FALSE) {
      d <- concernIndex(d,aoi=NULL)$concernNAC
      localNAC <- d[,names(result),drop=FALSE]/ifelse(useNACR,spatial_count(pu),1)
      ref <- colSums(localNAC,na.rm=TRUE)
      if (!useNACR & !relative)
         ref <- ref*metrics$'nPU'/spatial_count(pu)
                             # spatial_count(pu)/metrics$'nPU'
   }
   else {
     # ref <- concernIndex(d,aoi=NULL)#$sumNACi*maxVal
      ref <- regionStats(aoi=NULL
                        ,group=metrics$group
                        ,activity=names(metrics$industryNACB)
                        ,season=names(metrics$seasonNACB)
                        )$industryNACB*100
      mvalue <- mean(ref)
   }
   print(c(AOI=mean(result),ArcNet=mean(ref)))
   IND <- data.frame(industry=names(result)
                    ,name=industryName(names(result))
                    ,value=result
                    ,ref=ref
                    )
  # IND <- IND[!is.na(IND$value),]
   IND$value[is.na(IND$value)] <- 0
   IND <- IND[order(IND$value,IND$industry,decreasing=c(TRUE,FALSE),method="radix"),]
   IND$industry <- factor(IND$industry,levels=IND$industry,ordered=TRUE)
   iname <- substr(IND$industry,1,1)
  # sname <- unique(iname)
  # ct <- ursa::cubehelix(length(sname),bright=167,rotate="circle")
  # names(ct) <- sname
   mcol <- ctIndustry[substr(IND$industry,1,1)]
   if (F) {
      meanNAC <- sum(colSums(localNAC,na.rm=TRUE))/ifelse(!useNACR,spatial_count(pu),1)
      mvalue <- meanNAC/length(IND$industry)*ifelse(!useNACR & !relative,metrics$nPU,1)
      mvalue <- sum(colSums(localNAC,na.rm=TRUE))/length(IND$industry)
      if (!useNACR & !relative)
         mvalue <- mvalue*metrics$'nPU'/spatial_count(pu)
      if (isTRUE(relative)) { ## maxNAC
        # IND$value <- 100*IND$value/metrics$'maxNAC'
         maxNAC <- length(unique(concern$CF_code))*length(metrics$'SC0')*max(config$concern)
         IND$ref <- 100*IND$ref/maxNAC
         mvalue <- 100*mvalue/maxNAC
      }
   }
   ylabel <- ifelse(T | relative,"%",ifelse(useNACR,"Relative Indexes","Absolute indexes"))
   if (!plotly) {
      p <- ggplot(IND,aes(industry,value))+geom_col()+labs(x="",y=ylabel)
      return(p)
   }
   if (!isNamespaceLoaded("plotly"))
      suppressMessages(require(plotly))
   p <- plot_ly(IND,x=~industry,y=~value,type="bar",orientation="v"
               ,marker=list(color=mcol),name='AOI'
               ,text=~name
               ,textfont=list(color="transparent")
              # ,hoverinfo="text"
               ,hovertemplate=paste0("%{text}<br>%{x}: %{y:.1f}",ifelse(relative,"%",""))
               ) %>%
        add_trace(y=~ref,name='ArcNet domain',marker=list(color='rgb(204,204,204)')
                 ,visible="legendonly") %>%
        layout(NULL
              ,xaxis=list(title="")
              ,yaxis=list(title=ylabel)
             # ,title="Conservation Concern Level by Activity"
             # ,showlegend=FALSE
              ,legend=list(x=0.5,y=0.9,orientation='h')
              ,shapes=if (F & !relative) NULL else list(list(x0=~head(industry,1)
                         ,x1=~tail(industry,1),y0=mvalue,y1=mvalue
                         ,type="line",line=list(color="#8888",dash="dot")))
             # ,margin=list(l=0,r=0,t=0)
              )
   p
}
'regionConcernTables' <- function(metrics,ref,type=c("auto","raw")) {
   type <- match.arg(type)
   if (missing(ref)) {
      ref <- regionStats(aoi=NULL
                        ,group=metrics$group
                        ,season=names(metrics$seasonNACB)
                        ,activity=names(metrics$industryNACB)
                        )
      
   }
   if (!missing(ref)) {
      metrics <- metrics[grep("_",names(metrics),invert=TRUE)]
      ref <- ref[grep("_",names(ref),invert=TRUE)]
      res <- data.frame(SR=c(metrics$NAO,ref$NAO)
                       ,MNSR=c(metrics$NAC,ref$NAC)
                       ,SA=c(metrics$NAOB,ref$NAOB)*100
                       ,MNSA=c(metrics$NACB,ref$NACB)*100
                       )
      res <- rbind(res,apply(res,2,\(x) 100*x[1]/x[2]))
   }
   else {
      cat("regionConcernTables -- deprecated\n")
      if (F) {
         isAOI <- metrics$NACR==metrics$meanNACR
        # res <- metrics[grep('^NA',names(metrics))] |> as.data.frame()
         res <- metrics[c("NAOR","NACR","NAO","NAC")] |> as.data.frame()
         res <- rbind(res,data.frame(NAOR=metrics$meanNAOR,NACR=metrics$meanNACR,NAO=NA,NAC=NA))
         if (isAOI) {
            res <- res[1,,drop=FALSE]
            rownames(res) <- c("Selection","ArcNet")[2]
         }
         else
            rownames(res) <- c("Selection","ArcNet")
         cname <- colnames(res)
         cname[cname=="NAO"] <- paste0("<abbr title='"
                                      ,"Conservation concern, not allowed only"
                                      ,"'>","NAO","</abbr>")
         cname[cname=="NAC"] <- paste0("<abbr title='"
                                      ,"Conservation concern, not allowed and conditional"
                                      ,"'>","NAC","</abbr>")
         cname[cname=="NAOR"] <- paste0("<abbr title='"
                                      ,"Conservation concern, not allowed only relative"
                                      ,"'>","NAOR","</abbr>")
         cname[cname=="NACR"] <- paste0("<abbr title='"
                                      ,"Conservation concern, not allowed and conditional relative"
                                      ,"'>","NACR","</abbr>")
         colnames(res) <- cname
         res <- t(res) |> as.data.frame(check.names=FALSE)
      }
      else {
         res <- with(metrics,data.frame(SR=c(NAOR,meanNAOR,NAOR/meanNAOR*100)
                                       ,MNSR=c(NACR,meanNACR,NACR/meanNACR*100)
                                       ,SA=c(NAO,NA,SR*100)
                                       ,MNSA=c(NAC,sum(industryNAC),MNSR*100)
                                       ))
        # res <- t(res) |> as.data.frame(check.names=FALSE)
      }
   }
   if (!staffOnly) {
      res <- cbind(res[3,1:2],res[1,3:4])
      rownames(res) <- "%"
   }
   else
      rownames(res) <- c("src","ref","ratio")
   if ((!isShiny)||(type=="raw"))
      return(res)
   if (isShiny) {
      lut <- c('RSC-PU'="Relative Significant Concern Level for a given Area of Interest by Planning Units" ## 'SR'
              ,'ROC-PU'="Relative Overall Concern Level for a given Area of Interest by Planning Units" ## 'MNSR'
              ,'RSC-AOI'="Relative Significant Concern Level for a given Area of Interest" ## 'SA'
              ,'ROC-AOI'="Relative Overall Concern Level for a given Area of Interest" ## 'MNSA'
              )
      lname <- names(lut)
      if (!FALSE) {
         cname <- sapply(seq_along(lname)
            ,\(i) as.character(actionLink(inputId=paste0("click",lname[i])
                              ,label=lname[i]
                              ,onClick=sprintf("Shiny.setInputValue(id='thClick%s',value=%d,{priority: \"event\"});"
                                              ,lname[i],sample(seq(1000,9999),1))
                              )))
         cname <- sapply(seq_along(lut)
                        ,\(i) paste0("<abbr title='",lut[i],"'>",cname[i],"</abbr>"))
         ##~ lname <- sapply(seq_along(lname)
            ##~ ,\(i) a(lname[i],id=paste0("click",lname[i])
                   ##~ ,href="#",class=c("action-button","shiny-bound-input")) |> as.character())
         colnames(res) <- cname
      }
      else {
         cname <- sapply(seq_along(lut)
                        ,\(i) paste0("<abbr title='",lut[i],"'>",lname[i],"</abbr>"))
         colnames(res) <- cname
      }
   }
   da <- DT::datatable(res,escape=F
                     # ,colnames=""
                      ,selection="none"
                      ,options=list(dom="t",ordering=F),class="compact"
                      ,width="200px"
                      )
   da <- DT::formatRound(da,colnames(res),1)
   da
}
'regionActivityIndices' <- function(ctable=NULL,aoi=NULL,epoch=NULL
                                   ,extended=FALSE
                                  # ,type=c("auto","raw")
                                   ) {
   if (isShiny)
      cat("regionActivityIndices():\n")
  # aoi <- selectRegion(aoi)
   if (is.null(ctable))
      ctable <- crossTable(aoi=aoi)
   if (!extended) {
      CAA <- regionActionPriority(aoi=aoi,ctable=ctable,epoch=epoch,maxNAC=FALSE)
      CAB <- regionActionPriority(aoi=aoi,ctable=ctable,epoch=epoch,maxNAC=TRUE)
      return(list(CAA=CAA,CAB=CAB))
   }
  # cat("aoi:\n")
  # str(aoi)
  # cat("ctable:\n")
  # str(ctable)
   ##~ cap <- indexCAPR(aoi=aoi
                  ##~ ,group=attr(ctable,"season")
                  ##~ ,activity=NULL
                  ##~ ,season=attr(ctable,"season")
                  ##~ ) |> band_mean()
   if (F) {
      cap <- indexCAPR(aoi=aoi,ctable=ctable,epoch=epoch)
      cap <- round(band_mean(cap),0)
      hu <- indexHumanUse(aoi=aoi,ctable=ctable,epoch=epoch)
      hu <- round(band_mean(hu),2)
      res <- rbind(cap,hu)
      if (ncol(res)==1) {
         da <- data.frame(ArcNet=res[,"domain"])
      } else {
         da <- data.frame(Selection=res[,"aoi"],ArcNet=res[,"domain"])
      }
      if (FALSE & isShiny)
         shiny::removeNotification(id="indexCAPR")
     # print(res)
      rownames(da) <- c("<abbr title='Conservation Action Priority index per cell'>CAPR</abbr>"
                       ,"<abbr title='Industrial Activities Amount per cell'>HUAR</abbr>")
   }
   else {
      g0 <- session_grid();on.exit(session_grid(g0))
      session_grid(blank)
      if (isAOI <- !is.null(aoi))
         rAOI <- !is.na(puAOI(aoi) |> spatial_centroid() |> allocate())
      CAB <- regionActionPriority(aoi=aoi,ctable=ctable,epoch=epoch,maxNAC=TRUE)
      if (!is.null(CAB)) {
         cab <- ursa_read(file.path(root,CAB$cache))["cf"]
         cab <- c(domain=cab,aoi=if (!isAOI) cab else cab[rAOI])
         cab <- band_mean(cab)
      }
      CAA <- regionActionPriority(aoi=aoi,ctable=ctable,epoch=epoch,maxNAC=FALSE)
      if (!is.null(CAA)) {
         caa <- ursa_read(file.path(root,CAA$cache))["cf"]
        # print(as.data.frame(ursa_read(file.path(root,CAA$cache))["cf"][rAOI]))
        # print(as.data.frame(ursa_read(file.path(root,CAA$cache))["season"][rAOI]))
         caa <- c(domain=caa,aoi=if (!isAOI) caa else caa[rAOI])
         caa <- band_mean(caa)
      }
      else {
         caa <- cab <- c(domain=0,aoi=0)
      }
      CAP <- c(sum(CAA$capCF),sum(CAB$capCF))
      CAPR <- c(sum(CAA$capCF),sum(CAA$capCF0))
      hu <- indexHumanUse(aoi=aoi,ctable=ctable,epoch=epoch)
      hu <- band_mean(hu)
      if (length(hu)==1)
         hu <- c(hu,aoi=unname(hu)) 
      if (TRUE) 
         da <- cbind(CAPR=c(round(caa["aoi"],1),round(caa["domain"],1)
                           ,round(100*caa["aoi"]/caa["domain"],1))
                    ,CAP=c(round(caa["aoi"],1),round(cab["aoi"],1)
                          ,round(100*caa["aoi"]/cab["aoi"],1))
                    ,AAR=c(round(hu["aoi"],2),round(hu["domain"],2)
                          ,round(100*hu["aoi"]/hu["domain"],1))
                    )
      else {
         res <- cbind(cap=rev(caa)
                    # ,cam=c(domain=caa["aoi"],aoi=cab["aoi"])
                     ,cam=rev(cab)
                     ,hu=rev(hu))
         da <- rbind(res,relative=round(res["aoi",]/res["domain",]*100,1))
      }
      rownames(da) <- c("src","ref","%")
      ret <- list(CAB=CAB,CAA=CAA
                 ,nPU=if (isAOI) unname(band_n(rAOI)) else spatial_count(pu)
                 ,nPU0=spatial_count(pu)
                 ,summary=da)
      class(ret) <- "ActionPriority"
      return(ret)
   }
   NULL
}
'regionActivityTables' <- function(ctable=NULL,aoi=NULL,epoch=NULL,type=c("auto","raw")) {
   if (isShiny)
      cat("regionActivityTables():\n")
   type <- match.arg(type)
   if (FALSE & isShiny)
      shiny::showNotification(id="indexCAPR",closeButton=FALSE,duration=120
                      ,paste("This action may take some time."
                            ,"Please wait...")
                      ,type="warning")
   bundle <- regionActivityIndices(ctable=ctable,aoi=aoi,epoch=epoch,extended=TRUE)
   if (FALSE & isShiny)
      shiny::removeNotification(id="indexCAPR")
   da <- bundle$summary
   if (T & isShiny) {
      lut <- c('OIP-P'="Overall Industrial Pressure Level calculated for each Planning Unit" ## 'CAPR'
              ,'ROIP-AOI'="Relative Overall Industrial Pressure for a given Area of Interest" ## 'CAP'
              ,'AA-PU'="Relative Industrial Activity amount for a given Area of Interest by Planning Units" ## 'AAR'
              )
      lname <- names(lut)
      cname <- sapply(seq_along(lname)
         ,\(i) as.character(actionLink(inputId=paste0("click",lname[i])
                           ,label=lname[i]
                           ,onClick=sprintf("Shiny.setInputValue(id='thClick%s',value=%d,{priority: \"event\"});"
                                           ,lname[i],sample(seq(1000,9999),1))
                           )))
      cname <- sapply(seq_along(lut)
                     ,\(i) paste0("<abbr title='",lut[i],"'>",cname[i],"</abbr>"))
      colnames(da) <- cname
   }
   if (F & isShiny) {
      cname <- sapply(seq_along(lname)
         ,\(i) as.character(actionLink(inputId=paste0("click",lname[i])
                           ,label=lname[i]
                           ,onClick=sprintf("Shiny.setInputValue(id='thClick%s',value=%d,{priority: \"event\"});"
                                           ,lname[i],sample(seq(1000,9999),1))
                           )))
      colnames(da) <- c("<abbr title='Conservation Action Priority Relative'>OIP-P</abbr>" ## 'CAPR'
                       ,"<abbr title='Conservation Action Priority'>ROIP-AOI</abbr>" ## 'CAP'
                       ,"<abbr title='Activity Amount Relative (former HUAR)'>AA-PU</abbr>" ## 'HUAR'
                       )
   }
   if (!staffOnly) {
      da <- da[3,,drop=FALSE]
     # da[,2] <- NA
   }
   if ((!isShiny)||(type=="raw"))
      return(da)
   DT::datatable(da,escape=FALSE
                ,selection="none"
                ,options=list(dom="t",ordering=F),class="compact"
                )
}
'regionAddEPA_deprecated' <- function(map,aoi
                          ,group="Existing Protected Areas"
                          ,col="#992B"
                          ,layerID="layerEPA"
                          ,addPolygon=TRUE
                          ,addOverlay=TRUE
                          ,addLegend=TRUE) {
   prmAOI <- as.list(args(regionAddAOI))
   grAOI <- prmAOI$group
   grEPA <- group
   colEPA <- col
   layerEPA <- layerID
   showAOI <- (!missing(aoi))&&(is_spatial(aoi))
   showEPA <- addPolygon
   if (missing(map)) {
      stop("epsg?")
      if (showAOI)
         map <- conflictBasemap(aoi)
      else
         map <- conflictBasemap()
   }
   if (showEPA & addPolygon) {
      ursa:::.elapsedTime("0914c")
      opW <- options(warn=1) ## sf::sf_extSoftVersion() old GDAL?
      map <- leaflet::addPolygons(map,data=spatial_transform(PAs,4326)
                      ,label=grEPA
                      ,color=colEPA
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                 )
                      ,group=grEPA
                      ,layerId=layerEPA
                      )
      options(opW)
      ursa:::.elapsedTime("0914f")
   }
   if (showEPA & addLegend) {
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colEPA
                    ,opacity=0.2
                    ,labels=grEPA
                    ,group=grEPA
                    ,layerId=layerEPA
                    )
   }
   if (!showEPA) {
      ursa:::.elapsedTime("1018a")
      map <- removeControl(map,layerEPA)
      map <- removeShape(map,layerEPA)
      ursa:::.elapsedTime("1018b")
   }
   map <- removeLayersControl(map)
  # if (showEPA & addOverlay) {
   if (T) {
     # map <- showGroup(map,grEPA)
      map <- addLayersControl(map
                           ,overlayGroups=c(NULL
                                           ,"Basemap"
                                           ,if (showEPA) grEPA
                                           ,if (showAOI) grAOI
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
   }
   ursa:::.elapsedTime("0914g")
   map
}
'metricsMap' <- function(ctable=NULL,industry=NULL,group=NULL,season=NULL,subreg=NULL) {
   oldBehav <- TRUE
   group <- as.character(group)
   prm <- list(mulNA=config$concern,concern=concern,ctable=attributes(ctable)
              ,industry=sort(industry),group=sort(group),season=sort(season)
              ,subreg=subreg)
   prm$concern$comment <- NULL
   str(prm)
   fname <- file.path(root,"requests",paste0("m",digest::digest(prm,"crc32"),c(".rds",".tif")))
   prm_download(fname[2])
   if (T & file.exists(fname[2])) {
      cat(paste0("metricsMap: read from cache ",basename(fname[2]),"\n"))
      session_grid(NULL)
      return(ursa_read(fname[2]))
   }
   else {
      if (T & staffOnly)
         saveRDS(prm,fname[1])
      cat(paste0("metricsMap: new request ",basename(fname[2]),"\n"))
   }
   print("1023m")
   ic <- concernSubset(concern,ctable=ctable,activity=industry,group=group,season=season)
   print("1023n")
   ic <- concernIndex(ic,aoi=NULL)
   print("1023o")
   ursa:::.gc(TRUE)
   if (oldBehav) {
      v <- ic$concernNAC/ic$concernB
      v[is.na(v)] <- 0
      if (!length(indR <- do.call(c,lapply(group,grep,substr(rownames(v),1,1)))))
         indR <- seq_len(nrow(v))
      industryList <- colnames(v) |> industryName()
     # print(industryList |> industryCode()); q()
     # cfList <- rownames(v)
      if (!length(indC <- which(industryList %in% industry))) {
         if (length(indC <- which(names(industries) %in% industry))) {
            indC <- which(industryList %in% (industries[indC] |> unlist() |> unname()))
         }
      }
      if (!length(indC))
         indC <- seq_len(ncol(v))
      v <- v[sort(indR),sort(indC),drop=FALSE]
      v <- rowSums(v,na.rm=TRUE)
   }
   else {
      v <- ic$sumNAC
   }
   cf <- names(v)
   res <- puvspr[puvspr$species %in% cf,]
   rownames(res) <- NULL
  # print(subreg)
   resNAO <- res
   amount <- by(res$amount,res$species,sum) |> c()
   if (F) {
      str(puvspr)
      puvspr <- puvspr[puvspr$pu %in% (c(10094,21124,30953)[3]),]
     # puvspr <- puvspr[puvspr$pu %in% sample(puvspr$pu,2),]
      str(puvspr)
      resNAO <- res <- puvspr[puvspr$species %in% cf,]
      rownames(res) <- NULL
      rownames(resNAO) <- NULL
   }
   indCF <- match(res$species,cf)
   indAmount <- match(res$species,names(amount))
   str(res)
   res$value <- res$amount*v[indCF]/amount[indAmount]
  # res$sumNAC <- v[indCF] ## comment it
  # res$total_amount <- amount[match(res$species,names(amount))] ## comment it
  # print(res)
   print("1023p")
   if (!is.null(subreg)) {
      ind <- match(with(res,paste0(pu,species)),subreg)
      res <- res[which(!is.na(ind)),]
   }
   print("1023q")
   res <- by(res,res$pu,function(x) {
      sum(x$value)
   }) |> c()
  # print(summary(res))
   if (oldBehav) {
      vNAO <- ic$concernNAO/ic$concernB
      vNAO[is.na(vNAO)] <- 0
      vNAO <- vNAO[sort(indR),sort(indC),drop=FALSE]
      vNAO <- rowSums(vNAO,na.rm=TRUE)
   }
   else
      vNAO <- ic$sumNAO
   resNAO$value <- resNAO$amount*vNAO[indCF]/amount[indAmount]
   if (!is.null(subreg)) {
      ind <- match(with(resNAO,paste0(pu,species)),subreg)
      resNAO <- resNAO[which(!is.na(ind)),]
   }
   resNAO <- by(resNAO,resNAO$pu,function(x) sum(x$value)) |> c()
   ind <- which(!is.na(match(as.character(pu$ID),names(res))))
   res2 <- pu[ind,,drop=FALSE]
   spatial_data(res2) <- data.frame(PU=pu$ID[ind],SR=100*resNAO,MNSR=100*res)
   sf::st_agr(res2) <- "constant"
  # str(res2)
  # print(summary(res2$NAC))
  # res3 <- res2[res2$NAC>quantile(res2$NAC,0.9999),]
  # str(res3)
   if (FALSE)
      return(res2)
   session_grid(dist2land)
   res2 <- spatial_centroid(res2) |> allocate()
   opW <- options(warn=-1)
   ursa_write(res2,fname[2])
   options(opW)
   if (file.exists(fname[2]))
      prm_upload(fname[2])
   print("1023r")
   res2
}
# 'selectRegion' <- function(data=names(regionSF),region="(\\s(22)|Barents)") {
'selectRegion' <- function(region="PAC 22") {
   if (is.null(region))
      return(NULL)
   if (is_spatial(region))
      return(region)
   rname <- rname0 <- names(regionSF)
   rname[grep("priority.+area.+conservation",rname,ignore.case=TRUE)] <- "PACs"
   names(regionSF) <- rname
   b <- lapply(strsplit(region,split="\\s")[[1]],function(y) {
      d <- try(match.arg(y,rname),silent=TRUE)
      if (inherits(d,"try-error")) {
         if (ursa:::.is.numeric(y))
            y <- paste0("\\D",y)
         names(y) <- "reg"
         return(y)
      }
      names(d) <- "data"
      return(d)
   }) |> do.call(c,args=_)
  # str(strsplit(region,split="\\s")[[1]])
   aoi <- regionSF[[b["data"]]]
   if (!length(ind <- grep(b["reg"],aoi$region,ignore.case=TRUE)))
      return(NULL)
  # names(regionSF) <- rname0
   aoi[ind,]
}
'sleeping_ext' <- function() {
   if (isTRUE(getOption("sleepVerbose")))
      cat("                                                               (paused)\n")
   if (isTRUE((v <- getOption("sleepValue"))>0))
      Sys.sleep(v)
   return(invisible(NULL))
}
'config_init' <- function() {
   config <- list(comment=TRUE,relative=TRUE,sleepValue=0
                 ,concern=c(100,10,1),quantile=c(10,90),ncolor=7L
                # ,user=data.frame(name=character(),level=integer())
                 ,user=list()
                 )
   prm_init(config,configFile)
}
'config_exists' <- function() {
   prm_exists(configFile)
}
'comment_exists' <- function() {
   prm_exists(commentFile)
}
'config_read' <- function() {
   prm_read(configFile)
}
'comment_read' <- function() {
   da <- prm_read(commentFile)
   da <- do.call(rbind,lapply(da,as.data.frame))
   da <- da[order(da$Time,decreasing=TRUE),]
   da
}
'config_write' <- function(config) {
   prm_write(config,configFile)
}
'comment_write' <- function(comment) {
   prm_write(comment,commentFile)
}
'prm_exists' <- function(file) {
   file.exists(file)
}
'prm_init' <- function(prm,file) {
   if ((isRemote)||(prm_exists(file))) {
      prmPrev <- prm_read(file)
      if (identical(sort(names(prmPrev)),sort(names(prm))))
         prm <- prmPrev
      else
         prm_write(prm,file)
   } else {
      prm_write(prm,file)
   }
   prm
}
'prm_upload' <- function(file) {
   if (isRemote) {
      remote <- file.path("geranium",gsub(paste0(root,"/"),"",file))
      db <- rdrop2::drop_auth(rdstoken=rdstoken)
      wd_ <- setwd(dirname(file))
      rdrop2::drop_upload(basename(file),path=dirname(remote),verbose=F)
      setwd(wd_)
   }
   0L
}
'prm_download' <- function(file) {
   if ((isRemote)&&(T & !prm_exists(file))) {
      remote <- file.path("geranium",gsub(paste0(root,"/"),"",file))
      lpath <- dirname(file)
      db <- rdrop2::drop_auth(rdstoken=rdstoken)
      if (!dir.exists(lpath))
         dir.create(lpath,recursive=TRUE)
      wd_ <- setwd(lpath)
      a <- try(rdrop2::drop_download(remote,overwrite=TRUE,verbose=F))
      setwd(wd_)
      if ((inherits(a,"try-error"))&&(prm_exists(file))) {
         file.remove(file)
      }
   }
   0L
}
'prm_read' <- function(file) {
   prm_download(file)
   ret <- jsonlite::fromJSON(file,simplifyDataFrame=FALSE)
   ret
}
'prm_write' <- function(prm,file) {
   if (file.exists(file)) {
      prm0 <- jsonlite::fromJSON(file,simplifyDataFrame=FALSE)
      if (identical(prm0,prm)) {
        # cat(paste("no changes in",dQuote(file),"\n"))
         return(0L)
      }
   }
   writeLines(jsonlite::toJSON(prm),file)
   prm_upload(file)
}
'concernSubset' <- function(concern,ctable=NULL,group=NULL,activity=NULL,season=NULL
                           ,verbose=FALSE) {
   if (isShiny)
      cat("concernSubset():\n")
   d <- concern
   d$comment <- NULL
   rownames(d) <- NULL
   d$industry <- as.character(d$industry)
   if (!is.null(ctable)) {
      if (T) { ## ArcNet CF List
         gr <- attr(ctable,"group")
         if (!all(nchar(gr)==4)) {
            list1 <- sort(unique(d$CF_code))
            listCF <- lapply(gr,\(x) grep(paste0("^",x),list1,value=TRUE)) |>
               do.call(c,args=_) |> sort()
            if (!length(listCF)) {
               listCF <- list1[list1 %in% groupCF(gr)]
            }
         }
         else if (!is.null(gr))
            listCF <- gr
         else {
            listCF <- sort(unique(d$CF_code))
         }
      }
      else
         listCF <- as.integer(rownames(ctable))
     # listI <- industryCode(colnames(ctable)) |> na.omit() |> c()
      listI <- attr(ctable,"industry")
      listS <- attr(ctable,"season")
      if (any(nchar(listS)>2)) {
         listS <- match(substr(listS,1,3),substr(tail(seasonList,-1),1,3))
         if (!length(na.omit(listS)))
            listS <- seq(12)
      }
      if (verbose)
         str(list(listCF=listCF,listI=listI,listS=listS))
      if (verbose)
         str(d)
      d <- d[d$CF_code %in% listCF,]
      if (verbose)
         str(d)
      d <- d[d$industry %in% listI,]
      if (verbose)
         str(d)
      d <- d[d$month %in% listS,]
      if (verbose)
         str(d)
   }
   else {
      if (!is.null(group)) {
         if (verbose)
            str(d)
         if (all(nchar(group)==4))
            d <- d[d$CF_code %in% group,]
         else {
            gr <- lapply(group,\(x) grep(paste0("^",x),d$CF_code)) |> unlist() |> sort()
            if (!length(gr))
               gr <- groupCF(group)
            d <- d[gr,]
         }
      }
      if (!is.null(season)) {
         if (is.integer(season)) {
            d <- d[d$month %in% season,]
         }
         else {
            ind <- which(substr(tail(seasonList,-1),1,3) %in% substr(season,1,3))
            if (length(ind)) {
               if (verbose) {
                  str(d)
                  str(season)
                  str(tail(seasonList,-1))
                  str(c('season:'=ind))
               }
               d <- d[d$month %in% ind,]
            }
         }
        # gr <- lapply(group,\(x) grep(paste0("^",x),d$CF_code)) |> unlist() |> sort()
        # d <- d[gr,]
      }
      if (!is.null(activity)) {
         iname <- character()
         ind1 <- match(activity,industryAbbr$industry) |> na.omit() |> c()
         ind2 <- match(activity,industryAbbr$abbr) |> na.omit() |> c()
         ind3 <- match(activity,names(industries)) |> na.omit() |> c()
         if (verbose)
            str(list(ind1=ind1,ind2=ind2,ind3=ind3))
         if (length(ind1))
            iname <- c(iname,industryCode(activity))
         if (length(ind2))
            iname <- c(iname,activity)
         if (length(ind3))
            iname <- c(iname,industryCode(unlist(industries[activity])))
         if (length(iname)) {
            if (verbose) {
               print(iname)
               str(d)
            }
            d <- d[d$industry %in% iname,]
         }
      }
   }
   if (verbose) {
      str(d)
      print(series(d))
   }
   d
}
'concernIndex' <- function(concern,group=NULL,activity=NULL,season=NULL,aoi=NULL) {
   if (isShiny)
      cat("concernIndex():\n")
   mulNA <- config$concern
   maxVal <- max(mulNA)
   prm <- list(mulNA=mulNA,concern=concern,missing=ignoreMissing)
   if (isShiny)
      str(prm)
   concernFile <- file.path(root,"requests",paste0("i",digest::digest(prm,"crc32"),".rds"))
   prm_download(concernFile)
   if (T & file.exists(concernFile)) {
      cat(paste0("concernIndex: read from cache ",basename(concernFile),"\n"))
      return(readRDS(concernFile))
   }
   else
      cat(paste0("concernIndex: new request ",basename(concernFile),"\n"))
   ursa:::.elapsedTime("concernIndex -- start")
   ursa:::.elapsedTime("concernIndex -- finish") |> on.exit()
   concern$industry <- as.character(concern$industry)
   if (isShiny)
      shiny::showNotification(id="concernIndex",closeButton=FALSE,duration=120
                      ,if (!staffOnly) "Processing and rendering" else
                       paste("'concern' request"
                            ,dQuote(gsub("\\..+$","",basename(concernFile)))
                            ,"is at first time."
                            ,"Please wait his finalizing."
                            ,"It will be processed faster next time...")
                      ,type="warning")
   if ((develAOI <- FALSE)&&(!is.null(aoi))) {
      str(concern)
      print(unique(concern$CF_code))
      print(unique(concern$industry))
      print(unique(concern$month))
      print(is.null(aoi))
      b <- puAOI(aoi)
      str(b)
      q()
      am <- puvspr[puvspr$pu %in% b$ID,]
      cf1 <- am$species |> sort() |> unique()
      cf2 <- concern$CF_code  |> sort() |> unique()
      am2 <- by(am,am$species,function(x) {
         data.frame(cf=x$species[1],amount=sum(x$amount))
      }) |> do.call(rbind,args=_)
      am2$amount <- am2$amount/spec[spec$cf %in% am2$cf,"amount"]*100
      str(am2)
      q()
   }
   if (devel <- !TRUE) {
      val <- by(concern,list(value=concern$value),\(x) {
         x$comment <- NULL
         x
      })
      print(val)
     # str(val[])
   }
   concernNAR <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '3'))) |>
                 unclass() |> data.frame()
   concernNAY <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '2'))) |>
                 unclass() |> data.frame()
   concernNAG <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '1'))) |>
                 unclass() |> data.frame()
   seasonNAR <- by(concern,list(CF=concern$CF_code,month=concern$month)
                           ,function(x) length(which(x$value %in% '3'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   seasonNAY <- by(concern,list(CF=concern$CF_code,month=concern$month)
                           ,function(x) length(which(x$value %in% '2'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   seasonNAG <- by(concern,list(CF=concern$CF_code,month=concern$month)
                           ,function(x) length(which(x$value %in% '1'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   industryNAR <- by(concern,list(industry=concern$industry,month=concern$month)
                           ,function(x) length(which(x$value %in% '3'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   industryNAY <- by(concern,list(industry=concern$industry,month=concern$month)
                           ,function(x) length(which(x$value %in% '2'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   industryNAG <- by(concern,list(industry=concern$industry,month=concern$month)
                           ,function(x) length(which(x$value %in% '1'))) |>
                 unclass() |> data.frame(check.names=FALSE)
   seasonLength <- length(unique(concern$month))
   industryLength <- ncol(concernNAR)
   cfLength <- nrow(concernNAR)
   concernB <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                              ,function(x) {
      y <- length(which(!is.na(x$value)))
     # y[y==0] <- 0
      y
   }) |> unclass() |> data.frame()
   seasonB <- by(concern,list(CF=concern$CF_code,month=concern$month)
                              ,function(x) {
      y <- length(which(!is.na(x$value)))
     # y[y==0] <- 0
      y
   }) |> unclass() |> data.frame(check.names=FALSE)
   industryB <- by(concern,list(industry=concern$industry,month=concern$month)
                              ,function(x) {
      y <- length(which(!is.na(x$value)))
     # y[y==0] <- 0
      y
   }) |> unclass() |> data.frame(check.names=FALSE)
   if (F) {
      concernB <- seasonLength/concernB
      seasonB <- industryLength/seasonB
      industryB <- cfLength/industryB
   }
   if (F & ignoreMissing) {
      if (T) {
        # print(concernB,digits=2)
        # print(sum(concernB))
        # mul <- length(unique(concern$month))
         concernNAR <- concernNAR*concernB
         concernNAY <- concernNAY*concernB
         concernNAG <- concernNAG*concernB
      }
      if (T) {
        # print(seasonB,digits=2)
        # print(sum(seasonB))
         seasonNAR <- seasonNAR*seasonB
         seasonNAY <- seasonNAY*seasonB
         seasonNAG <- seasonNAG*seasonB
      }
      if (T) {
        # print(industryB,digits=2)
        # print(sum(industryB))
         industryNAR <- industryNAR*industryB
         industryNAY <- industryNAY*industryB
         industryNAG <- industryNAG*industryB
      }
   }
  # concernNAM <- mulNA[1]*concernB
   concernNAO <- mulNA[1]*concernNAR
   concernNAC <- mulNA[1]*concernNAR+mulNA[2]*concernNAY+mulNA[3]*concernNAG
   seasonNAC <- mulNA[1]*seasonNAR+mulNA[2]*seasonNAY+mulNA[3]*seasonNAG
   industryNAC <- mulNA[1]*industryNAR+mulNA[2]*industryNAY+mulNA[3]*industryNAG
   colnames(industryB) <- colnames(seasonB) <- sprintf("%02d",as.integer(colnames(seasonB)))
   colnames(industryNAC) <- colnames(seasonNAC) <- sprintf("%02d",as.integer(colnames(seasonNAC)))
  # print(industryNAC)
  # print(industryB[1:6,1:6])
  # print(seasonNAC[1:6,1:6])
   sumNAO <- rowSums(concernNAO)/rowSums(concernB)/maxVal
   sumNAC <- rowSums(concernNAC)/rowSums(concernB)/maxVal
  # sumNACi <- rowSums(industryNAC)/colSums(concernB)/maxVal ## the same no1
   sumNACi <- rowSums(industryNAC)/rowSums(industryB)/maxVal ## the same no1
   sumNACs <- colSums(seasonNAC)/colSums(seasonB)/maxVal
   sumNAO[is.infinite(sumNAO)] <- 0
   sumNAC[is.infinite(sumNAC)] <- 0
   sumNACs[is.infinite(sumNACs)] <- 0
   sumNACi[is.infinite(sumNACi)] <- 0
   sumNAC[is.nan(sumNAC)] <- 0
   sumNACi[is.nan(sumNACi)] <- 0
   sumNACs[is.nan(sumNACs)] <- 0
   meanNAOR <- sum(colSums(concernNAO,na.rm=TRUE))/maxVal #/spatial_count(pu)
   meanNACR <- sum(colSums(concernNAC,na.rm=TRUE))/maxVal #/spatial_count(pu)
   meanNACRs <- sum(colSums(seasonNAC,na.rm=TRUE))/maxVal #/spatial_count(pu)
   meanNACRi <- sum(colSums(industryNAC,na.rm=TRUE))/maxVal #/spatial_count(pu)
   if (F) {
      sumNAC <- rowSums(concernNAC,na.rm=TRUE)/ncol(concernNAC)/max(concernNAC,na.rm=TRUE)
      sumNAO <- rowSums(concernNAO,na.rm=TRUE)/ncol(concernNAO)/max(concernNAO,na.rm=TRUE)
   } else if (F & ignoreMissing) {
      sumNAC <- rowSums(concernNAC,na.rm=TRUE)/rowSums(concernB,na.rm=TRUE)/maxVal
      sumNAO <- rowSums(concernNAO,na.rm=TRUE)/rowSums(concernB,na.rm=TRUE)/maxVal
   } else if (F) {
      d <- max(config$concern)*seasonLength
      sumNAC <- apply(concernNAC,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
      sumNAO <- apply(concernNAO,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
      if (!FALSE) { ## TODO
         d <- max(config$concern)*industryLength
         sumNACs1 <- apply(seasonNAC,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d) ## !=sumNAC
         sumNACs2 <- apply(seasonNAC,2,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d) ## !=sumNAC
         d <- max(config$concern)*cfLength
         sumNACi1 <- apply(industryNAC,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d) ## !=sumNAC
         sumNACi2 <- apply(industryNAC,2,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d) ## !=sumNAC
      }
   }
   ##~ str(sum(unlist(concernNAC)))
   ##~ str(sum(unlist(seasonNAC)))
   ##~ str(sum(unlist(industryNAC)))
   ret <- list(concernNAO=concernNAO,concernNAC=concernNAC
              ,seasonNAC=seasonNAC,industryNAC=industryNAC
              ,concernB=concernB*maxVal,seasonB=seasonB*maxVal,industryB=industryB*maxVal
              ,meanNAOR=meanNAOR,meanNACR=meanNACR
              ,meanNACRs=meanNACRs,meanNACRi=meanNACRi
              ,sumNAO=sumNAO,sumNAC=sumNAC
              ,sumNACs=sumNACs,sumNACi=sumNACi
              )
   if (!dir.exists(dirname(concernFile)))
      dir.create(dirname(concernFile),recursive=TRUE)
   saveRDS(ret,concernFile)
   prm_upload(concernFile)
   if (isShiny)
      removeNotification(id="concernIndex")
   ret
}
'huAmount' <- function(epoch=c("2025","2030"),season=NULL,subset=NULL,verbose=FALSE) {
   if (T | isShiny)
      cat("huAmount():\n")
  # ursa:::.elapsedTime("start")
   epoch <- match.arg(epoch)
   if (is.null(season))
      season <- seq(12)
   else if (!length(season))
      season <- seq(12)
   else if (all(nchar(season)>=3)) {
      sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%b")
      season <- na.omit(match(tolower(substr(season,1,3)),tolower(sname)))
      if (!length(season))
         season <- seq(12)
   }
   opW <- options(warn=-1)
   season <- as.integer(season)
   options(opW)
   if (anyNA(season))
      season <- seq(12)
   mm <- sprintf("%02d",season)
   subset <- industryCode(subset)
  #  subset <- subset[subset %in% unique(gsub("^\\d+-(\\w+)-\\d+$","\\1",names(a)))]
   prm <- list(epoch=epoch,season=mm,subset=subset)
   if (verbose)
      str(prm)
   fname <- file.path(root,"requests",paste0("h",digest::digest(prm,"crc32"),".rds"))
   prm_download(fname)
   if (T & file.exists(fname)) {
      list2 <- file.path(root,readRDS(fname))
      if (length(fname2 <- list2[file.exists(list2)])>0) {
         cat("   reading from cache",basename(fname2[1]),"\n")
         return(ursa_read(fname2[1]))
      }
   }
   if (file.exists(fname2 <- gsub("\\.rds",".tif",fname)))
      return(ursa_read(fname2))
   cat("   cache",basename(fname2),"not found\n")
   list2 <- fname2
   a <- open_envi(file.path(root,"requisite/humanuse"),resetGrid=TRUE,cache=TRUE)
   industry <- sort(unique(gsub("^.+-([A-Z]{2,3})-.+$","\\1",names(a))))
   industry <- industry[industry %in% subset]
   res <- ursa(bandname=industry)
   for (i in seq(res) |> sample()) {
      res[i] <- local_sum(a[paste(epoch,industry[i],mm,sep="-")],cover=0)
   }
   ind <- which(!band_blank(res))
   w <- 1/band_max(res)
   w[is.infinite(w)] <- 0
   res <- res*w
   res2 <- ursa(bandname=mm)
   for (i in seq(res2) |> sample()) {
      res2[i] <- local_sum(a[paste(epoch,industry,mm[i],sep="-")],cover=0,weight=w)
   }
   close(a)
   if (FALSE)
      res <- res[!band_blank(res)]
   ##~ print(global_sum(res))
   ##~ print(global_sum(res2))
   if (alias <- !identical(names(res),prm$subset)) {
      prm$subset <- names(res)
      .fname2 <- fname2
      fname2 <- file.path(root,"requests",paste0("h",digest::digest(prm,"crc32"),".tif"))
      cat("   huAmount - resubsetting",basename(.fname2),"->",basename(fname2),"\n")
      rm(.fname2)
      list2 <- c(list2,fname2)
      saveRDS(list2,fname)
      prm_upload(list2)
      if (T & file.exists(fname2)) {
         return(ursa_read(fname2))
      }
   }
   saveRDS(gsub(paste0(root,"/"),"",list2),fname)
   prm_upload(fname)
   res2 <- res2[!band_blank(res2)]
   if (T) {
      res[res==0] <- NA
      if (length(res2))
         res2[res2==0] <- NA
   }
   else if (T) {
      res[is.na(blank)] <- NA
      res2[is.na(blank)] <- NA
   }
   if (F) {
      bmax <- band_max(res)
      if (verbose)
         print(bmax)
      if (length(ind <- which(bmax!=0)))
         res[ind] <- res[ind]/bmax[ind]
   }
  # res2 <- res2*global_sum(res)/global_sum(res2)
   if (length(res2)>0)
      res <- 100*c(res,res2)
   else
      res <- 100*c(res)
  # res <- res^0.5
   print(c(industry=res[grep("\\d",names(res))] |> global_sum()
          ,season=res[grep("\\D",names(res))] |> global_sum()))
   ursa_write(res,fname2)
   prm_upload(fname2)
  # ursa:::.elapsedTime("finish")
   res
}
'huAmount_ver2' <- function(epoch=c("2025","2030"),season=NULL,verbose=FALSE) {
   if (T | isShiny)
      cat("huAmount():\n")
  # ursa:::.elapsedTime("start")
   epoch <- match.arg(epoch)
   if (is.null(season))
      season <- seq(12)
   if (verbose)
      print(c(season=season))
   opW <- options(warn=-1)
   season <- as.integer(season)
   options(opW)
   if (anyNA(season))
      season <- seq(12)
   res <- ursa_read(paste0("humanuse/amount",epoch))
   when <- ursa_read(paste0("humanuse/season",epoch))
   w <- c(ursa_value(when))
   indW <- which(!is.na(w)) |> sample()
   mul <- sapply(w[indW],\(v) {
      length(which(which(diff(v %% 2^(seq(1,12)-1))>0) %in% season))
   })
   w[indW] <- mul
   ursa_value(when) <- w
   res <- res*when
   res <- res[!band_blank(res)]
   rm(when,w,indW,mul)
   if (T) {
      msk <- (dist2land["ID"]>0)-1
      res[is.na(res)] <- rep(msk,length(res))[is.na(res)]
      if (verbose)
         print(res)
   }
   bmax <- band_max(res)
   if (verbose)
      print(bmax)
   if (length(ind <- which(bmax!=0)))
      res[ind] <- res[ind]/bmax[ind]
   res <- res^0.5
  # ursa:::.elapsedTime("finish")
   res
}
'huAmount_ver1' <- function(verbose=FALSE) {
   res <- ursa_read("humanuse/amount")
   if (verbose)
      print(res)
   if (T) {
      msk <- (dist2land["ID"]>0)-1
      res[is.na(res)] <- rep(msk,length(res))[is.na(res)]
      if (verbose)
         print(res)
   }
   rm <- band_max(res)
   if (verbose)
      print(rm)
   ind <- which(rm!=0)
   res[ind] <- res[ind]/rm[ind]
   res <- res^0.5
   ret <- lapply(sight,function(x) {
      if (!(all(x %in% names(res))))
         return(NULL)
      y <- res[x]
      if (F & verbose)
         print(y)
      if (length(y)==1)
         return(y)
      local_sum(y,cover=0)
   }) |> as.ursa()
   ret
}
'indexNACR' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,subreg=NULL,verbose=FALSE) {
   ret <- metricsMap(ctable=ctable,group=group,industry=activity,season=season
                    ,subreg=subreg
                    )["MNSR"]
   names(ret) <- "OC-P"
   ret
}
'indexNAOR' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,subreg=NULL,verbose=FALSE) {
   ret <- metricsMap(ctable=ctable,group=group,industry=activity,season=season
                    ,subreg=subreg
                    )["SR"]
   names(ret) <- "SC-P"
   ret
}
'indexHumanUse' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                           ,epoch=NULL,verbose=FALSE) {
   session_grid(blank)
   if (!is.null(ctable)) {
      season <- attr(ctable,"season")
     # cs <- concernSubset(concern,ctable=ctable) ## quick, but CFs not for domain
      cs <- concernSubset(concern,ctable=NULL
                         ,group=attr(ctable,"group")
                         ,activity=attr(ctable,"industry")
                         ,season=season
                         )
   }
   else
      cs <- concernSubset(concern,ctable=NULL,group=group,activity=activity,season=season)
   rHU <- huAmount(epoch,season)
   ind <- which(names(rHU) %in% unique(cs$industry))
   if (!length(ind))
      return(c(humanUse=ursa(0)))
   res <- c(domain=local_sum(rHU[ind],cover=0))
   ignorevalue(res) <- -99
   if (!is.null(aoi)) {
      session_grid(res)
      rAOI <- !is.na(puAOI(aoi) |> spatial_centroid() |> allocate())
      return(c(res,aoi=res[rAOI]))
   }
   names(res) <- "AA-P"
   res
}
'indexCAPR' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,maxNAC=FALSE,concernNAC=NULL,seasonNAC=NULL,epoch=NULL
                       ,verbose=FALSE) {
  # maxNAC <- TRUE
   if (T | isShiny)
      cat("indexCAPR():\n")
   ursa:::.gc(isShiny)
   if (F & is.null(epoch))
      stop("NULL epoch")
   if (is.null(concernNAC)) {
      if (!is.null(ctable)) {
        # cs <- concernSubset(concern,ctable=ctable) ## quick, but CFs not for domain
         cs <- concernSubset(concern,ctable=NULL
                            ,group=attr(ctable,"group")
                            ,activity=attr(ctable,"industry")
                            ,season=attr(ctable,"season")
                            )
      }
      else
         cs <- concernSubset(concern,ctable=NULL,group=group
                            ,activity=activity,season=season)
      ci <- concernIndex(cs)
      if (maxNAC) {
        # nmonth <- length(unique(cs$month))
        # concernNAC <- as.data.frame((!is.na(concernNAC))*config$concern[1]*nmonth)
         concernNAC <- ci$concernB
         seasonNAC <- ci$seasonB
      }
      else {
         concernNAC <- ci$concernNAC
         seasonNAC <- ci$seasonNAC
      }
   }
   str(sum(unlist(concernNAC)))
   str(sum(unlist(seasonNAC)))
  # seasonNAC <- seasonNAC*sum(concernNAC,na.rm=TRUE)/sum(seasonNAC,na.rm=TRUE)
   listCF <- rownames(concernNAC)
  # listCF <- sort(unique(concern$CF_code))
  # str(puvspr)
  # cf <- puvspr[]
   cname <- colnames(concernNAC)
   if (is.null(seasonNAC))
      season <- NULL
   else
      season <- as.integer(colnames(seasonNAC))
   cap <- huAmount(epoch,season,subset=cname) ## rHU ## huAmount() rHU
   prmcap <- list(epoch=epoch,cap=names(cap))
  # cap[is.na(cap)] <- 0
   capS <- cap[grep("\\d+",names(cap))]
   if (!length(capS)) {
      capS <- ursa(NA,bandname=colnames(seasonNAC))
   }
   cap <- cap[cname]
   print(global_sum(cap))
   print(global_sum(capS))
   if (F & maxNAC)
      cap <- !is.na(cap)
   session_grid(cap)
   ind <- match(names(cap),cname)
   if (all(is.na(ind))) {
      session_grid(cap)
      return(c(domain=ursa(0),aoi=ursa(0)))
   }
   if (F) {
      concernNAC <- concernNAC[,na.omit(ind),drop=FALSE]
      cap <- cap[which(!is.na(ind))]
   }
   listCF0 <- as.character(sort(unique(concern$CF_code)))
   prm <- list(cap=prmcap,aoi=spatial_geometry(aoi),concern=concernNAC)
   fbase <- file.path("requests"
                     ,paste0("p",digest::digest(prm,"crc32"),".",c("tif","rds")))
   if (F & isShiny) {
     # saveRDS(prm,file.path("C:/tmp",basename(fbase[2])))
   }
   fname <- file.path(root,fbase)
   viaRaster <- TRUE
   prm_download(fname[1])
   if (viaRaster) {
      prm_download(fname[2])
   }
   if (isAOI <- !is.null(aoi))
      rAOI <- !is.na(puAOI(aoi) |> spatial_centroid() |> allocate())
  # ursa_write(rAOI,"C:/tmp/pac43.tif")
   if (devel <- FALSE) {
      cat("0911a -- CROP CROP CROP\n")
      rAOI <- ursa_crop(rAOI,border=1)
      cap <- regrid(cap)
      capS <- regrid(capS)
   }
   if (length(which(file.exists(fname)))!=length(fname)) {
      if (staffOnly)
         saveRDS(prm,file.path(dirname(fname)[2],gsub("\\.rds$","_.rds",basename(fname[2]))))
      cat("new CAPR request",basename(fname[1]),"\n")
      if (isShiny)
         shiny::showNotification(id="indexCAPR",closeButton=FALSE,duration=120
                         ,if (!staffOnly) "Processing and rendering" else 
                          paste("'CAPR' request"
                               ,dQuote(gsub("\\..+$","",basename(fname[1])))
                               ,"is at first time."
                               ,"Please wait his finalizing."
                               ,"It will be processed faster next time...")
                         ,type="warning")
      if (verbose) {
         print(series(concernNAC,8))
         print(cap)
      }
      if (!viaRaster) { ## via vector
         ursa:::.elapsedTime("HU amount via vector -- start")
         ursa:::.gc(T | isShiny)
         ##~ if (F) {
            ##~ str(puvspr)
            ##~ res <- by(puvspr$amount,puvspr$species,max,na.rm=TRUE)
            ##~ print(summary(res))
            ##~ print(puvspr[puvspr$species==1007,])
           ##~ # puvspr0 <- p
           ##~ # puvspr <- read.csv("D:/RAS/2021/accenter/branch/pampan/4.2.1/common/input/puvspr.dat.gz")
            ##~ puvspr <- read.csv("D:/RAS/2023/ArcNet/geranium/devel/puvspr.dat.gz")
           ##~ ##### ursa_value(a)[ind] <- cf$amount/max(cf$amount)
            ##~ str(puvspr)
            ##~ res <- by(puvspr$amount,puvspr$species,max,na.rm=TRUE)
            ##~ print(summary(res))
            ##~ print(puvspr[puvspr$species==1007,])
            ##~ puvspr$amount <- puvspr$amount/18
            ##~ print(puvspr[puvspr$species==1007,])
            ##~ q()
         ##~ }
         pr <- puvspr[puvspr$species %in% rownames(concernNAC),]
        # pr$coverage <- NULL
        # res1 <- by(puvspr,puvspr$species,\(x) max(x$amount))
        # print(summary(res1))
         cp <- c(pu=allocate(spatial_centroid(pu["ID"])),cap)
         cp <- cp[!is.na(cp["pu"])]
         cp <- polygonize(cp)
         cp <- spatial_data(cp)
        # cp <- cp[!is.na(cp$pu),]
         nac <- cbind(species=as.integer(rownames(concernNAC)),concernNAC)
         print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreasing=TRUE),7))
         if (indetails <- FALSE) {
            res1 <- merge(pr,cp,by="pu")
           # ursa:::.gc(T | isShiny)
            res2 <- merge(res1,nac,by="species")
           # ursa:::.gc(T | isShiny)
            str(pr)
            str(cp)
            str(nac)
           # str(res1)
           # str(res2)
            q()
            rm(res1,nac)
         }
         else {
            res2 <- merge(merge(pr,cp,by="pu"),nac,by="species")
            ursa:::.gc(T | isShiny)
            rm(nac)
         }
         print("'res2' was created")
         ursa:::.gc(T | isShiny)
         rname <- colnames(res2)
         aname <- gsub("\\..$","",rname)
         ta <- table(aname)
         tname <- names(ta[ta==2])
         ##~ str(pr)
         ##~ str(cp)
         ##~ str(nac)
         if (F) {
            xname <- paste0(tname,".x")
            yname <- paste0(tname,".y")
           # res2x <- res2[,paste0(tname,".x"),drop=FALSE]
           # res2y <- res2[,paste0(tname,".y"),drop=FALSE]
            print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreasing=TRUE),7))
            res2z <- res2[,paste0(tname,".x"),drop=FALSE]*
                     res2[,paste0(tname,".y"),drop=FALSE]
            print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreasing=TRUE),7))
            print("'res2z was assigned'")
            ursa:::.gc(T | isShiny)
            res2z <- res2z*res2[["amount"]]
            print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreasing=TRUE),7))
            print("after 'res2z*res2'")
            ursa:::.gc(T | isShiny)
            colnames(res2z) <- tname
            print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreasing=TRUE),7))
            res2 <- cbind(res2[,-match(c(xname,yname),colnames(res2))],res2z)
            rm(res2z)
            print("'res2z' is removed")
            ursa:::.gc(T | isShiny)
         }
         else {
            for (i in seq_along(tname)) {
              # message("DO MATRIX BY MATRIX MULTIPLICATION WITHOUT LOOP")
               cname1 <- paste0(tname[i],".x")
               cname2 <- paste0(tname[i],".y")
               res2[[tname[i]]] <- res2[[cname1]]*res2[[cname2]]*res2[["amount"]]
               res2[[cname1]] <- NULL
               res2[[cname2]] <- NULL
            }
         }
         str(res2)
        # print(res2)
        # print(sum(res2$amount))
         if (T) {
            if (TRUE) {
               saveRDS(res2,fname[2])
               if (FALSE)
                  prm_upload(fname[2])
            }
           # qs::qsave(res2,"C:/tmp/interim.qs")
           # res3 <- aggregate(res2,by=list(pu=res2$pu),function(x) {
           #    str(x)
           # })
            res3 <- by(res2[,tname],res2$pu,simplify=!FALSE,\(x) {
               sum(unlist(x),na.rm=TRUE)
            })
            rm(res2)
            ursa:::.gc(isShiny)
           # print(sum(res3))
           # print(mean(res3))
           # print(sum(res3)/spatial_count(pu))
            res3 <- data.frame(pu=as.integer(names(res3)),value=as.numeric(res3))
            a5 <- spatial_geometry(pu)[match(res3$pu,pu$ID)]
            spatial_data(a5) <- data.frame(domain=res3$value)
            a5 <- spatial_centroid(a5) |> allocate()
         }
         ursa:::.elapsedTime("HU amount via vector -- finish")
      }
      else { ## via raster
         ursa:::.elapsedTime("HU amount via raster -- start")
         a <- open_envi(file.path(root,"requisite/amount"),cache=TRUE)
        # a6 <- a["1007"]
        # print(as.data.frame(c(pu=dist2land["ID"][a6],a6=a6))[,-c(1,2)])
         aname <- names(a)
         cl <- length(chunk_band(a,ifelse(staffOnly,1500,1500))[[1]])
         nbreak <- ceiling(length(listCF)/cl)
        # str(cl)
         if (nbreak==1)
            cl <- rep(1L,length(listCF))
         else
            cl <- cut(seq_along(listCF),breaks=nbreak) |> as.integer()
        # print(table(cl))
         a4 <- a4s <- ursa(-101,nband=length(unique(cl)))
         v1 <- as.matrix(cap)
         v1[is.na(v1)] <- 0
         v1s <- as.matrix(capS)
         v1s[is.na(v1s)] <- 0
         colnames(v1) <- names(cap) ## 0311
         colnames(v1s) <- names(capS) ## 0311
         if (devel) {
            cat("rAOI (PAC XX):\n")
            print(as.matrix(rAOI))
            cat("v1 (Industrial Amount per activity):\n")
            print(v1)
            cat("v1s (Industrial Amount per season):\n")
            print(v1s)
        }
         capCF0 <- rep(0,length(listCF0))
         names(capCF0) <- listCF0
         capIA0 <- rep(0,length(cap))
         names(capIA0) <- names(cap)
         capS0 <- rep(0,length(capS))
         names(capS0) <- names(capS)
         if (isAOI) {
            capCF <- capCF0
            capIA <- capIA0
            capS <- capS0
            indAOI <- which(!is.na(ursa_value(rAOI)))
         }
         for (i in seq_along(unique(cl)) |> sample()) {
            print(c(i=i))
            if (T) { 
               bname <- listCF[cl==i]
               ind <- match(bname,aname)
               m <- m2 <- as.matrix(a[bname])
               m2[is.na(m2)] <- 0
               if (devel) {
                  colnames(m2) <- bname ## 0311
                  cat("m2 (CF Amount):\n")
                  print(m2)
               }
               v2 <- as.matrix(concernNAC[bname,])
               v2[is.na(v2)] <- 0
               if (devel) {
                  cat("v2 (industrial MNS):\n")
                  print(v2)
                  cat("v3 (cf MNS):\n")
                  print(m2 * t(v2 %*% t(v1)))
               }
              ## m: amount CF, v1: amount IA, v2: MNS
               a3 <- as_ursa(m * t(v2 %*% t(v1)))
               if (F) {
                 # ind2 <- match(puAOI(aoi)$ID,pu$ID)
                  v3 <- unclass(ursa_value(compress(a3[rAOI])))
                 # print(v3)
                  ind2 <- attr(v3,"sparse")
                  print(ursa_value(a3)[ind2,])
                  str(m)
                  str(v1)
                  str(m[ind2,])
                  str(v1[ind2,])
                  cat("---------------\n")
                  print(m_ <- m[ind2,,drop=FALSE]) ## Amount CF
                  print(v1_ <- v1[ind2,,drop=FALSE]) ## Amount IA
                  print(v1s_ <- v1s[ind2,,drop=FALSE]) ## Amount IA
                  q()
                  print(v2_ <- v2) ## MNS_{ij}
                  print(t(v2_ %*% t(v1_))) ## (MNS) %*% t(Amount IA)
                  print(m_ * t(v2_ %*% t(v1_))) 
                  cat("---------------\n")
                  ind <- which(v1[,1]>0)
                  str(ind)
                  print(summary(v1[ind,1]))
                  m_ <- m
                  v1_ <- v1
                  v2_ <- v2
                  m_[!is.na(m_)] <- 1
                  v1_[!is.na(v1_)] <- 1
                 # v2_[!is.na(v2_)] <- 1 
                  a3_ <- as_ursa(m_ * t(v2_ %*% t(v1_)))
                 # print(cor(na.omit(c(ursa_value(a3))),na.omit(c(ursa_value(a3_)))))
                  if (F)
                     a3 <- a3_
               }
               ##~ print(sum(c(v1)))
               ##~ print(sum(c(v2)))
               ##~ print(global_sum(a3))
               v4 <- (m2 %*% v2) * v1
               if (devel) {
                  cat("v4 (industrial cap):\n")
                  print(v4)
               }
               ##~ str(v4)
               ##~ print(sum(c(m2)))
               ##~ print(sum(c(v1)))
               ##~ print(sum(c(v2)))
               ##~ print(sum(c(v4)))
               capIA0 <- capIA0+colSums(v4,na.rm=TRUE)
               if (isAOI) {
                  capIA <- capIA+colSums(v4[indAOI,,drop=FALSE],na.rm=TRUE)
               }
               e4 <- as_ursa(v4)
               rm(v2,v4)
               v2s <- as.matrix(seasonNAC[bname,])
               v2s <- v2s[,match(colnames(v1s),colnames(v2s))]
               v2s[is.na(v2s)] <- 0
               if (devel) {
                  cat("v2s (seasonal MNS):\n")
                  print(v2s)
                  cat("v3s (??? MNS):\n")
                  print(m2 * t(v2s %*% t(v1)))
               }
               a3s <- as_ursa(m * t(v2s %*% t(v1s)))
               ##~ print(sum(c(v1s)))
               ##~ print(sum(c(v2s)))
               ##~ print(global_sum(a3s))
               v4s <- (m2 %*% v2s) * v1s
               if (devel) {
                  cat("v4s (seasonal cap):\n")
                  print(v4s)
               }
               ##~ print(sum(c(m2)))
               ##~ print(sum(c(v1s)))
               ##~ print(sum(c(v2s)))
               ##~ print(sum(c(v4s)))
               capS0 <- capS0+colSums(v4s,na.rm=TRUE)
               if (isAOI) {
                  capS <- capS+colSums(v4s[indAOI,,drop=FALSE],na.rm=TRUE)
               }
               rm(v2s,v4s,m,m2)
              # memoryUsage()
            }
            else {
               a2 <- a[listCF[cl==i]]
               bname <- names(a2)
              # print(a2)
              # print(as.data.frame(c(pu=dist2land["ID"][a2],a2=a2))[,-c(1,2)])
              # str(ursa_value(a2))
              # str(ursa_value(cap))
              # str(concernNAC[bname,])
               a3 <- ursa(bandname=bname)
               for (j in seq(a2) |> sample()) {
                 # a3[j] <- local_sum(a2[j]*cap*unlist(concernNAC[bname[j],]),cover=0)
                 # unlist(concernNAC[bname[j],]) |> str()
                  a5 <- a2[j]*cap*unlist(concernNAC[bname[j],])
                 # print(a5)
                  a3[j] <- local_sum(a5,cover=0)
               }
            }
           # print(capIA0)
           # print(capS0)
           # print(capIA0 |> sum())
           # print(capS0 |> sum())
           # rm(v2s,v4s,m,m2)
            memoryUsage()
           # print(series(a3,12))
           # print(series(a3[rAOI],12))
            ind <- match(bname,names(capCF0))
            capCF0[ind] <- band_sum(a3)
           # capS0[ind] <- band_sum(a3s)
            if (isAOI) {
               capCF[ind] <- band_sum(a3[rAOI])
              # capS[ind] <- band_sum(a3s[rAOI])
            }
            if (F) {
               str(capCF[capCF>0])
               str(capIA)
               q()
            }
            a4[i] <- local_sum(a3,cover=0)
            a4s[i] <- local_sum(a3s,cover=0)
         }
         close(a)
        # print(head(sort(sapply(ls(),\(x) object.size(get(x))),decreas=T),7))
         ursa:::.gc(TRUE)
        # a5 <- c(domain=local_sum(a4,cover=0))
        # a5s <- c(domain=local_sum(a4s,cover=0))
         ursa:::.elapsedTime("HU amount via raster -- finish")
         a5 <- c(cf=local_sum(a4,cover=0),season=local_sum(a4s,cover=0))
         print(a5)
        # display(a5,ramp=FALSE,stretch="eq")
        # print(a5s)
      }
      if (F) {
         print(as.data.frame(c(pu=dist2land["ID"][a5],a5=a5))[,-c(1,2)])
      }
      if (F)
         display(ursa_crop(a5,border=1))
      opW <- options(warn=-1)
      ignorevalue(a5) <- -101
      ursa_write(a5,fname[1])
      prm_upload(fname[1])
      if (viaRaster) {
         if (F) {
            cat("-------------\n")
            str(capCF0)
            str(capIA0)
            str(capS0)
            cat("-------------\n")
           # memoryUsage(TRUE)
           # q()
            if (isAOI) {
               str(capCF)
               str(capIA)
               str(capS)
            }
            print(sum(capCF0,na.rm=TRUE))
            print(sum(capIA0,na.rm=TRUE))
            print(sum(capS0,na.rm=TRUE))
            if (isAOI) {
               print(sum(capCF,na.rm=TRUE))
               print(sum(capIA,na.rm=TRUE))
               print(sum(capS,na.rm=TRUE))
            }
         }
         n0 <- band_n(blank)
         capCF0 <- capCF0/n0
         capIA0 <- capIA0/n0
         capS0 <- capS0/n0
         capCF0 <- capCF0[!is.na(capCF0)]
         capIA0 <- capIA0[order(names(capIA0))]
         if (isAOI) {
            n <- band_n(rAOI)
            capCF <- capCF/n
            capCF <- capCF[!is.na(capCF)]
            capIA <- capIA/n
            capIA <- capIA[order(names(capIA))]
            capS <- capS/n
            capS <- capS[order(names(capS))]
         }
         else {
            n <- n0
            capCF <- capCF0
            capIA <- capIA0
            capS <- capS0
         }
         md <- list(cache=fbase[1],capCF=capCF,capCF0=capCF0
                   ,capIA=capIA,capIA0=capIA0,capS=capS,capS0=capS0,nPU=n,nPU0=n0)
         saveRDS(md,fname[2])
         prm_upload(fname[2])
         if (F) {
            str(md)
            print(sum(md$capCF*md$nPU))
            print(sum(md$capCF0*md$nPU0))
            print(sum(md$capIA*md$nPU))
            print(sum(md$capIA0*md$nPU0))
         }
      }
      options(opW)
      if (isShiny)
         removeNotification(id="indexCAPR")
   }
   else {
      cat("read CAPR from cache",basename(fname[1]),"\n")
      session_grid(NULL)
      a5 <- ursa_read(fname[1])["cf"]
      names(a5) <- "domain"
   }
   if (isAOI) {
      a5 <- c(a5,aoi=a5[rAOI])
   }
   return(a5)
   if (F) {
      listI <- colnames(concernNAC)
      if (verbose)
         print(rHU)
     # rHU <- band_mean(rHU)
     # if (verbose)
     #    print(rHU)
      if (verbose) {
         print(sight[listI])
      }
      if (verbose)
         print(concernNAC)
      if (verbose)
         str(cap)
      #concernNAC
      #data.frame(cap)
      cap <- concernNAC*cap
      cvr <- ctable[,"Cover",drop=FALSE]
      ind <- match(rownames(cvr),rownames(cap))
     # cap1 <- sum(cap[ind,],na.rm=TRUE)
      cap <- cap[ind,]*cvr[[1]]/100
      cap <- sum(cap,na.rm=TRUE)
      return(cap)
   }
   return(NULL)
}
'iceConcCover' <- function(CF=NULL,industry=NULL,advanced=FALSE) {
   industry <- industryCode(industry)
   if (T & isShiny)
      print(data.frame(CF=CF,industry=industry))
   gPU <- spatial_centroid(pu) |> allocate(resetGrid=TRUE) |> spatial_grid()
   listCF <- concern$CF_code |> unique() |> sort()
   ice3 <- spatial_read(file.path(root,"requisite/iceconc"))
   rule <- rules[rules$abbr==industry,]
   if (FALSE)
      str(rule)
   iceDependent <- !is.na(rule$iceFree)
   if (TRUE) { # patch to fill all 22678 cells
      ice <- spatial_data(ice3[1,])
      ice[] <- 0
      ice <- ice[rep(1,spatial_count(pu)),]
      ice$id <- pu$ID
      ice[match(ice3$id,ice$id),] <- spatial_data(ice3)
      spatial_geometry(ice) <- spatial_geometry(pu)
      mname <- grep("month",colnames(ice),value=TRUE)
      for (m in mname) {
         ice[[m]] <- if (iceDependent) 1-ice[[m]]/100 else 1
      }
      if (isIceFree <- isTRUE(rule$iceFree<20)) {
         da <- spatial_data(ice[,mname])
         da[da<=0.5] <- 0
         da[da!=0] <- 1
         icefree <- data.frame(value=rowSums(da)>=rule$iceFree)
         spatial_geometry(icefree) <- spatial_geometry(ice)
         for (m in mname) {
            ice[[m]] <- ice[[m]]*icefree[[1]]
         }
      }
   }
   if (T) {
      above <- readxl::read_excel(file.path(root,"requisite/ArcNet CFs above zero.xlsx")
                                 ,.name_repair="minimal")
      above <- isTRUE(above[above$CF_code %in% CF
                           ,grep("above.*0",colnames(above),ignore.case=TRUE)]==1)
      terra <- rule$minDepth<0
   }
   concern2 <- concern[concern$CF_code==CF & concern$industry==industry,]
   if (!nrow(concern2))
      return(NULL)
   concern2 <- concern2[order(concern2$month),]
   rownames(concern2) <- NULL
   ind <- pu$depth<=rule$maxDepth & pu$depth>=rule$minDepth &
          pu$coast>=rule$minCoast*cell & pu$coast<=rule$maxCoast*cell
   human <- pu["ID"]
   human$industry <- 0L
   human$industry[ind] <- 1L
   if (isIceFree)
      human$industry <- human$industry*icefree$value
  # glance(human["industry"],plot.lwd=0.001,resetGrid=TRUE,border=0)
   spatial_data(human) <- cbind(spatial_data(human)
                               ,spatial_data(ice)[,grep("month",colnames(ice),value=TRUE)])
   for (m in mname) {
      human[[m]] <- human[[m]]*human[["industry"]]
   }
  # spatial_centroid(human[mname]) |> allocate(resetGrid=TRUE) |> display()
   assess <- human
   assess$amount <- 0
   sp <- puvspr[puvspr$species %in% CF,]
   assess$amount[match(sp$pu,human$ID)] <- sp$amount
   if (advanced) {
      g0 <- c(800,600)
      subject <- assess[assess$amount>0,]["amount"]
      session_grid(g0)
      compose_open(2)
      session_grid(consistent_grid(spatial_grid(subject),ref=g0,expand=1.1))
      bbox <- ursa:::spatialize(session_bbox())
      ct1 <- compose_panel(subject,blank="white",coast.fill="#00000010")
      session_grid(consistent_grid(spatial_grid(pu),ref=g0,expand=0.9))
      compose_panel(subject,pal=ursa_colortable(ct1)[[1]]
                   ,blank="white",coast.fill="#00000010",plot.lwd=0)
      panel_plot(bbox,lwd=1,col="transparent",border="black")
      compose_legend(ct1)
      compose_close()
   }
   for (m in mname) {
      assess[[m]] <- assess[[m]]*assess[["amount"]]
   }
   if (advanced) {
      session_grid(gPU)
      a <- spatial_centroid(assess[assess$amount>0,][mname]) |> allocate() |>
         ursa_crop(border=2)
      session_grid(a)
      display_brick(a,blank="white")
   }
   am <- assess[assess$amount>0,]
   # glance(am,resetGrid=TRUE)
   da <- spatial_data(am)
   res <- colSums(da[,mname])/sum(da[["amount"]])
   # ind <- match(gsub("\\D","",res) |> as.integer(),concern2$month)
   concern2$available <- 0
   ind <- match(concern2$month,gsub("\\D","",names(res)) |> as.integer())
   concern2$available <- res[ind]
   print(c(above0=above,terra=terra))
   if (above & !terra)
      concern2$available <- NA
   if (length(ind <- grep("industry",colnames(concern2),ignore.case=TRUE))>0)
      colnames(concern2)[ind] <- "Activity"
   knitr::kable(concern2,digits=3)
   ret <- list(grid=gPU,rule=rule,concern=concern2
              ,assess=am,human=human["industry"]
              ,ice=if (iceDependent) mname else NULL
              )
   ret
}
'bymonth' <- function(x=sample(12),split=FALSE,name=FALSE,verbose=FALSE) {
   if (name)
      sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%B")
   if (is.character(x)) {
      x <- strsplit(x,split=",") |> do.call(c,args=_)
      x <- lapply(x,\(v) {
         v <- as.integer(strsplit(v,split="-")[[1]])
         seq(head(v,1),tail(v,1))
      }) |> do.call(c,args=_) |> sort()
      if (split)
         return(x)
   }
   else
      x <- sort(unique(x))
   result <- rep("",12)
   for (k in seq(12)) {
      y <- x
      y[y<k] <- y[y<k]+12
      y <- sort(y)
      d <- diff(y)
     # if (all(d==1))
     #    return("1-12")
      ind <- which(d!=1)
      ind <- c(0,ind,length(x))
      j <- tail(seq_along(ind),-1)
      ret <- vector("list",length(j))
      for (i in j) {
         ret[[i-1]] <- y[seq(ind[i-1]+1,ind[i])]
      }
      ret <- lapply(ret,\(x2) {
         y2 <- range(x2) |> unique()
         y2[y2>12] <- y2[y2>12]-12
         if (name)
            y2 <- sname[y2]
         if (length(y2)==1)
            return(y2)
         paste(y2,collapse="-")
      })
      result[k] <- paste(do.call(paste,list(ret)),collapse=",")
   }
   if (verbose)
      print(result)
   part <- sapply(result,\(x) length(strsplit(x,split=",")[[1]]))
   result <- result[which.min(part)]
   result
}
'trafficValue' <- function(x) {
  # cat("trafficValue():\n")
  # print(x)
   isChar <- is.character(x)
   if (!length(x))
      return(x)
   if (F & all(as.integer(x) %in% c(1L,2L,3L)))
      return(x)
   if (any(!as.integer(x) %in% c(1L,2L,3L)))
      return(x)
  # ret <- rev(config$concern)[as.integer(x)]
   ret <- c('M','N','S')[as.integer(x)]
   if (!is.character(x))
      return(ret)
   as.character(ret)
}
'getPIN' <- function(name) {
   substr(strtoi(paste0("0x",substr(digest::digest(paste(adminPWD,name),"crc32"),1,6))),1,4)
}
'regionActionPriority' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL
                                  ,season=NULL,maxNAC=FALSE,epoch=NULL
                                  ,verbose=FALSE) {
   if (isShiny)
      cat("regionActionPriority():\n")
   if (!is.null(ctable)) {
     # cs <- concernSubset(concern,ctable=ctable) ## quick, but CFs not for domain
      cs <- concernSubset(concern,ctable=NULL
                         ,group=attr(ctable,"group")
                         ,activity=attr(ctable,"industry")
                         ,season=attr(ctable,"season")
                         )
   }
   else {
      cs <- concernSubset(concern,ctable=NULL,group=group,activity=activity
                         ,season=season)
   }
   ci <- concernIndex(cs)
   if (maxNAC) {
     # nmonth <- length(unique(cs$month))
     # concernNAC <- as.data.frame((!is.na(concernNAC))*config$concern[1]*nmonth)
      concernNAC <- ci$concernB
      seasonNAC <- ci$seasonB
   }
   else {
      concernNAC <- ci$concernNAC
      seasonNAC <- ci$seasonNAC
   }
   season <- unique(cs$month)
   cap <- huAmount(epoch,season,subset=colnames(concernNAC)) ## rHU ## huAmount()
   prmcap <- list(epoch=epoch,cap=names(cap))
  # cap <- cap[!is.na(cap)]
   ind <- match(names(cap),colnames(concernNAC))
   if (all(is.na(ind))) {
      return(NULL)
   }
   capS <- cap[grep("\\d+",names(cap))]
  # concernNAC <- concernNAC[,na.omit(ind),drop=FALSE]
   cap <- cap[which(!is.na(ind))]
   prm2 <- list(cap=prmcap,concern=concernNAC,aoi=spatial_geometry(aoi),maxNAC=maxNAC)
   fname2 <- file.path(root,"requests",paste0("a",digest::digest(prm2,"crc32"),".rds"))
  # saveRDS(prm2,file.path("C:/tmp",basename(fname2)))
   if (T & !file.exists(fname2)) {
      prm <- list(cap=prmcap,aoi=spatial_geometry(aoi),concern=concernNAC)
      fname <- file.path(root,"requests",paste0("p",digest::digest(prm,"crc32"),".rds"))
      if (F & file.exists(fname))
         file.remove(fname)
      if (!file.exists(fname)) {
         cat("needs for indexCAPR()",basename(fname),"\n")
         indexCAPR(aoi=aoi,concernNAC=concernNAC,seasonNAC=seasonNAC
                  ,maxNAC=F,epoch=epoch) ## maxNAC=maxNAC
         cat("indexCAPR()",basename(fname),"has got it!\n")
      }
      if (T | isShiny)
         cat(paste("Getting CAPR from cache:",basename(fname),"\n"))
      res2 <- readRDS(fname)
      if (all(c("capCF","capCF0","capIA","capIA0","nPU","nPU0") %in% names(res2))) {
         return(res2)
      }
      cat("This branch is deprecated\n")
      if (T | isShiny)
         cat(paste("Preparing action priority:",basename(fname2),"\n"))
      if (!is.null(ctable))
         listCF <- attr(ctable,"group")
      else
         listCF <- sort(unique(res2$species))
      if (isAOI <- !is.null(aoi)) {
         reg <- spatial_intersection(pu,spatial_transform(aoi,pu))
         reg <- reg[spatial_area(reg)>1e6,]
      }
      tname <- colnames(res2)
      tname <- tname[tname %in% industryCode()]
      res3 <- res2[res2$species %in% listCF,]
      capIA0 <- colSums(res3[,tname,drop=FALSE],na.rm=TRUE)
      capCF0 <- c(by(res3[,tname],res3$species,\(x) {
         y <- sum(unlist(x),na.rm=TRUE)
      }))[]
      if (isAOI) {
         res3 <- res3[res3$pu %in% reg$ID,]
         capIA <- colSums(res3[,tname,drop=FALSE],na.rm=TRUE)
         capCF <- c(by(res3[,tname,drop=FALSE],res3$species,\(x) {
            y <- sum(unlist(x),na.rm=TRUE)
         }))[]
      }
      else {
         capIA <- capIA0
         capCF <- capCF0
      }
      nPU <- ifelse(isAOI,spatial_count(reg),spatial_count(pu))
      nPU0 <- spatial_count(pu)
      if (T) {
         capCF <- capCF/nPU
         capCF0 <- capCF0/nPU0
         capIA <- capIA/nPU
         capIA0 <- capIA0/nPU0
      }
      ret <- list(cache=file.path("requests",gsub("\\.rds$",".tif",basename(fname)))
          ,capCF=capCF,capCF0=capCF0,capIA=capIA,capIA0=capIA0,nPU=nPU,nPU0=nPU0)
      saveRDS(ret,fname2)
      return(ret)
   }
   if (T | isShiny)
      cat(paste("Reading action priority from cache:",basename(fname2),"\n"))
   readRDS(fname2)
}
'memoryUsage' <- function(detail=FALSE) {
   cat("                                                            (verbosing)\n")
   ursa:::.elapsedTime("verbosing -- start")
   v1 <- Sys.time()
   v2 <- proc.time()
   v3 <- gc(verbose=FALSE)
   v4 <- max(v3[,6])
   if (v4>400)
      ursa:::.gc(TRUE)
  # v4 <- lobstr::mem_used()
   if (detail) {
      da <- lapply(search(),\(e) sapply(ls(e),\(x) object.size(get(x)))) |> unlist()
      da <- data.frame(object=names(da),size=unname(da))
      da <- da[order(da$size,decreasing=TRUE),]
      print(head(da,12))
      print(sum(da$size))
      print(lobstr::mem_used())
     # cat("App size:",pacs::app_size(file.path(root)) / 2^20, "MB\n")
   }
   print(v1)
   print(v2)
  # print(v3)
   print(v4)
   sink(file.path(root,"requests/verb-geranium.log"),append=TRUE)
   print(v1)
   print(v2)
  # print(v3)
   print(v4)
   sink()
   ursa:::.elapsedTime("verbosing -- finish")
   NULL
}
