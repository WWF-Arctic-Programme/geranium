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
   devel2 <- !FALSE
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
   if (identical(crs1,crs2)) {
      opW <- options(warn=-1)
      sf::st_crs(aoi) <- sf::st_crs(pu)
      options(opW)
   }
   else 
      aoi <- spatial_transform(aoi,pu)
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
'conflictBasemap' <- function(b) {
   if (missing(b)) {
     # b <- sf::st_cast(ursa:::spatialize(c(50,45,50,135),crs=4326),"POLYGON")
      e <- ursa:::spatialize(data.frame(lon=-45+c(0,180),lat=60,value=0),crs=4326)
      e <- spatial_transform(e,3575)
   }
   else {
      b <- spatial_transform(b,3575) ## 6931 of 'a'-rectangle is slight rotated to 3575 of 'b'
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
   #spatial_data(e) <- data.frame(id=seq(nrow(xy2)))
   m <- ursa:::polarmap(spatial_geometry(e),style="sdi",addFeature=F,opacity=0.7
                       ,group="Basemap"
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
'CFMap' <- function(cf) {
   if (missing(cf))
      return(conflictBasemap())
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
   m <- conflictBasemap(am)
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
                                        ,c("Arctic SDI","CF overlay")
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
  # saveRDS(m,"c:/tmp/leafletCF.rds")
   m
}
'regionMap' <- function(aoi=NULL,showPAs=FALSE) {
  # showPAs <- FALSE
   if (missing(aoi))
      return(conflictBasemap())
   showAOI <- is_spatial(aoi)
  # if (is.null(aoi))
  #    return(conflictBasemap())
   if (showAOI) {
      ursa:::.elapsedTime("0904a")
      aoi <- aoi |> puAOI() |> spatial_union() |> spatial_transform(4326)
      ursa:::.elapsedTime("0904b")
     # aoi <- spatial_transform(spatial_union(aoi),4326)
     # aoi <- spatial_transform(aoi,4326)
      m <- conflictBasemap(aoi)
   }
   else
      m <- conflictBasemap()
   ursa:::.elapsedTime("0904c")
   grAOI <- "Selected region(s)"
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
      ursa:::.elapsedTime("0904f")
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
      ursa:::.elapsedTime("0904g")
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
                                        ,"Arctic SDI"
                                        ,if (showPAs) grPAs
                                        ,if (showAOI) grAOI
                                        )
                        ,options=layersControlOptions(collapsed=FALSE)
                        )
  # if (T | showPAs)
  #    m <- hideGroup(m,grPAs)
   ursa:::.elapsedTime("0904h")
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
            d2[is.na(d2)] <- "N/A"
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
      da <- by(da,list(da$industry,da$month),\(x) x$value)[]
   }
   else {
      da <- concern[concern$industry==industryCode(cut),]
      da <- by(da,list(da$CF_code,da$month),\(x) x$value)[]
   }
   da[] <- as.character(da)
   da[is.na(da)] <- "N/A"
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
'crossTable' <- function(aoi=NULL,group=NULL,activity=NULL,season=NULL
                        ,minCover=0,verbose=FALSE) {
   if (isShiny)
      cat("crosstable():\n")
  # if (T & is.null(aoi))
  #    return(NULL)
   #aoi <- NULL
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
      ng <- unique(nchar(group))
      stopifnot('Mixed groups are not supported'=length(ng)==1)
      gr2 <- substr(as.character(am2$cf),1,ng)
      ind <- lapply(group,function(patt) grep(patt,gr2)) |> do.call(c,args=_) |> unique()
      am2 <- am2[ind,]
   }
   am2 <- am2[am2$cf %in% unique(concern$'CF_code'),]
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
            ret <- paste(sort(unique(x),decreasing=TRUE),collapse="/")
            ret[!nchar(ret)] <- "N/A"
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
         m <- na.omit(match(season,tail(seasonList,-1)))
         if (length(m))
            con2 <- con2[con2$month %in% m,]
         else {
            m <- na.omit(match(substr(season,1,3),substr(tail(seasonList,-1),1,3)))
            if (length(m))
               con2 <- con2[con2$month %in% m,]
         }
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
         ret <- paste(sort(unique(y),decreasing=TRUE),collapse="/")
         ret[!nchar(ret)] <- "N/A"
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
   indName <- match(rownames(res),scenarioCF$CF_code)
   indCF <- match(rownames(res),as.character(am2$cf))
   res <- cbind('CF name'=scenarioCF$CF_name[indName]
               ,'Cover'=am2$amount[indCF]
               ,'NAO'=NA
               ,'NAC'=NA
               ,res)
   res <- res[res$Cover>=minCover,]
   res[is.na(res)] <- "N/A" ## "" "N/A"
   if (is.null(season))
      season <- tail(seasonList,-1)
   attr(res,"industry") <- industryCode(colnames(res)) |> na.omit() |> c()
   attr(res,"group") <- group
   attr(res,"season") <- season
   con2 <- concernSubset(concern,ctable=res)
   ic <- concernIndex(con2) ## concern con2
   sumNAC <- ic$sumNAC
   sumNAO <- ic$sumNAO
   indNAI <- match(rownames(res),names(sumNAC))
   res$NAO <- sumNAO[indNAI]*100
   res$NAC <- sumNAC[indNAI]*100
   res
}
'conditionMap' <- function(industry="Mass tourism",group="\\d",season="max",economy=NULL) {
   finterim <- "C:/tmp/interim.rds"
   if (file.exists(finterim)) {
      res3 <- readRDS(finterim)
      str(res3)
      q()
     # print(object.size(res3))
      res4 <- by(res3,list(species=res3$species,industry=res3$industry),\(x) {
        # length(unique(x$value))
         sum(x$amount)
      })
      str(res4[])
      print(res4[1:6,1:6],digits=1)
      occupation <- aggregate(puvspr$amount,by=list(puvspr$species),sum)
      colnames(occupation) <- c("species","ncell")
      str(occupation)
      ind <- match(rownames(res4),occupation$species)
      occupation <- occupation[ind,]
      str(occupation)
      ncell <- 1/occupation[,"ncell",drop=TRUE]
      str(1/ncell)
      res5 <- res4[]*ncell
      str(res5[])
      print(res5[1:6,1:6],digits=1)
      res6 <- cbind(total=1/ncell,res5)
     # write.csv(res6,"c:/tmp/spvsac.csv")
      xlsx::write.xlsx(res6,"c:/tmp/spvsac.xlsx",sheetName="Potential touch",append=FALSE)
      q()
   }
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
              ,group=sort(unname(group))
             # ,season=sname[sort(match(season,sname))]
              ,season=sort(match(season,sname))
              ,economy=sort(unname(economy)))
   str(prm)
   cat("r:\n")
   str(r)
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
   cat("v:\n")
   str(v)
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
   cat("w:\n")
   str(w)
   u <- puvspr[,c("species","pu","amount")]
   cat("u:\n")
   str(u)
   if (F) {
      ind <- match(u$pu,pu$ID)
      u$coast <- pu$coast[ind]
      u$depth <- pu$depth[ind]
      print(object.size(u))
      str(u)
   }
   on.exit(ursa:::.elapsedTime("complete"))
   ursa:::.elapsedTime("w+r")
   res1 <- merge(w,r)
   str(res1)
   print(object.size(res1))
   res1 <- res1[res1$depth<=rule$maxDepth & res1$depth>=rule$minDepth &
                res1$coast>=rule$minCoast*cell & res1$coast<=rule$maxCoast*cell,]
  # res1 <- res1[,grep("^(min|max)",colnames(res1),invert=TRUE)]
   res1 <- res1[,c("pu","industry")]
   res1$industry <- factor(res1$industry,levels=levels(concern$industry))
   str(res1)
   print(object.size(res1))
   ##~ ursa:::.elapsedTime("wr+v")
   ##~ res1 <- merge(res1,v)
   ##~ str(res1)
   ##~ print(object.size(res1))
   ##~ return(40L)
   ursa:::.elapsedTime("u+v")
   res2 <- merge(u,v)
   str(res2)
   print(object.size(res2))
   res3 <- res2[with(res2,paste0(industry,pu)) %in% with(res1,paste0(industry,pu)),]
   saveRDS(res3,"C:/tmp/interim.rds")
   return(40L)
   ursa:::.elapsedTime("uv+w")
   res2 <- merge(res1,w)
   str(res2)
   print(object.size(res2))
   ursa:::.elapsedTime("uvw+r")
   res3 <- merge(res2,r)
   str(res3)
   print(object.size(res3))
   ursa:::.elapsedTime("complete")
   NULL
}
'interimMap' <- function(industry="Mass tourism",group="\\d",season="max",economy=NULL) {
   sname <- format(as.Date(paste0("2021-",seq(12),"-15")),"%B")
   byMonths <- (length(season))&&(all(season %in% sname))
   if ((group[1]=="\\d")&&(grepl("max",season[1],ignore.case=TRUE))&&(is.null(economy))) {
      fname <- file.path("trafficlights"
                        ,paste0("t",digest::digest(unname(industry),"crc32")))
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
   fname <- file.path("requests",paste0("t",digest::digest(prm,"crc32")))
   if (envi_exists(fname)) {
      cat(paste0("interimMap: read from cache ",fname,"\n"))
      session_grid(NULL)
      return(read_envi(fname))
   }
   if (isShiny)
      shiny::showNotification(id="trafficMap",closeButton=FALSE,duration=120
                      ,paste("Your 'trafficlight' request is at first time."
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
   else {
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
      res2 <- indexCAPR(group=group,activity=industry,season=season)
      res2 <- polygonize(res2)
   }
   else if (isTRUE(economy=="nacr")) {
      str(list(group=group,activity=industry,season=season))
      res2 <- indexNACR(group=group,activity=industry,season=season)
      res2 <- polygonize(res2)
   }
   else if (isTRUE(economy=="naor")) {
      res2 <- indexNAOR(group=group,activity=industry,season=season)
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
   write_envi(ret,fname)
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
   if (is.list(label)) {
      txt <- do.call(paste,label[!sapply(label,is.null)])
   }
   else {
      txt <- paste0("<span class=\"linkbutton ",color
                        ,"\"><a href=#section-",gsub("^#","",ref),">"
                        ,label,"</a></span>")
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
   s1 <- monther(gsub(patt,"\\1",s))
   s2 <- monther(gsub(patt,"\\2",s))
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
   industryAbbr$abbr[match(gsub(pattRules,"\\2",cname),industryAbbr$industry)]
}
'industryName' <- function(abbr) {
   if (missing(abbr))
      return(industryAbbr$industry)
   industryAbbr$industry[match(abbr,industryAbbr$abbr)]
}
'CFCode' <- function(cfname) {
   if (missing(cfname))
      return(scenarioCF$CF_code)
   if (length(grep("\\d{4}",cfname)))
      return(gsub(".*(\\d{4}).*","\\1",cfname))
   scenarioCF$CF_code[match(cfname,scenarioCF$CF_name)]
}
'CFName' <- function(cfcode) {
   if (missing(cfcode))
      return(scenarioCF$CF_name)
   scenarioCF$CF_name[match(cfcode,scenarioCF$CF_code)]
}
'regionStats' <- function(aoi,ctable=NULL,season=NULL,isPA=FALSE,raw=FALSE) {
   if (isShiny)
      cat("regionStats():\n")
   puaoi <- puAOI(aoi)
   coverland <- pu$Coverland[match(puaoi$ID,pu$ID)]
   if (isPA) {
     # str(PAs)
      if (is.null(aoi)) { ## full domain
         ursa:::.elapsedTime("regE")
         aoi <- spatial_union(pu) |> spatial_transform(4326)
         spatial_data(aoi) <- data.frame(ID=0L)
      }
     # PAs <- ursa:::.spatial_repair(PAs,verbose=TRUE)
      sf::st_agr(aoi) <- "constant"
     # PAs <- sf::st_cast(PAs,"POLYGON")
     # ind <- spatial_valid(PAs,each=TRUE)
     # print(c(aoi=spatial_crs(aoi),PAs=spatial_crs(PAs)))
     # if (spatial_crs(PAs)!=spatial_crs(aoi))
     #    PAs <- spatial_transform(PAs,aoi)
     # epa <- sf::st_intersection(aoi,PAs)
      opW <- options(warn=1) ## old GDAL `sf::sf_extSoftVersion()`?
      epa <- spatial_intersection(aoi,spatial_transform(PAs,aoi))
      options(opW)
   }
   if (is.null(ctable)) {
      ursa:::.elapsedTime("crosstable -- begin")
      b <- crossTable(puaoi)
      ursa:::.elapsedTime("crosstable -- end")
   }
   else
      b <- ctable
   if (is.null(season))
      season <- attr(b,"season")
   if (!is.null(season)) {
      ind <- na.omit(match(substr(season,1,3),substr(tail(seasonList,-1),1,3)))
      if (!length(ind))
         season <- NULL
   }
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
   emptyCF <- !nrow(b)
   if (!emptyCF) {
     # mul <- nPU
      con2 <- concernSubset(concern,ctable=b)
      ic <- concernIndex(con2)
      if (T & length(listCF)) ## All CFs are need for full domain
         con2 <- con2[con2$CF_code %in% listCF,]
      maxVal <- config$concern[1]
      cvr <- b[,"Cover",drop=FALSE]*0.01
      resM <- by(con2,list(con2$CF_code,con2$industry),\(x) {
         nrow(x[!is.na(x$value),])
      })
      if (F & ignoreMissing) {
        # print(length(unique(con2$month)))
         resM <- (resM>0)*length(unique(con2$month))
        # print(resM[])
        # q()
      }
      ##~ resI <- aggregate(con2$value
                       ##~ ,by=list(CF_code=con2$CF_code,industry=con2$industry)
                       ##~ ,\(x) length(na.omit(x)))
     # print(cvr[match(rownames(resI),rownames(cvr)),,drop=FALSE])
      resI <- maxVal*resM*cvr$'Cover'[match(rownames(resM),rownames(cvr))]
     # print(resI[])
      resS <- by(con2,list(con2$CF_code,con2$month),\(x) {
         nrow(x[!is.na(x$value),])
      })
      resS <- resS[]*cvr$'Cover'[match(rownames(resS),rownames(cvr))]*maxVal
      result$'industryNAC' <- colSums(resI,na.rm=TRUE)/mul
      result$'CFNAC' <- rowSums(resI,na.rm=TRUE)/mul
      d <- colSums(resS,na.rm=TRUE)/mul
      names(d) <- format(as.Date(paste0("2021-",names(d),"-15")),"%b")
      result$'seasonNAC' <- d
     # mul <- sum(spatial_area(aoi)*1e-6)
     # result$'cover' <- cvr
      d <- ic$concernNAO
     # result$'maxNAO' <- max(d,na.rm=TRUE)/mul*sum(cvr) ## cumulative for 12 months
      result$'maxNAO' <- maxVal/mul*sum(cvr)*length(result$'seasonNAC') ## monthly 
      dNAO <- d[rownames(d) %in% listCF,colnames(d) %in% listI,drop=FALSE]
      dNAO <- dNAO[match(rownames(cvr),rownames(dNAO)),,drop=FALSE]
      result$'NAOR' <- sum(rowSums(dNAO,na.rm=TRUE)*t(cvr))/nPU
     # result$'NAO' <- sum(rowSums(dNAO,na.rm=TRUE)*t(cvr[match(rownames(dNAO),rownames(cvr)),]))
      d <- ic$concernNAC
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
      result$'NACR' <- sum(rowSums(dNAC,na.rm=TRUE)*t(cvr))/nPU
     # result$'NAC' <- sum(rowSums(dNAC,na.rm=TRUE)*t(cvr[match(rownames(dNAC),rownames(cvr)),]))
      result$'meanNAOR' <- ic$meanNAOR
      result$'meanNACR' <- ic$meanNACR
      result$'NAO' <- result$'NAOR'*nPU
      result$'NAC' <- result$'NACR'*nPU
      MNS <- rowSums(dNAM,na.rm=TRUE)*cvr
     # print(sum(sum(rowSums(dNAC,na.rm=TRUE)*cvr)))
     # print(sum(MNS))
      result$'MNSR' <- sum(rowSums(dNAC,na.rm=TRUE)*cvr)/sum(MNS)
      result$'SR' <- sum(rowSums(dNAO,na.rm=TRUE)*cvr)/sum(MNS)
      d <- colSums(dNAC*t(cvr),na.rm=TRUE)/mul
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
      result$'SC0' <- d
      d0 <- sort(unique(d))
      thHi <- tail(d0,3) |> head(1)
      indSc <- which(d>=thHi)
     # d1 <- d[indSc,,drop=F]
      res <- d[indSc]
      if (length(res)>3)
         res <- res[res>=quantile(res,1-3/length(res))]
      result$'SC' <- res
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
      if (spatial_count(epa))
         result$'ePA' <- sum(spatial_area(epa)*1e-6)
      else
         result$'ePA' <- 0
   }
   if (!raw)
      return(result)
   list(result=result,cover=cvr,NAO=dNAO,NAC=dNAC,NAM=dNAM)
}
'regionTable' <- function(metrics) { ## metrics=regionStats()
   if (metrics$nCF==0)
      return(DT::datatable(data.frame('Missed CFs'="bar",check.names=FALSE)[integer(),,drop=FALSE]
                          ,options=list(dom="t",ordering=F),class="compact"
                          ))
   res <- metrics[grep("(nPU|nCF|ePA|^(area|pu))",names(metrics))] |> as.data.frame()
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
'regionPlotSeason' <- function(metrics,relative=TRUE) {
   sc <- metrics[['SC0']]
   if (relative) {
     # sc <- sc/metrics$'maxNAC'*100
      sc <- sc/metrics$'seasonNAC'*100
   }
   sc <- data.frame(month=names(sc),value=sc)
   sc$month <- factor(sc$month,levels=sc$month,ordered=TRUE)
   p <- plot_ly(sc,x=~month,y=~value,type="bar",orientation="v") %>%
            layout(NULL
                  ,xaxis=list(title="")
                  ,yaxis=list(title=ifelse(relative,"Indexes (%)"
                          ,ifelse(useNACR,"Relative Indexes","Absolute indexes")))
                 # ,margin=list(l=0,r=0,t=0)
                  )
   p
}
'regionPlotIndustry' <- function(metrics,relative=TRUE) {
   str(metrics)
  # saveRDS(metrics,"C:/tmp/interim.rds")
   result <- metrics$'IND'
   if (isTRUE(relative)) { ## maxNAC
      print(result)
      print(metrics$'industryNAC')
      result <- result/metrics$'industryNAC'*100
   }
  # ctable <- rvActivityStat()
  # d <- concernSubset(concern,ctable=ctable)
   d <- concern
   d <- concernSubset(d,season=names(metrics$'SC0'),verbose=T)
   d <- concernIndex(d)$concernNAC
   localNAC <- d[,names(result),drop=FALSE]/ifelse(useNACR,spatial_count(pu),1)
   ref <- colSums(localNAC,na.rm=TRUE)
   if (!useNACR & !relative)
      ref <- ref*metrics$'nPU'/spatial_count(pu)
                          # spatial_count(pu)/metrics$'nPU'
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
   p <- plot_ly(IND,x=~industry,y=~value,type="bar",orientation="v"
               ,marker=list(color=mcol),name='Region'
               ,text=~name
               ,textfont=list(color="transparent")
              # ,hoverinfo="text"
               ,hovertemplate="%{text}<br>%{x}: %{y:.1f}%"
               ) %>%
        add_trace(y=~ref,name='Domain',marker=list(color='rgb(204,204,204)')
                 ,visible="legendonly") %>%
        layout(NULL
              ,xaxis=list(title="")
              ,yaxis=list(title=ifelse(relative,"Indexes (%)"
                      ,ifelse(useNACR,"Relative Indexes","Absolute indexes")))
             # ,title="Conservation Concern Level by Activity"
             # ,showlegend=FALSE
              ,legend=list(x=0.5,y=0.9,orientation='h')
              ,shapes=if (!relative) NULL else list(list(x0=~head(industry,1)
                         ,x1=~tail(industry,1),y0=mvalue,y1=mvalue
                         ,type="line",line=list(color="#8888",dash="dot")))
             # ,margin=list(l=0,r=0,t=0)
              )
   p
}
'regionConcernIndices' <- function(metrics) {
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
      rownames(res) <- c("src","ref","%")
      if (isShiny) {
         lut <- c('SR'="Significant Relative"
                 ,'MNSR'="Minor-Notable-Significant Relative"
                 ,'SA'="Significant for Area"
                 ,'MNSA'="Minor-Notable-Significant for Area"
                 )
         lname <- names(lut)
         cname <- sapply(seq_along(lut)
                        ,\(i) paste0("<abbr title='",lut[i],"'>",lname[i],"</abbr>"))
         colnames(res) <- cname
      }
      if (!staffOnly)
         res <- res[3,,drop=FALSE]
     # res <- t(res) |> as.data.frame(check.names=FALSE)
   }
   if (!isShiny)
      return(res)
   da <- DT::datatable(res,escape=F
                     # ,colnames=""
                      ,selection="none"
                      ,options=list(dom="t",ordering=F),class="compact"
                      ,width="200px"
                      )
   da <- DT::formatRound(da,colnames(res),1)
   da
}
'regionActivityIndices' <- function(ctable=NULL,aoi=NULL) {
  # aoi <- selectRegion(aoi)
   if (is.null(ctable))
      ctable <- crossTable(aoi=aoi)
  # cat("aoi:\n")
  # str(aoi)
  # cat("ctable:\n")
  # str(ctable)
   if (FALSE & isShiny)
      shiny::showNotification(id="indexCAPR",closeButton=FALSE,duration=120
                      ,paste("This action may take some time."
                            ,"Please wait...")
                      ,type="warning")
   ##~ cap <- indexCAPR(aoi=aoi
                  ##~ ,group=attr(ctable,"season")
                  ##~ ,activity=NULL
                  ##~ ,season=attr(ctable,"season")
                  ##~ ) |> band_mean()
   if (F) {
      cap <- indexCAPR(aoi=aoi,ctable=ctable)
      cap <- round(band_mean(cap),0)
      hu <- indexHumanUse(aoi=aoi,ctable=ctable)
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
      cam <- indexCAPR(aoi=aoi,ctable=ctable,maxNAC=TRUE)
      cam <- round(band_mean(cam),1)
      if (length(cam)==1)
         cam <- c(cam,aoi=unname(cam))
      print(cam)
      cap <- indexCAPR(aoi=aoi,ctable=ctable)
      cap <- round(band_mean(cap),1)
      if (length(cap)==1)
         cap <- c(cap,aoi=unname(cap))
      hu <- indexHumanUse(aoi=aoi,ctable=ctable)
      hu <- round(band_mean(hu),2)
      if (length(hu)==1)
         hu <- c(hu,aoi=unname(hu)) 
      res <- cbind(cap=rev(cap),cam=c(domain=cap["aoi"],aoi=cam["aoi"]),hu=rev(hu))
      da <- rbind(res,relative=round(res["aoi",]/res["domain",]*100,1))
      if (FALSE & isShiny)
         shiny::removeNotification(id="indexCAPR")
      rownames(da) <- c("src","ref","%")
      if (isShiny)
         colnames(da) <- c("<abbr title='Conservation Action Priority Relative'>CAPR</abbr>"
                          ,"<abbr title='Conservation Action Priority'>CAP</abbr>"
                          ,"<abbr title='Activity Amount Relative (former HUAR)'>AAR</abbr>"
                          )
      if (!staffOnly) {
         da <- da[3,,drop=FALSE]
         da[,2] <- NA
      }
   }
   if (!isShiny)
      return(da)
   DT::datatable(da,escape=FALSE
                ,selection="none"
                ,options=list(dom="t",ordering=F),class="compact"
                )
}
'regionAddAOI' <- function(map,aoi=NULL
                          ,group="Selected region(s)",col="#092B",layerID="layerAOI"
                          ,addPolygon=TRUE,addOverlay=FALSE,addLegend=TRUE) {
   grAOI <- group
   colAOI <- col
   layerAOI <- layerID
   ursa:::.elapsedTime("0904g")
   showAOI <- (!missing(aoi))&&(is_spatial(aoi))
   if (missing(map)) {
      if (showAOI) {
         ursa:::.elapsedTime("0904i1")
         map <- conflictBasemap(aoi)
      }
      else {
         ursa:::.elapsedTime("0904i2")
         map <- conflictBasemap()
      }
      ursa:::.elapsedTime("0904j")
   }
   if (FALSE & showAOI) {
      ursa:::.elapsedTime("0904a")
      aoi <- aoi |> puAOI() |> spatial_union() |> spatial_transform(4326)
      ursa:::.elapsedTime("0904b")
     # aoi <- spatial_transform(spatial_union(aoi),4326)
     # aoi <- spatial_transform(aoi,4326)
   }
   if (showAOI & addPolygon) {
      ursa:::.elapsedTime("0904c")
      map <- leaflet::addPolygons(map,data=aoi
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
      ursa:::.elapsedTime("0904f")
   }
   if (showAOI & addLegend) {
      ursa:::.elapsedTime("0904k")
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colAOI
                    ,opacity=0.2
                    ,labels=grAOI
                    ,group=grAOI
                    ,layerId=layerAOI
                    )
   }
   if (showAOI & addOverlay) {
      ursa:::.elapsedTime("0904l")
      map <- addLayersControl(map
                           ,overlayGroups=c(NULL
                                           ,"Arctic SDI"
                                           ,if (showAOI) grAOI
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
   }
     ##~ # if (T | showPAs)
     ##~ #    m <- hideGroup(m,grPAs)
   ursa:::.elapsedTime("0904h")
   map
}
'regionAddEPA' <- function(map,aoi
                          ,group="Existing Protected Areas",col="#992B",layerID="layerEPA"
                          ,addPolygon=TRUE,addOverlay=FALSE,addLegend=TRUE) {
   prmAOI <- as.list(args(regionAddAOI))
   grAOI <- prmAOI$group
   grPAs <- group
   colPAs <- col
   layerEPA <- layerID
   showAOI <- (!missing(aoi))&&(is_spatial(aoi))
   showEPA <- TRUE
   if (missing(map)) {
      if (showAOI)
         map <- conflictBasemap(aoi)
      else
         map <- conflictBasemap()
   }
   if (showEPA & addPolygon) {
      ursa:::.elapsedTime("0914c")
      opW <- options(warn=1) ## sf::sf_extSoftVersion() old GDAL?
      map <- leaflet::addPolygons(map,data=spatial_transform(PAs,4326)
                      ,label=grPAs
                      ,color=colPAs
                      ,fillOpacity=0.2
                      ,highlightOptions=leaflet::highlightOptions(fillOpacity=0.5
                                                                 )
                      ,group=grPAs
                      )
      options(opW)
      ursa:::.elapsedTime("0914f")
   }
   if (showEPA & addLegend) {
      map <- leaflet::addLegend(map
                    ,position="bottomleft"
                    ,colors=colPAs
                    ,opacity=0.2
                    ,labels=grPAs
                    ,group=grPAs
                    )
   }
   if (showEPA & addOverlay) {
      map <- showGroup(map,grPAs)
      map <- addLayersControl(map
                           ,overlayGroups=c(NULL
                                           ,"Arctic SDI"
                                           ,if (showEPA) grPAs
                                           ,if (showAOI) grAOI
                                           )
                           ,options=layersControlOptions(collapsed=FALSE)
                           )
   }
   map
}
'metricsMap' <- function(ctable=NULL,industry=NULL,group=NULL,season=NULL) {
   prm <- list(mulNA=config$concern,concern=concern,ctable=attributes(ctable)
              ,industry=industry,group=group,season=season)
   str(prm)
   fname <- file.path("requests",paste0("m",digest::digest(prm,"crc32")))
   if (envi_exists(fname)) {
      cat(paste0("metricsMap: read from cache ",fname,"\n"))
      session_grid(NULL)
      return(read_envi(fname))
   }
   ic <- concernSubset(concern,ctable=ctable,activity=industry,group=group,season=season)
   ic <- concernIndex(ic)
   v <- ic$concernNAC
   if (!length(indR <- do.call(c,lapply(group,grep,substr(rownames(v),1,1)))))
      indR <- seq_len(nrow(v))
   industryList <- colnames(v) |> industryName()
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
   cf <- names(v)
   resNAO <- res <- puvspr[puvspr$species %in% cf,]
   rownames(res) <- NULL
   rownames(resNAO) <- NULL
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
   res$value <- res$amount*v[indCF]/amount[indAmount]
   res$sumNAC <- v[indCF] ## comment it
   res$total_amount <- amount[match(res$species,names(amount))] ## comment it
  # print(res)
   res <- by(res,res$pu,function(x) {
      sum(x$value)
   }) |> c()
  # print(summary(res))
   vNAO <- ic$concernNAO[sort(indR),sort(indC),drop=FALSE]
   vNAO <- rowSums(vNAO,na.rm=TRUE)
   resNAO$value <- resNAO$amount*vNAO[indCF]/amount[indAmount]
   resNAO <- by(resNAO,resNAO$pu,function(x) sum(x$value)) |> c()
   ind <- which(!is.na(match(as.character(pu$ID),names(res))))
   res2 <- pu[ind,,drop=FALSE]
   spatial_data(res2) <- data.frame(PU=pu$ID[ind],NAOR=resNAO,NACR=res)
   sf::st_agr(res2) <- "constant"
  # str(res2)
  # print(summary(res2$NAC))
  # res3 <- res2[res2$NAC>quantile(res2$NAC,0.9999),]
  # str(res3)
   if (FALSE)
      return(res2)
   session_grid(dist2land)
   res2 <- spatial_centroid(res2) |> allocate()
   write_envi(res2,fname)
   res2
}
# 'selectRegion' <- function(data=names(regionSF),region="(\\s(22)|Barents)") {
'selectRegion' <- function(region="PAC 22") {
   if (is.null(region))
      return(NULL)
   if (is_spatial(region))
      return(region)
   rname <- names(regionSF)
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
   aoi <- regionSF[[b["data"]]]
   if (!length(ind <- grep(b["reg"],aoi$region,ignore.case=TRUE)))
      return(NULL)
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
                 ,concern=c(100,10,1),quantile=c(10,90),ncolor=7L)
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
   prm_read(commentFile)
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
      db <- rdrop2::drop_auth(rdstoken=rdstoken)
      wd_ <- setwd(dirname(file))
      rdrop2::drop_upload(basename(file),verbose=F)
      setwd(wd_)
   }
   0L
}
'prm_download' <- function(file) {
   if ((isRemote)&&(T | !prm_exists(file))) {
      db <- rdrop2::drop_auth(rdstoken=rdstoken)
      wd_ <- setwd(dirname(file))
      try(rdrop2::drop_download(basename(file),overwrite=TRUE,verbose=F))
      setwd(wd_)
   }
   0L
}
'prm_read' <- function(file) {
   prm_download(file)
   ret <- jsonlite::fromJSON(file)
   ret
}
'prm_write' <- function(prm,file) {
   if (file.exists(file)) {
      prm0 <- jsonlite::fromJSON(file)
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
   rownames(d) <- NULL
   d$industry <- as.character(d$industry)
   if (!is.null(ctable)) {
      if (T) { ## ArcNet CF List
         gr <- attr(ctable,"group")
         if (!all(nchar(gr)==4)) {
            list1 <- sort(unique(d$CF_code))
            listCF <- lapply(gr,\(x) grep(paste0("^",x),list1,value=TRUE)) |>
               do.call(c,args=_) |> sort()
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
            d <- d[gr,]
         }
      }
      if (!is.null(season)) {
         ind <- which(substr(season,1,3) %in% substr(tail(seasonList,-1),1,3))
         if (length(ind)) {
            if (verbose) {
               str(d)
               str(season)
               str(tail(seasonList,-1))
               str(c('season:'=ind))
            }
            d <- d[d$month %in% ind,]
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
'concernIndex' <- function(concern,group=NULL,activity=NULL,season=NULL) {
   if (isShiny)
      cat("concernIndex():\n")
   mulNA <- config$concern
   maxVal <- max(mulNA)
   concernFile <- file.path("requests",paste0("i"
      ,digest::digest(list(mulNA=mulNA,concern=concern,missing=ignoreMissing),"crc32")
                                      ,".rds"))
   if (T & file.exists(concernFile)) {
      print(concernFile)
      return(readRDS(concernFile))
   }
   ursa:::.elapsedTime("concernIndex -- start")
   ursa:::.elapsedTime("concernIndex -- finish") |> on.exit()
   concern$industry <- as.character(concern$industry)
   seasonLength <- length(unique(concern$month))
   if (isShiny)
      shiny::showNotification(id="concernIndex",closeButton=FALSE,duration=120
                      ,paste("Your 'concern' request is at first time."
                            ,"Please wait his finalizing."
                            ,"It will be processed faster next time...")
                      ,type="warning")
   concernNAR <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '3'))) |>
                 unclass() |> data.frame()
   concernNAY <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '2'))) |>
                 unclass() |> data.frame()
   concernNAG <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                           ,function(x) length(which(x$value %in% '1'))) |>
                 unclass() |> data.frame()
   if (ignoreMissing) {
      concernMissed <- by(concern,list(CF=concern$CF_code,industry=concern$industry)
                                 ,function(x) {
         y <- length(which(!is.na(x$value)))
         y[y==0] <- NA
         y
      }) |> unclass() |> data.frame()
     # mul <- length(unique(concern$month))
     # print(concernMissed["1014",])
     # print(sum(concernMissed["1014",],na.rm=TRUE))
      concernNAR <- concernNAR/concernMissed*seasonLength
      concernNAY <- concernNAY/concernMissed*seasonLength
      concernNAG <- concernNAG/concernMissed*seasonLength
   }
  # concernNAM <- mulNA[1]*concernMissed
   concernNAO <- mulNA[1]*concernNAR
   concernNAC <- mulNA[1]*concernNAR+mulNA[2]*concernNAY+mulNA[3]*concernNAG
  # print(concernNAC["1014",])
  # print(sum(concernNAC["1014",],na.rm=TRUE))
   meanNAOR <- sum(colSums(concernNAO,na.rm=TRUE))/spatial_count(pu)
   meanNACR <- sum(colSums(concernNAC,na.rm=TRUE))/spatial_count(pu)
   if (F) {
      sumNAC <- rowSums(concernNAC,na.rm=TRUE)/ncol(concernNAC)/max(concernNAC,na.rm=TRUE)
      sumNAO <- rowSums(concernNAO,na.rm=TRUE)/ncol(concernNAO)/max(concernNAO,na.rm=TRUE)
   } else if (ignoreMissing) {
      sumNAC <- rowSums(concernNAC,na.rm=TRUE)/rowSums(concernMissed,na.rm=TRUE)/maxVal
      sumNAO <- rowSums(concernNAO,na.rm=TRUE)/rowSums(concernMissed,na.rm=TRUE)/maxVal
   } else {
      d <- max(config$concern)*seasonLength
      sumNAC <- apply(concernNAC,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
      sumNAO <- apply(concernNAO,1,\(v) sum(v,na.rm=TRUE)/sum(!is.na(v))/d)
      rm(d)
   }
   ret <- list(concernNAO=concernNAO,concernNAC=concernNAC
              ,meanNAOR=meanNAOR,meanNACR=meanNACR
              ,sumNAO=sumNAO,sumNAC=sumNAC
              )
   if (!dir.exists(dirname(concernFile)))
      dir.create(dirname(concernFile),recursive=TRUE)
   saveRDS(ret,concernFile)
   if (isShiny)
      removeNotification(id="concernIndex")
   ret
}
'huAmount' <- function(verbose=FALSE) {
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
                       ,verbose=FALSE) {
   metricsMap(ctable=ctable,group=group,industry=activity,season=season)["NACR"]
}
'indexNAOR' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,verbose=FALSE) {
   metricsMap(ctable=ctable,group=group,industry=activity,season=season)["NAOR"]
}
'indexHumanUse' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,verbose=FALSE) {
   if (!is.null(ctable)) {
     # cs <- concernSubset(concern,ctable=ctable) ## quick, but CFs not for domain
      cs <- concernSubset(concern,ctable=NULL
                         ,group=attr(ctable,"group")
                         ,activity=attr(ctable,"industry")
                         ,season=attr(ctable,"season")
                         )
   }
   else
      cs <- concernSubset(concern,ctable=NULL,group=group,activity=activity,season=season)
   ind <- which(names(rHU) %in% unique(cs$industry))
   if (!length(ind))
      return(c(humanUse=ursa(0)))
   res <- c(domain=local_sum(rHU[ind]))
   if (!is.null(aoi)) {
      session_grid(res)
      rAOI <- !is.na(puAOI(aoi) |> spatial_centroid() |> allocate())
      return(c(res,aoi=res[rAOI]))
   }
   res
}
'indexCAPR' <- function(aoi=NULL,ctable=NULL,group=NULL,activity=NULL,season=NULL
                       ,maxNAC=FALSE,verbose=FALSE) {
   ursa:::.gc(isShiny)
   if (!is.null(ctable)) {
     # cs <- concernSubset(concern,ctable=ctable) ## quick, but CFs not for domain
      cs <- concernSubset(concern,ctable=NULL
                         ,group=attr(ctable,"group")
                         ,activity=attr(ctable,"industry")
                         ,season=attr(ctable,"season")
                         )
   }
   else
      cs <- concernSubset(concern,ctable=NULL,group=group,activity=activity,season=season)
   ci <- concernIndex(cs)
   concernNAC <- ci$concernNAC
   if (maxNAC) {
      nmonth <- length(unique(cs$month))
      concernNAC <- as.data.frame((!is.na(concernNAC))*config$concern[1]*nmonth)
   }
   listCF <- rownames(concernNAC)
  # listCF <- sort(unique(concern$CF_code))
  # str(puvspr)
  # cf <- puvspr[]
   cap <- rHU ## huAmount() rHU
   if (F & maxNAC)
      cap <- !is.na(cap)
   session_grid(cap)
   ind <- match(names(cap),colnames(concernNAC))
   if (all(is.na(ind))) {
      session_grid(cap)
      return(c(domain=ursa(0),aoi=ursa(0)))
   }
   concernNAC <- concernNAC[,na.omit(ind),drop=FALSE]
   cap <- cap[which(!is.na(ind))]
   fname <- file.path("requests"
           ,paste0("p",digest::digest(list(cap=cap,concern=concernNAC)
                                     ,"crc32")))
   if (!envi_exists(fname)) {
      if (isShiny)
         shiny::showNotification(id="indexCAPR",closeButton=FALSE,duration=120
                         ,paste("Your 'CAPR' request is at first time."
                               ,"Please wait his finalizing."
                               ,"It will be processed faster next time...")
                         ,type="warning")
      if (verbose) {
         print(series(concernNAC,8))
         print(cap)
      }
      if (T) { ## via vector
         ursa:::.elapsedTime("HU amount via vector -- start")
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
         print("0212a")
        # pr$coverage <- NULL
        # res1 <- by(puvspr,puvspr$species,\(x) max(x$amount))
        # print(summary(res1))
         cp <- c(pu=allocate(spatial_centroid(pu["ID"])),cap)
         cp <- polygonize(cp)
         cp <- spatial_data(cp)
         print("0212b")
         nac <- cbind(species=as.integer(rownames(concernNAC)),concernNAC)
         res1 <- merge(pr,cp,by="pu")
         res2 <- merge(res1,nac,by="species")
         rm(res1)
         ursa:::.gc(isShiny)
         rname <- colnames(res2)
         aname <- gsub("\\..$","",rname)
         ta <- table(aname)
         tname <- names(ta[ta==2])
         print("0212c")
         ##~ str(pr)
         ##~ str(cp)
         ##~ str(nac)
         if (T) {
            xname <- paste0(tname,".x")
            yname <- paste0(tname,".y")
            print("0212d")
           # res2x <- res2[,paste0(tname,".x"),drop=FALSE]
           # print("0212e")
           # res2y <- res2[,paste0(tname,".y"),drop=FALSE]
            res2z <- res2[,paste0(tname,".x"),drop=FALSE]*
                     res2[,paste0(tname,".y"),drop=FALSE]
            print("0212f")
            res2z <- res2z*res2[["amount"]]
            print("0212g")
            ursa:::.gc(isShiny)
            colnames(res2z) <- tname
            print("0212h")
            res2 <- cbind(res2[,-match(c(xname,yname),colnames(res2))],res2z)
            print("0212i")
            rm(res2z)
            ursa:::.gc(isShiny)
         }
         else {
            for (i in seq_along(tname)) {
               message("DO MATRIX BY MATRIX MULTIPLICATION WITHOUT LOOP")
               cname1 <- paste0(tname[i],".x")
               cname2 <- paste0(tname[i],".y")
               res2[[tname[i]]] <- res2[[cname1]]*res2[[cname2]]*res2[["amount"]]
               res2[[cname1]] <- NULL
               res2[[cname2]] <- NULL
            }
         }
        # print(res2)
        # print(sum(res2$amount))
         if (T) {
           # res3 <- aggregate(res2,by=list(pu=res2$pu),function(x) {
           #    str(x)
           # })
            print("0212j")
            res3 <- by(res2[,tname],res2$pu,simplify=!FALSE,\(x) {
               sum(unlist(x),na.rm=TRUE)
            })
            print("0212k")
            rm(res2)
            ursa:::.gc(isShiny)
            print("0212l")
           # str(res3)
           # print(sum(res3))
           # print(mean(res3))
           # print(sum(res3)/spatial_count(pu))
            res3 <- data.frame(pu=as.integer(names(res3)),value=as.numeric(res3))
            print("0212m")
            a5 <- spatial_geometry(pu)[match(res3$pu,pu$ID)]
            print("0212n")
            spatial_data(a5) <- data.frame(domain=res3$value)
            print("0212o")
            a5 <- spatial_centroid(a5) |> allocate()
            print("0212p")
         }
         ursa:::.elapsedTime("HU amount via vector -- finish")
      }
      else { ## via raster
         ursa:::.elapsedTime("HU amount via raster -- start")
         a <- open_envi("requisite/amount",cache=TRUE)
        # a6 <- a["1007"]
        # print(as.data.frame(c(pu=dist2land["ID"][a6],a6=a6))[,-c(1,2)])
         aname <- names(a)
         cl <- length(chunk_band(a,30)[[1]])
         nbreak <- ceiling(length(listCF)/cl)
        # str(cl)
        # str(nbreak)
         if (nbreak==1)
            cl <- rep(1L,length(listCF))
         else
            cl <- cut(seq_along(listCF),breaks=nbreak) |> as.integer()
        # str(cl)
         a4 <- ursa(nband=length(unique(cl)))
         for (i in seq_along(unique(cl)) |> sample()) {
            a2 <- a[listCF[cl==i]]
           # print(a2)
           # print(as.data.frame(c(pu=dist2land["ID"][a2],a2=a2))[,-c(1,2)])
            bname <- names(a2)
            a3 <- ursa(bandname=bname)
            for (j in seq(a2) |> sample()) {
              # a3[j] <- local_sum(a2[j]*cap*unlist(concernNAC[bname[j],]),cover=0)
               a5 <- a2[j]*cap*unlist(concernNAC[bname[j],])
              # print(a5)
               a3[j] <- local_sum(a5,cover=0)
            }
           # print(a2)
           # print(a3)
            a4[i] <- local_sum(a3,cover=0)
         }
         close(a)
        # q()
         a5 <- c(domain=local_sum(a4,cover=0))
         ursa:::.elapsedTime("HU amount via raster -- finish")
      }
      if (F) {
         print(as.data.frame(c(pu=dist2land["ID"][a5],a5=a5))[,-c(1,2)])
      }
      if (F)
         display(ursa_crop(a5,border=1))
      write_envi(a5,fname)
      if (isShiny)
         removeNotification(id="indexCAPR")
   }
   else {
      cat("read CAP from cache",fname,"\n")
      a5 <- read_envi(fname,resetGrid=TRUE)
   }
   if (!is.null(aoi)) {
      rAOI <- !is.na(puAOI(aoi) |> spatial_centroid() |> allocate())
      return(c(a5,aoi=a5[rAOI]))
   }
   return(a5)
   if (F) {
      listI <- colnames(concernNAC)
      if (verbose)
         print(rHU)
     # rHU <- band_mean(rHU)
      if (verbose)
         print(rHU)
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
      cap
      return(cap)
   }
   return(NULL)
}
