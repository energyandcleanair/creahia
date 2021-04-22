library(rgdal)
library(sp)
library(rgeos)
library(reshape2)
library(raster)
library(gstat)
library(rworldmap)
library(maps)

getPuffCSVs <- function(ext=".csv",gasunit="ug") {
  ext<-gsub("^\\.","\\\\.",ext)
  files <- list.files(pattern=paste0("rank.*",ext))
  fileSpecs <- data.frame(name = files, scale = 1,stringsAsFactors = F)
  cbind(fileSpecs,colsplit(files,"_",c("X1","species","hr","type","scenario"))[,-1]) -> fileSpecs
  fileSpecs$type[fileSpecs$type=="conc"] <- "concentration" 
  fileSpecs$unit <- "ug/m3" 
  fileSpecs[grep("tflx",fileSpecs$name),"type"] <- "deposition"
  fileSpecs[grep("tflx",fileSpecs$name),"scale"] <- 8760*3600/1e9*1e4
  fileSpecs[grep("tflx",fileSpecs$name),"unit"] <- "kg/ha/yr"
  
  fileSpecs[fileSpecs$species == 'hg','scale'] <- fileSpecs[fileSpecs$species == 'hg','scale'] * 1e3
  fileSpecs[fileSpecs$species == 'hg','unit'] <- "mg/ha/yr"
  
  fileSpecs$hr <- as.numeric(gsub("[_hr]","",fileSpecs$hr))
  fileSpecs$FUN <- "mean"
  fileSpecs[fileSpecs$hr<=24,"FUN"] <- "max"
  fileSpecs$scenario <- gsub(ext,"",fileSpecs$scenario)
  
  fileSpecs$period <- NA
  fileSpecs[fileSpecs$hr ==1,"period"] <- "hourly"
  fileSpecs[fileSpecs$hr ==24,"period"] <- "daily"
  fileSpecs[fileSpecs$hr > 7000,"period"] <- "annual"
  
  fileSpecs$speciesName <- toupper(fileSpecs$species)
  fileSpecs[fileSpecs$species == "so2eq","speciesName"] <- "acid"
  fileSpecs[fileSpecs$species == "pm25","speciesName"] <- "PM2.5"
  fileSpecs[fileSpecs$species == "tpm10","speciesName"] <- "PM10"
  fileSpecs[fileSpecs$species == "so2eq","unit"] <- paste0(fileSpecs[fileSpecs$species == "so2eq","unit"]," SO2-equivalent")
  fileSpecs[fileSpecs$species == "pm","speciesName"] <- "fly ash"
  fileSpecs[fileSpecs$species == "hg","speciesName"] <- "mercury"
  
  #scaling for non-standard units
  fileSpecs$plotscale <- 1
  fileSpecs$plotunit <- fileSpecs$unit
  
  if(gasunit=='ppb') {
    fileSpecs[fileSpecs$speciesName=="SO2","plotscale"] <- 0.355 
    fileSpecs[fileSpecs$speciesName=="NO2","plotscale"] <- 0.494
    fileSpecs[fileSpecs$speciesName=="SO2","plotunit"] <- 'ppb' 
    fileSpecs[fileSpecs$speciesName=="NO2","plotunit"] <- 'ppb'
  }
  
  #exceedance thresholds - these will be recorded and included as a threshold level in contour plots
  fileSpecs$threshold <- NA
  fileSpecs[fileSpecs$speciesName=="SO2" & fileSpecs$hr==24,"threshold"] <- 20 #WHO
  fileSpecs[fileSpecs$speciesName=="NO2" & fileSpecs$hr==1,"threshold"] <- 200 #WHO
  fileSpecs[fileSpecs$speciesName=="PM2.5" & fileSpecs$hr==24,"threshold"] <- 25 #WHO
  fileSpecs[fileSpecs$speciesName=="PM10" & fileSpecs$hr==24,"threshold"] <- 50 #WHO
  fileSpecs[fileSpecs$speciesName=="mercury" & fileSpecs$type=="deposition","threshold"] <- 125 #Great lakes study
  fileSpecs[fileSpecs$speciesName=="SO2" & fileSpecs$hr==1,"threshold"] <- 75/0.355 #U.S. NAAQS
  fileSpecs$threshold.plotunit <- fileSpecs$threshold * fileSpecs$plotscale
  
  return(fileSpecs)
}

ordermerge <- function(x,y,...) {
  x$riw <- 1:nrow(x)
  merge(x,y,...) -> x
  return(x[order(x$riw),-which(colnames(x)=="riw")])
}

makeGrid <- function(runName=NULL,
                     UTMz=get('UTMz',envir=.GlobalEnv),
                     UTMhem=get('UTMH',envir=.GlobalEnv),
                     mapres=get('mapres',envir=.GlobalEnv),
                     filename=NULL) {
  if(is.null(runName)) runName <- fileSpecs[1,'scenario']
  if(is.null(filename)) 
    filename <-     fileSpecs[fileSpecs$species=="pm25" & 
                                fileSpecs$hr >24 & 
                                fileSpecs$scenario %in% runName, "name"][1]
  
  poll <- read.table(filename,
                     skip=7,header=F,col.names=c("Xkm","Ykm","PM25"),sep=",")
  
  pollSP <- SpatialPointsDataFrame(coords = subset(poll,select=c(Xkm,Ykm)), 
                                   data = subset(poll,select=-c(Xkm,Ykm)),
                                   proj4string = CRS(paste0("+proj=utm +zone=",UTMz,
                                                            ifelse(UTMhem=="S"," +south",""),
                                                            " +datum=WGS84 +units=km +no_defs")))
  domain <- extent(pollSP)
  res <- (domain@xmax - domain@xmin)/49
  domain <- extend(domain,res/2)
  
  r <- raster(domain, resolution=mapres, crs=crs(pollSP))
  
  gridSP <<- as(r, 'SpatialPixels')
  gridR <<- raster(gridSP)
}


makeMap <- function(gridR,shp=NULL,admin_level=0, ...) {
  require(rworldmap)
  gridLL <- projectRaster(gridR,crs = proj4string(countriesLow))
  gridLL <<- extend(gridLL,c(40,40))
  
  if(is.null(shp)) {
    admLL <- getadm(admin_level, ...)
  } else admLL <- shp
  
  admUTM <<- spTransform(crop(admLL,gridLL),CRS(proj4string(gridR)))
  
  cities <- shapefile(GISpath("citiesDistToLarge.shp"))
  
  citySP <- crop(cities,gridLL)
  cityUTM <<- spTransform(citySP,CRS(proj4string(gridR)))
}


makePop <- function(gridR=get('gridR',envir=.GlobalEnv),
                    makeGrump=F,grumpPath=NULL) {
  pop <- raster("~/GIS/population/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2015.tif")
  
  if(!exists('gridLL')) gridLL <- projectRaster(gridR,crs = proj4string(countriesLow)) %>% 
      extend(c(40,40))
  popC <- crop(pop,gridLL)
  popUTM <- projectRaster(popC,crs = CRS(proj4string(gridR)))
  popD_CP <- resample(popUTM,gridR)
  popCP <- popD_CP  * area(popD_CP)
  names(popCP) <- "pop"
  
  if(makeGrump) {
    if(is.null(grumpPath)) {
      tryPaths <- c(GISpath('../HIA/GRUMPv1/glurextents.bil'))
      for(tryPath in tryPaths)
        if(file.exists(tryPath)) tryPath -> grumpPath
    }
    
    if(is.null(grumpPath) | !file.exists(grumpPath))
      stop('GRUMP data not found')
    
    raster(grumpPath) -> grump
    crs(grump) <- crs(countriesLow)
    crop(grump,gridLL) -> grump_C
    projectRaster(grump_C,gridR,method='ngb') ->> grumpUTM
  }
  
  return(popCP)
}

readCALPOST = function(csvfile) {
  readLines(csvfile, n=10) -> inlines
  startline = inlines %>% gsub(" ", "", .) %>% nchar %>% equals(0) %>% which %>% '['(2)
  read.table(csvfile, skip=startline,header=F,sep=",")
}

#output tiffs
makeTifs <- function(fileSpecs,
                     ext='',
                     queue=NULL, 
                     subsets = F,
                     max.ranks = 1,
                     overwrite=F, 
                     nmax=4, idp=2, ...) {
  if(is.null(queue)) queue = 1:nrow(fileSpecs)
  
  
  files = fileSpecs$name
  
  if(subsets %>% typeof == "logical") {
    if(subsets) { subsets <- c('_g','_d')
    } else subsets <- ''
  }
  
  queue <- queue[queue %in% grep(subsets[1], fileSpecs$name)]
  
  for(file in queue) {
    ranks <- 1
    if(grepl('rank\\(all)', files[file])) ranks <- max.ranks
    for(rank.n in 1:ranks) {
      files[file] %>% gsub("\\.csv",paste0(ext, ".tif"),.) %>% 
        gsub(subsets[1],"",.) %>% 
        gsub('rank\\(all)', paste0('rank(',rank.n,')'), .) -> rfile
      
      if(file.exists(rfile)) warning('file exists')
      if(!file.exists(rfile) | overwrite) {
        inF <- sapply(subsets, function(x) gsub(subsets[1],x,files[file]))
        
        #create interpolated raster
        inF %>% lapply(readCALPOST) %>% do.call(rbind, .) -> poll
        poll[, c(1:2, 2+rank.n)] -> poll
        colnames(poll) = c("Xkm","Ykm","conc")
        poll$conc <- fileSpecs[file,"scale"] * poll$conc
        
        pollSP <- SpatialPointsDataFrame(coords = subset(poll,select=c(Xkm,Ykm)), 
                                         data = subset(poll,select=-c(Xkm,Ykm)),
                                         proj4string = CRS(proj4string(gridSP)))
        pollSP %<>% crop(extent(gridR)+30.1)
        
        conc_krige <- idw(as.formula(paste0("conc"," ~ 1")), 
                          pollSP, gridSP,nmax=nmax, idp=idp, ...)
        conc_R <- raster(conc_krige,values=T)
        conc_R %<>% crop(gridR)
        
        writeRaster(conc_R,rfile,format="GTiff",overwrite=T)
        plot(conc_R, main=rfile)
        print(paste(files[file],'processed'))
      } 
    }
  }
}


sigfloor <- function(x,sigdig=1) {
  mag <- 10^floor(log10(x)-sigdig+1)
  return(floor(x/mag)*mag)
}


textbuffer <- function(coords,width=5,steps=8) {
  theta <- seq(0, 2 * pi, length.out = steps + 1)[-1]
  coords %>% 
    matrix(ncol=2) %>% 
    alply(1, 
          function(c.in) {
            matrix(c(c.in[1] + width * cos(theta),
                     c.in[2] + width * sin(theta)),
                   ncol=2)
          }) %>% 
    do.call(rbind, .)
}

#'Create contour polygons from a raster
#'
#' credit: StackOverflow user 'Paul Regular'
#' @param r Input raster
#' @param levels Contour levels. Numeric vector, or "auto" to set automatically (default).
#' @export
raster2contourPolys <- function(r, levels = NULL) {
  require(maptools)
  ## set-up levels
  if(levels != "auto") {
    levels <- sort(levels)
    plevels <- c(min(values(r), na.rm=TRUE), levels, max(values(r), na.rm=TRUE)) # pad with raster range
    llevels <- paste(plevels[-length(plevels)], plevels[-1], sep=" - ")  
    llevels[1] <- paste("<", min(levels))
    llevels[length(llevels)] <- paste(">", max(levels))
  } else levels = NULL
  
  ## convert raster object to matrix so it can be fed into contourLines
  xmin <- extent(r)@xmin
  xmax <- extent(r)@xmax
  ymin <- extent(r)@ymin
  ymax <- extent(r)@ymax
  rx <- seq(xmin, xmax, length.out=ncol(r))
  ry <- seq(ymin, ymax, length.out=nrow(r))
  rz <- t(as.matrix(r))
  rz <- rz[,ncol(rz):1] # reshape
  
  ## get contour lines and convert to SpatialLinesDataFrame
  cat("Converting to contour lines...\n")
  cl <- contourLines(rx,ry,rz,levels=levels) 
  if(length(cl)==0) { warning('levels too high - no contours generated'); return(NULL) }
  
  #convert to contour lines while catching 'too short' error
  tryCatch(cl <- ContourLines2SLDF(cl), 
           error = function(e) {
             if(grepl('too short',as.character(e))) { 
               warning('levels too high - no contours generated'); return(NULL)
             } else stop('unknown error') 
           } )
  
  
  ## extract coordinates to generate overall boundary polygon
  xy <- coordinates(r)[which(!is.na(values(r))),]
  i <- chull(xy)
  b <- xy[c(i,i[1]),]
  b <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), "1")))
  
  ## add buffer around lines and cut boundary polygon
  cat("Converting contour lines to polygons...\n")
  bcl <- gBuffer(cl, width = 0.0001) # add small buffer so it cuts bounding poly
  cp <- gDifference(b, bcl)
  
  ## restructure and make polygon number the ID
  polys <- list() 
  for(j in seq_along(cp@polygons[[1]]@Polygons)) {
    polys[[j]] <- Polygons(list(cp@polygons[[1]]@Polygons[[j]]),j)
  }
  cp <- SpatialPolygons(polys)
  cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)))
  
  ## cut the raster by levels
  rc <- cut(r, breaks=plevels)
  
  ## loop through each polygon, create internal buffer, select points and define overlap with raster
  cat("Adding attributes to polygons...\n")
  l <- character(length(cp))
  for(j in seq_along(cp)) {
    p <- cp[cp$id==j,] 
    bp <- gBuffer(p, width = -max(res(r))) # use a negative buffer to obtain internal points
    if(!is.null(bp)) {
      xy <- SpatialPoints(coordinates(bp@polygons[[1]]@Polygons[[1]]))[1]
      l[j] <- llevels[raster::extract(rc,xy)]
    } 
    else { 
      xy <- coordinates(gCentroid(p)) # buffer will not be calculated for smaller polygons, so grab centroid
      l[j] <- llevels[raster::extract(rc,xy)]
    } 
  }
  
  ## assign level to each polygon
  cp$level <- factor(l, levels=llevels)
  cp$min <- plevels[-length(plevels)][cp$level]
  cp$max <- plevels[-1][cp$level]  
  cp <- cp[!is.na(cp$level),] # discard small polygons that did not capture a raster point
  if(nrow(cp)==0) return(NULL)
  df <- unique(cp@data[,c("level","min","max")]) # to be used after holes are defined
  df <- df[order(df$min),]
  row.names(df) <- df$level
  llevels <- df$level
  
  ## define depressions in higher levels (ie holes)
  
  if(length(llevels) > 1) {
    cat("Defining holes...\n")
    spolys <- list()
    p <- cp[cp$level==llevels[1],] # add deepest layer
    p <- gUnaryUnion(p)
    spolys[[1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[1])
    for(i in seq(length(llevels)-1)) {
      p1 <- cp[cp$level==llevels[i+1],] # upper layer
      p2 <- cp[cp$level==llevels[i],] # lower layer
      x <- numeric(length(p2)) # grab one point from each of the deeper polygons
      y <- numeric(length(p2))
      id <- numeric(length(p2))
      for(j in seq_along(p2)) {
        xy <- coordinates(p2@polygons[[j]]@Polygons[[1]])[1,]
        x[j] <- xy[1]; y[j] <- xy[2]
        id[j] <- as.numeric(p2@polygons[[j]]@ID)
      }
      xy <- SpatialPointsDataFrame(cbind(x,y), data.frame(id=id))
      holes <- over(xy, p1)$id
      holes <- xy$id[which(!is.na(holes))]
      if(length(holes)>0) {
        p2 <- p2[p2$id %in% holes,] # keep the polygons over the shallower polygon
        p1 <- gUnaryUnion(p1) # simplify each group of polygons
        p2 <- gUnaryUnion(p2)
        p <- gDifference(p1, p2) # cut holes in p1      
      } else { p <- gUnaryUnion(p1) }
      spolys[[i+1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[i+1]) # add level 
    }
  }

  cp <- SpatialPolygons(spolys, pO=seq_along(llevels), proj4string=CRS(proj4string(r))) # compile into final object
  try(cpdf <- SpatialPolygonsDataFrame(cp, df,match.ID = T))
  if(!exists('cpdf')) cpdf <- SpatialPolygonsDataFrame(cp, df,match.ID = F)
  cat("Done!")
  cpdf
  
}

initkml <- function() {
  if(!file.exists("zip.exe"))
    file.copy(boxpath("tools&templates/zip.exe"),"zip.exe")
  labelF <- "factoryTransp3.png"
  if(!file.exists(labelF))
    file.copy(paste0("~/../Desktop/Box Sync/tools&templates/",labelF),labelF)
}


writeConcKML <- function(outFileName,plotTitle=outFileName,
                         contours,lvls,fileSpecs,
                         CFPPplot=get('CFPPplot',envir=.GlobalEnv),
                         sourceNameCol='Source.Name',
                         times=NULL,
                         initFile=T,closeFile=T,
                         leaveLabels=F, #should the label image files be left in the directory for checking
                         labelSize=.5,
                         iconScale=.5,
                         iconScaleCol=NULL) {
  require(plotKML)
  initkml()
  colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb
  
  #open file for writing and make label
  if(initFile) {
    legendlvls <- lvls
    #legendlvls[length(lvls)] <- paste0(legendlvls[length(lvls)],fileSpecs[,"plotunit"])
    plotTitle <- paste0(plotTitle,' (',fileSpecs[,"plotunit"],')')
    labwidth <- max(sum(nchar(legendlvls))*56,
                    nchar(plotTitle)*11)
    
    png("label.png",width=labwidth,height=100,pointsize=18,bg = "transparent")
    
    par(mar = rep(.5, 4))
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    legend("topleft", legend = legendlvls, col=yorb, pch = 15,
           xjust=0.5, yjust=0,horiz=T,title = plotTitle,bg="white"
    )
    dev.off()
    
    kml_open(file.name=paste0(outFileName,".kml"))
  }
  
  #write contours
  if(!is.null(contours)) {
    if(!is.list(contours))
      list(contours) -> contours
    
    
    formatTime <- function(x) ifelse(is.null(x),NULL,format(x,'%Y-%m-%dT%H:%M:%SZ'))
    for(i in which(sapply(contours,nrow) > 0)) {
      
      if(!is.null(times)) {
        start.end <- c(start=formatTime(times[i]),
                       end=formatTime(times[i+1]-1))
      } else start.end <- NULL
      
      contours[[i]]$colN <- rank(contours[[i]]$max)
      kml_layer(obj=contours[[i]], subfolder.name=fileSpecs[,"plotunit"], 
                colour=colN,
                colour_scale=c("steelblue","yellow","orange","red","darkred"),
                alpha=0.5,altitude=0,plot.labpt=F,
                labels=level,LabelScale=0.5,
                TimeSpan.begin=start.end['start'],
                TimeSpan.end=start.end['end'])
    }
  }
  

  if(closeFile) {
    if(!is.null(iconScaleCol)) {
      CFPPplot$iconScale = iconScale(CFPPplot[[iconScaleCol]])
    } else CFPPplot$iconScale = iconScale
    
    if(!is.null(labelSize)) {
      CFPPplot$KMLlabel <- enc2utf8(as.character(CFPPplot@data[[sourceNameCol]]))
      CFPPplot$labelSize <- labelSize
    } else { 
      CFPPplot$KMLlabel <- ''
      CFPPplot$labelSize <- .5
    }
    
    kml_layer(obj=CFPPplot, subfolder.name="Modeled sources", 
            size=iconScale,
            alpha=1,altitude=0,
            labels=KMLlabel,
            LabelScale=labelSize,sname="labels",shape="factoryTransp3.png")
    kml_screen(image.file="label.png",position="UL",sname="Label")
    kml_close(file.name=paste0(outFileName,".kml"))
    zip(paste0(outFileName,".kmz"),c(paste0(outFileName,".kml"),"factoryTransp3.png","label.png"))
    file.remove(paste0(outFileName,".kml"))
    
    if(leaveLabels) {
      file.rename("label.png",paste0('label-',outFileName,'.png'))
    } else file.remove("label.png")
  }
}

cropProj <- function(shapeobj, rasterobj, expand=1.25, ...) {
  shapeobj %>% 
    crop(extent(projectExtent(rasterobj, 
                              crs(shapeobj)))*expand) -> shapeobj
  if(grepl("Raster", class(shapeobj))) {
    shapeobj %>% projectRaster(rasterobj, ...) %>% return
  } else shapeobj %>% spTransform(crs(rasterobj)) %>% return
}
