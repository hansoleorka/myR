# TODO: Add comment
# TITLE:
#
# Date: Jul 5, 2011 
# Author: hanso
###############################################################################


#Nes


cell.area <- 160
(cell.size <- sqrt(cell.area))
Ymin <- bb[2,1]
Ymax <- bb[2,2]
Xmin <- bb[1,1]
Xmax <- bb[1,2]
crs.string <- proj4string(stands)

Nrow <- ceiling((Ymax - Ymin)/cell.size)
Ncol <- ceiling((Xmax - Xmin)/cell.size)

##----------------------------------------------------------------------------------------
##Fishnet
##----------------------------------------------------------------------------------------
#fishlines <- list()
#for(row in 0:Nrow){
#	for(col in 0:Ncol){
#		#ID <- append(ID,paste(col,row,sep="-"))
#		fishline.x <- Lines(Line(cbind(c(Xmin+(cell.size*col),Xmin+(cell.size*col)),c(Ymin ,Ymin+cell.size*Nrow))),paste(col,row,sep="-"))
#		fishline.y <- Lines(Line(cbind(c(Xmin,Xmin+cell.size*Ncol),c(Ymin+(cell.size*row),Ymin+(cell.size*row)))),paste(row,col,sep="_"))
#		fishlines <- append(fishlines, fishline.x)
#		fishlines <- append(fishlines, fishline.y)
#	}
#}
#Fish.net <- SpatialLines(fishlines)
#plot(Fish.net)
#
##----------------------------------------------------------------------------------------
##Grid Center Points
##----------------------------------------------------------------------------------------
#Grid.center.Points <- c()
#for(row in 1:Nrow-1){
#	for(col in 1:Ncol-1){
#		Grid.center.Points <- rbind(Grid.center.Points,c(x=Xmin+cell.size*col+cell.size/2,y=Ymin+cell.size*row+cell.size/2)) 
#	}
#	Grid.center.Points <- data.frame(Grid.center.Points)
#}
#coordinates(Grid.center.Points) <- ~x+y
#proj4string(Grid.center.Points) <- crs.string
#points(Grid.center.Points)
#
##----------------------------------------------------------------------------------------
#Grid Center Points
#----------------------------------------------------------------------------------------
#Grid.center.Points <- c()
#for(row in 1:Nrow-1){
#	for(col in 1:Ncol-1){
#		Grid.center.Points <- rbind(Grid.center.Points,c(x=Xmin+cell.size*col+cell.size/2,y=Ymin+cell.size*row+cell.size/2)) 
#	}
#	Grid.center.Points <- data.frame(Grid.center.Points)
#}
#coordinates(Grid.center.Points) <- ~x+y
#proj4string(Grid.center.Points) <- crs.string
#points(Grid.center.Points)

#----------------------------------------------------------------------------------------
#Grid Cell
#----------------------------------------------------------------------------------------
#Grid.cell <- SpatialPixels(Grid.center.Points)
#plot(Grid.cell)

Grid.grd <- GridTopology(c(Xmin,Ymin), rep(cell.size,2), c(Ncol,Nrow))
Grid.cell <- SpatialGrid(Grid.grd,proj4string=crs.string)
length(Grid.cell)
Grid.cell2 <- SpatialGridDataFrame(Grid.cell, data.frame(id = 1:length(Grid.cell)),proj4string=crs.string)
library(maptools)
Grid.cell3 <- as(Grid.cell2,"SpatialPixelsDataFrame")

#Celle senter:
CellCenter <- SpatialPoints(Grid.cell2)
proj4string(CellCenter) <- proj4string(stands)
o <- over(Grid.cell3,stands)



CellCenter <- CellCenter[is.na(o$Bestand_ID) == FALSE,]
plot(CellCenter,add=TRUE)

dim(stands)
length(o)

#writeGDAL(Grid.cell2,"D:\\HOME\\PROSJEKT\\Laser-2\\Data\\Nes\\GridCells4.img")
writeGDAL(Grid.cell2,"D:\\HOME\\PROSJEKT\\Laser-2\\Data\\Nes\\GridCells5.tif",driver="GTiff",type="Int32")
#
#library(maptools)
#
Grid.cell.poly <- as(as(Grid.cell, "SpatialPixels"), "SpatialPolygons")
Grid.cell.poly  <- SpatialPolygonsDataFrame(Grid.cell.poly,data.frame(id = 1:length(Grid.cell)))
#writeOGR(Grid.cell.poly ,"D:\\HOME\\PROSJEKT\\Laser-2\\Data\\Nes", "GridCells", driver="ESRI Shapefile")
#
##http://r-sig-geo.2731867.n2.nabble.com/converting-grid-objects-to-spatial-polygon-objects-and-export-as-shapefile-td4143331.html 
#gt <- GridTopology(c(-177.5, -87.5), c(5, 5), c(72, 36))
#grd <- SpatialGrid(gt, proj4string=CRS("+proj=longlat"))
#spix <- as(grd, "SpatialPixels")
#spol <- as(spix, "SpatialPolygons") 

