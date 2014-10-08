#' Point Cloud Viewer - Edited version 
#' 
#' Visualize point colud using rgl
#' 
#' @param pc a matrix with xyz s in coloumns 1 2 3 
#' @param size something other than something else
#' @param col a iteger with the index to the coloumn  or the name of the column used for coloring points
#' @param add logical it adding the 
#' @param background Color of background
#' @param col.column number or name of column used to color points
#' @param type kind of type
#' @param alpha something
#' @param device exisitng device ? 
#' @param grey.out something else
#' @param windowRect  someting more 
#' @param ... other issues to be pased 
#' @author Marius and Hans Ole 
pcv <- function (pc, size = 1, col = "default", add = FALSE, background = "black", 
		col.column = 3, type = "p", alpha = 1, device = rgl.cur(), 
		grey.out = NA, windowRect = c(40, 70, 1000, 1000), ...) 
{
	require("rgl")
	
	#--------------------------------------------------------------------------------------------------------------
	#Bruk av tekst streng for å identifisere col.column 
	#Example:
	#col.column = 4
	#col.colum = "i"
	col.column <- ifelse(is.numeric(col.column),col.column,which(names(pc)==col.column))
	#--------------------------------------------------------------------------------------------------------------
	
	if (!exists("pcv.min", where = .GlobalEnv)) {
		eval({
					pcv.min <<- data.frame(matrix(nrow = 0, ncol = 4))
					names(pcv.min) <<- c("xmin", "ymin", "zmin", "device")
				}, envir = .GlobalEnv)
	}
	if (!exists("pcv.param", where = .GlobalEnv)) {
		eval(pcv.param <<- list(), envir = .GlobalEnv)
	}
	if (!add) {
		r <- nrow(pcv.min) + 1
		pcv.min[r, 1] <<- min(pc[, 1])
		pcv.min[r, 2] <<- min(pc[, 2])
		pcv.min[r, 3] <<- min(pc[, 3])
	}
	else {
		r <- row.names(pcv.min[pcv.min$device == device, ])
	}
	pc[, 1] <- pc[, 1] - pcv.min[r, 1]
	pc[, 2] <- pc[, 2] - pcv.min[r, 2]
	pc[, 3] <- pc[, 3] - pcv.min[r, 3]
	if (is.function(col)) {
		color.ramp <- col
		col <- "default"
	}
	else {
		color.ramp <- colorRampPalette(c("blue", "cyan", "green", 
						"yellow", "orange", "red"))
	}
	if (col[1] == "default") {
		farge.tabell <- color.ramp(100)
		farge <- round(99 * ((pc[, col.column] - min(pc[, col.column]))/(max(pc[, 
												col.column] - min(pc[, col.column]))))) + 1
		farge <- farge.tabell[farge]
		if (!is.na(grey.out[1])) 
			farge[grey.out] <- "grey42"
	}
	else farge <- col
	if (!add) {
		open3d(windowRect = windowRect)
		device = rgl.cur()
		pcv.min$device[r] <<- device
		bg3d(color = background)
		eval(parse(text = paste("pan.begin <- function(x, y) {\n\t\t\t\t\t\t\t\t\t\trgl.set(which=", 
								device, ",silent=TRUE)\t\n\t\t\t\t\t\t\t\t\t\tpcv.param$userMatrix", 
								device, " <<- par3d(\"userMatrix\")\n\t\t\t\t\t\t\t\t\t\tpcv.param$viewport", 
								device, " <<- par3d(\"viewport\")\n\t\t\t\t\t\t\t\t\t\tpcv.param$scale", 
								device, " <<- par3d(\"scale\")\n\t\t\t\t\t\t\t\t\t\tpcv.param$projection", 
								device, " <<- rgl.projection()\n\t\t\t\t\t\t\t\t\t\tpcv.param$pos", 
								device, " <<- rgl.window2user( x/pcv.param$viewport", 
								device, "[3], 1 - y/pcv.param$viewport", device, 
								"[4], 0.5, \n\t\t\t\t\t\t\t\t\t\tprojection=pcv.param$projection", 
								device, ")}", sep = "")))
		eval(parse(text = paste("pan.update <- function(x, y) {\n\t\t\t\t\t\t\t\t\t\txlat <- (rgl.window2user( x/pcv.param$viewport", 
								device, "[3], 1 - y/pcv.param$viewport", device, 
								"[4], 0.5,\n\t\t\t\t\t\t\t\t\t\tprojection = pcv.param$projection", 
								device, ") - pcv.param$pos", device, ")*pcv.param$scale", 
								device, "\n\t\t\t\t\t\t\t\t\t\tmouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])\n\t\t\t\t\t\t\t\t\t\tpar3d(userMatrix = pcv.param$userMatrix", 
								device, " %*% t(mouseMatrix) )\n\t\t\t\t\t\t\t\t\t\t}", 
								sep = "")))
		rgl.setMouseCallbacks(3, pan.begin, pan.update)
	}
	rgl.set(device, silent = TRUE)
	if (type == "p") 
		id <- points3d(pc[, 1], pc[, 2], pc[, 3], size = size, 
				col = farge, alpha = alpha, ...)
	if (type == "s") 
		id <- spheres3d(pc[, 1], pc[, 2], pc[, 3], radius = size, 
				col = farge, alpha = alpha, ...)
	if (is.null(id)) 
		id <- 0
	out <- c(id, device)
	names(out) <- c("id", "device")
	return(out)
}
