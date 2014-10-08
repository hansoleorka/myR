#' Read workbook from excel files 
#' 
#' Many people use excel files to store data in. This function reads a specified excel worksheet into a data.frame.   
#' 
#' @param filename full path to excel file.
#' @param header logical value for including the header from the excel sheet or not 
#' @param sheet a numeric value indicating which Sheet to be read
#' @return data.frame
#' @seealso \code{\link{XLConnect}}
#' @author Marius Hauglin & Hans Ole &Oslashrka
read.excel<-function(filename,header=TRUE,sheet=1){
	invisible(capture.output(suppressMessages(require("XLConnect",quietly=TRUE)))) 
	
	return(readWorksheetFromFile(filename,sheet=sheet,header=header))
	
}

