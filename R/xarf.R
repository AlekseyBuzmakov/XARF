#' Infer type of XARF based on vector \code{x}
#'
#' @param x is a vector that type to be determined
#'
#' @return
#'
#' @examples
#' inferType(1:10)
inferType = function(x) {
  if( is.factor(x) || is.character(x) ) {
    # A magic constant that makes the file readable by human
    if( length(unique(x)) > 20 ) {
      return(is.factor(x) ? "categoric" : "string")
    } else {
      return(paste0("{",paste0(unique(x),collapse = ", "),"}"))
    }
  }
  if(is.integer(x)) {
    return("integer categorical=true")
  }
  stopifnot(is.numeric(x))
  return("real")
}

#' @title Data Output to XARF
#'
#' @description
#' Function saves a data.frame like objct to XARF format.
#' More details on format can be found in \url{https://bitbucket.org/realKD/realkd/wiki/model/data/xarf}.
#'
#' @details
#' The function takes the dataset and writes it to file \code{file}.
#' The names of the rows are written as the name attribute.
#' The attributes are written in the order given in \code{attrDescription}.
#' If an attribute is missing from \code{attrDescription}, it is written at the end of the dataset.
#'
#' @param x the data.frame to be written
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @param dbID a unique identifier of the dataset
#' @param dbDescription a list specifying the description of the dataset
#' @param attrDescription a vector of descriptions for every attribute
#'
#' @export
#'
#' @examples
#' write.xarf(mtcars, file="",
#'   "mtcars",
#'   dbDescription=list(
#'     title="Motor Trend Car Road Tests",
#'     version="1.0.0",
#'     source="Basic R dataset 'mtcars'"),
#'   attrDescription=list(
#'     list(id="mpg",name="Miles/(US) gallon",type="real",description="The distance in miles a car can go with a single gallon of petrolium"),
#'     list(id="cyl",name="Number of cylinders",type="integer"),
#'     list(id="disp",name="Displacement (cu.in.)")
#'   ))
write.xarf <- function(x, dbID, file="", dbDescription=NULL, attrDescription=NULL) {
  stopifnot(!is.null(dbID) || !is.character(dbID))
  dbID=gsub("[^a-zA-Z0-9]","", dbID)
  stopifnot(dbID != "")

  stopifnot("data.frame" %in% class(x))

  file.close=function(){}
  if(is.character(file)){
    if(file == "") {
      file = stdout()
    } else {
      file=base::file(file,"w+")
      file.close=function(){close(file)}
    }
  }
  # Writing description of the dataset
  if(!is.null(dbDescription)) {
    cat("%",dbDescription$title,"\n", file = file)
    cat("% Version:",dbDescription$version,"\n", file = file)
    cat("% Source:",dbDescription$source,"\n", file = file)
  }
  cat("@relation", dbID, "caption=\"", dbDescription$title,"\"\n", file = file )

  ############################################
  # Writing attributes

  # Attribute for names of rows
  cat("@attribute id name\n", file = file)
  # x.towrite and describedAttributes are to control the attributes we have already written
  # and the order
  x.towrite=data.frame(id=row.names(x))
  describedAttributes = rep(F, ncol(x))
  # Processing known attributes
  for(attr in attrDescription) {
    if(is.null(attr$id) || !is.character(attr$id)) {
      warning(paste0(attr,"has a bad id"))
      next
    }
    attr$id=gsub("[^a-zA-Z0-9]","", attr$id)
    if(attr$id == "") {
      warning(paste0(attr,"has a bad id"))
      next
    }
    if( !(attr$id %in% names(x)) ) {
      warning(paste0(attr$id," is not found in the dataset"))
      next
    }
    describedAttributes[names(x)==attr$id]=T
    x.towrite=cbind(x.towrite,x[[attr$id]])
    cat("@attribute", attr$id, file = file)
    if(!is.null(attr$type) && is.character(attr$type)
       && attr$type %in% c("integer","categoric","string","real","numeric"))
    {
      cat("", attr$type, file = file)
    } else {
      if(!is.null(attr$type)) {
        warning(paste0(attr$type, " is unknown"))
      }
      cat("", inferType(x[[attr$id]]), file = file)
    }
    if(!is.null(attr$name)) {
      cat(" caption=\"",attr$name,"\"", file = file)
    }
    if(!is.null(attr$description)) {
      cat(" description=\"",attr$description,"\"", file = file)
    }
    cat("\n",file = file)
  }
  # Processing unknown attributes
  for(i in which(!describedAttributes)){
    cat("@attribute", names(x)[i], inferType(x[[attr$id]]), "\n", file = file)
    x.towrite=cbind(x.towrite,x[[i]])
  }
  cat("@data\n", file = file)
  write.table(x.towrite,file = file, dec=".", sep=",",col.names=F,row.names = F)
  flush(file)
  file.close()
}
