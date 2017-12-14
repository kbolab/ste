#' A data harvsdsadsester
#'
#' @description  uppasds
#' @export
is.a.number<-function( stringa ) {
  if(length(stringa)>1) return(FALSE)
  return(!(is.na(suppressWarnings(as.numeric(stringa)))))
}
is.a.string<-function( stringa ) {
  if(length(stringa)>1) return(FALSE)
  return(!is.a.number(stringa))
}
is.a.numeric.array<-function( stringa ) {
  if(!(length(stringa)>1)) return(FALSE)
  if( !(FALSE %in% unlist(lapply(stringa,is.a.number)))  ) return(TRUE)
  else return(FALSE)
}
is.a.string.array<-function( stringa ) {
  if(!(length(stringa)>1)) return(FALSE)
  if( FALSE %in% unlist(lapply(stringa,is.a.number))  ) return(TRUE)
  else return(FALSE)
}
is.a.quoted.string<-function( stringa ) {
  if(length(stringa)>1) return(FALSE)
  stringa <- str_trim(stringa)
  if(stringa=="") return(FALSE)
  # cerca di capire se il conteuto e' compreso fra " o ' e non ce ne sono altri in mezzi
  # (stringa semplice)
  if(!is.na(str_extract(string = stringa, pattern = "^\"[^\"]*\"$"))) return(TRUE)
  if(!is.na(str_extract(string = stringa, pattern = "^\'[^\']*\'$"))) return(TRUE)
  return(FALSE)
}
togli.apici.esterni.stringa<-function( stringa ) {
  stringa <- str_trim(stringa)
  stringa <- str_sub(stringa,start = 2,end = str_length(stringa)-1)
  return(stringa)
}
can.it.be.a.date <- function( stringa ) {
  
  if(str_length(stringa)!=19 & str_length(stringa)!=10) {  return( list("can.be.a.date"=FALSE)) }    
  
  pos.spazio <- str_locate(string = stringa,pattern = " ")[1]
  
  # cercando lo spazio cerca di capire se e' una data o un datetime
  if(!is.na(pos.spazio)) {
    sus.HMS <- str_trim(str_sub(string = stringa,start = pos.spazio[1]+1, end = str_length(stringa)))
    sus.date <- str_trim(str_sub(string = stringa,start = 1, end= pos.spazio[1]-1))
  } else {
    sus.HMS <- ""
    sus.date<- str_trim(stringa)      
  }
  
  # Interpreta la DATA
  date.assigned <- FALSE
  date.char <- unlist(strsplit(x = sus.date,split = ""))
  date.posizioni.sep <-  which(is.na(suppressWarnings(unlist(lapply(date.char,as.numeric)))))
  # aaa <- suppressWarnings(unlist(lapply(aaa,as.numeric)))
  separatori <- date.char[ date.posizioni.sep ]
  if( date.posizioni.sep[1]==5  &  date.posizioni.sep[2]==8  ) {
    date.format <- str_c("%Y",separatori[1],"%m",separatori[1],"%d")
    date.assigned <- TRUE
  }
  if( date.posizioni.sep[1]==3  &  date.posizioni.sep[2]==6  ) {
    date.format <- str_c("%d",separatori[1],"%m",separatori[1],"%Y")
    date.assigned <- TRUE
  }
  if(date.assigned==FALSE) {  return( list("can.be.a.date"=FALSE)) }
  
  # Interpreta HMS
  if( sus.HMS !="" ) {  
    # browser()
    time.char <- unlist(strsplit(x = sus.HMS,split = ""))
    time.posizioni.sep <-  which(is.na(suppressWarnings(unlist(lapply(time.char,as.numeric)))))
    time.separatori <- time.char[ time.posizioni.sep ]
    if( time.posizioni.sep[1]!=3  &  time.posizioni.sep[2]!=6  ) {  return( list("can.be.a.date"=FALSE)) }
    time.format <- str_c("%H",time.separatori[1],"%M",time.separatori[1],"%S")
    date.format <- str_c(date.format," ",time.format)
  }
  
  return( list("can.be.a.date"=TRUE, "data.format"=date.format))
}
stopQuietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
