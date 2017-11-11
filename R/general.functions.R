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
