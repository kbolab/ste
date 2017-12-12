#' a DB connector class
#'
#' @description  A class to connect with RDBMS
#' @import RMySQL
#' @param RDBMS.type the RDBMS type. By default 'mysql', at the moment (the only supported)
#' @param user the user for the RDBMS connection
#' @param password the password for the RDBMS connection
#' @param host the host for the RDBMS connection
#' @param dbname the dataBase name for the RDBMS connection
#' @export
Ste.tool.class <- function( ) {
  
  passed.parameters <- list()
  passed.string <- NA
  
  # ===============================================================================
  # PROXY
  # si occupa di chiamare le classi opportune
  # stringa = il metodo da invocare
  # lst.parametri = eventuali altri valori passati come PARAMETER
  # ===============================================================================
  proxy <- function( stringa , lst.parametri = c() ) {
    
    passed.parameters <<- lst.parametri
    passed.string <<- stringa
    
    if(stringa=="today") {
      valToRet <- as.character(Sys.Date())
      return( list("value"=valToRet , "error"=FALSE , "type" = "date"))
    }
    if(stringa=="getDeltaDays") {
      return( getDeltaDays() )
    }    
    stop("\n Syntax error in invocking a Tools'method")
  }
  # ===============================================================================
  # checkParameters
  # verifica che siano stati passati un numero di parametri congruo con il tipo
  # di metodo invocato
  # ===============================================================================
  checkParameters<-function(quanti) {
    if(quanti!=length(passed.parameters)) {
      cat("\n ======================================================")
      cat("\n ERRORE! Il metodo ", passed.string," prevede ",quanti," parametri: ne sono invece stati rilevati  ",length(passed,parameters))
      cat("\n ======================================================\n")
      browser()
      stop()
    }
  }
  # ===============================================================================
  # getDeltaDays
  # calcola quanti giorni ci sono di distanz fra due date
  # ===============================================================================  
  getDeltaDays<-function() {
    checkParameters(2)
    # browser()
    d.1 <- can.it.be.a.date( passed.parameters[[1]]$value )
    # browser()
    d.2 <- can.it.be.a.date( passed.parameters[[2]]$value )
    
    if(d.1$can.be.a.date!=TRUE) stop("Errore: una non e' una data")
    if(d.2$can.be.a.date!=TRUE) stop("Errore: una non e' una data")
    
    delta.Date <- as.numeric(difftime(as.POSIXct(passed.parameters[[1]]$value, format = d.1$data.format),as.POSIXct(passed.parameters[[2]]$value, format = d.2$data.format),  units = 'days'))
    return( list("value"=delta.Date , "error"=FALSE , "type" = "numeric"))
  }
  # -------------------------------------------------
  # costruttore
  # -------------------------------------------------
  constructor <- function() {
    passed.parameters <<- list()
    passed.string <<- NA
  }
  constructor()
  # -------------------------------------------------
  return(list(
    "proxy"=proxy
  ))
}
