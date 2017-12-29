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
  # global.LLL.env<-NA
  global.mem.struct<-NA
  global.null.value<-NA
  # global.mem.struct$class.methods<-NA
  
  # ===============================================================================
  # PROXY
  # si occupa di chiamare le classi opportune
  # stringa = il metodo da invocare
  # lst.parametri = eventuali altri valori passati come PARAMETER
  # ===============================================================================
  proxy <- function( stringa , lst.parametri = c() ) {
    
    passed.parameters <<- lst.parametri
    passed.string <<- stringa
    
    if(stringa=="arrayNew") {
      return( arrayNew() )
    }    
    if(stringa=="arrayPush") {
      return( arrayPush() )
    }    
    if(stringa=="getMax") {
      return( getMax() )
    }        
    if(stringa=="arrayLength") {
      return( arrayLength() )
    }
    if(stringa=="getDeltaDays") {
      return( getDeltaDays() )
    }    
    if(stringa=="addDaysToDate") {
      return( addDaysToDate() )
    }        
    if(stringa=="today") {
      valToRet <- as.character(Sys.Date())
      return( list("value"=valToRet , "error"=FALSE , "type" = "date"))
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
  # arrayLength
  # Restituisci il numero di elementi di un array
  # ===============================================================================  
  arrayLength<-function() {
    # browser()
    checkParameters(1)
    quantiElementi <- length(passed.parameters[[1]]$value)
    return( list("value"=quantiElementi , "error"=FALSE , "type" = ""))
  }   
  # ===============================================================================
  # arrayNew
  # Crea un array del tipo specificato
  # ===============================================================================  
  arrayNew<-function() {
    # browser()
    checkParameters(0)
    return( list("value"=c() , "error"=FALSE , "type" = ""))
  }  
  # ===============================================================================
  # arrayNew
  # Crea un array del tipo specificato
  # ===============================================================================  
  arrayPush<-function() {
    checkParameters(2)
    # Togli l'eventuale valore "null"
    listaValoriNonNull <- passed.parameters[[1]]$value[ which(passed.parameters[[1]]$value!=global.null.value)]
    # ed accoda
    listaValoriNonNull <- c(listaValoriNonNull,passed.parameters[[2]]$value)
    # poi ritorna
    return( list("value"=listaValoriNonNull , "error"=FALSE , "type" = ""))
  }   
  # ===============================================================================
  # getMax
  # Crea un array del tipo specificato
  # ===============================================================================  
  getMax<-function() {
    checkParameters(1)
    nuovoArray <- cast.to.numeric.array(passed.parameters[[1]]$value)
    if(length(nuovoArray)==0) massimo <- global.null.value
    else  massimo <- max(nuovoArray)
    # poi ritorna
    return( list("value"=massimo , "error"=FALSE , "type" = ""))
  }    
  # ===============================================================================
  # addDaysToDate
  # aggiungi TOT giorni ad una certa data
  # ===============================================================================  
  addDaysToDate<-function() {
    # browser()
    checkParameters(2)
    
    d.1 <- can.it.be.a.date( passed.parameters[[1]]$value ) 
    if(d.1$can.be.a.date!=TRUE) stop("Errore: il primo campo passato non e' una data")
    if(is.numeric(passed.parameters[[2]]$value)!=TRUE) stop("Errore: il secondo campo passato non e' un intero")

    dataFrom <- as.POSIXct(passed.parameters[[1]]$value, format = d.1$data.format)
    dataCalcolata <- dataFrom + passed.parameters[[2]]$value *24*60*60
    dataCalcolata <- as.character(format( dataCalcolata, format= d.1$data.format))
    return( list("value"=dataCalcolata , "error"=FALSE , "type" = "string"))
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
  # ----------------------------------------------------------------
  # setEnv
  # Setta il contesto, attivita' FACOLTATIVA, nel caso di TOOLS
  # ----------------------------------------------------------------
  setEnv<-function(  mem = list() , null.value = NA) {
    # if(length(env)>0) global.LLL.env <<- env
    if(length(mem)>0) global.mem.struct <<- mem
    if(!is.na(null.value)>0) global.null.value <<- null.value
    # if(length(classMethods)>0) global.mem.struct$class.methods <<- classMethods
  }  
  # -------------------------------------------------
  # costruttore
  # -------------------------------------------------
  constructor <- function() {
    passed.parameters <<- list()
    passed.string <<- NA
    global.mem.struct<<-list()
    global.null.value<<-"null"
  }
  constructor()
  # -------------------------------------------------
  return(list(
    "proxy"=proxy,
    "setEnv"=setEnv
  ))
}
