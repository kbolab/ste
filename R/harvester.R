#' A data harvester
#'
#' @description  A class to collect data and create storages (csv, log, tables, ...)
#' @import stringr XML
#' @export
harvester<-function( ) {
  global.descriptor.lst <- NA
  global.descriptor.xml <- NA
  global.descriptor.xml.fileName <- NA
  global.obj.LLL <- NA
  global.obj.HLL <- NA
  
  # -------------------------------------------------
  # load.descriptor
  # carica il descrittore
  # -------------------------------------------------
  load.descriptor<-function( XML.descriptor.file=NA, XML.descriptor.text=NA){
    if(is.na(XML.descriptor.file) & is.na(XML.descriptor.text)) stop("\n\n\n ERRORE: o 'XML.descriptor.path o 'XML.descriptor.text' deve essere diverso da 'NA'")
    
    if(!is.na(XML.descriptor.file)){
      global.descriptor.xml <<- xmlInternalTreeParse( XML.descriptor.file )
      global.descriptor.xml.fileName <<- XML.descriptor.file
    }
    if(!is.na(XML.descriptor.text)){
      global.descriptor.xml <<- xmlInternalTreeParse(XML.descriptor.text, asText=TRUE)
      global.descriptor.xml.fileName<<-NA
    }    
    global.descriptor.lst<<-xmlToList(global.descriptor.xml)
  }
  # prendi.tags<-function( tagName ) { 
  #   return(global.descriptor.lst[names( global.descriptor.lst) == tagName])
  # }
  # -------------------------------------------------
  # create.virtual.csv
  # crea un CSV fisico, partendo dai dati del descrittore
  # -------------------------------------------------  
  calcola.sottoblocco <- function( lista , livello = 1 , id ){

    classe <- lista$.attrs["class"] 

    # calcolo le mie colonne
    quante.colonne <- sum(names(lista)=="column")
    arr.def.colonna <- c()
    for( n.col in 1:quante.colonne ) {
      # prendi il contenuto della cella
      contenuto.colonna <- as.character(lista[names( lista) == "column"][n.col])
      # e componi il nome dell'attributo 
      stringa.attributo <- str_c(classe,".",contenuto.colonna)
      
      tmp.HLL <- HLL()
      imported.class.methods <- global.obj.HLL$getClassMethods()
      tmp.HLL$setEnv(env = global.obj.LLL, mem = list("implicit.PK"=id),classMethods = imported.class.methods )
      res <- tmp.HLL$execute(script = stringa.attributo)
      arr.id <- res$valore
      
      arr.def.colonna<-c(arr.def.colonna,arr.id)
    }

    # se sei una foglia, ritorna
    if( !("section" %in% names(lista) )) {
      return(arr.def.colonna) 
    }
    # se no, invoca il ricorisivo
    metodo.figlio <- as.character(lista$section$.attrs["method"])
    stringa.cattura.nuovi.id <- str_c(classe,".",metodo.figlio)
    tmp.HLL <- HLL()

    imported.class.methods <- global.obj.HLL$getClassMethods()
    tmp.HLL$setEnv(env = global.obj.LLL, mem = list("implicit.PK"=id), classMethods = imported.class.methods )
    res <- tmp.HLL$execute(script = stringa.cattura.nuovi.id)
    arr.id <- res$valore
    
    if(is.null(arr.id)) return(c())
    
    tot.sottomatrice <- c()
    for( i in 1:length(arr.id)) { 
      sottomatrice <- calcola.sottoblocco(lista = lista$section, livello = livello + 1, id = arr.id[i] )
      tot.sottomatrice <- rbind(tot.sottomatrice,sottomatrice)
    }
    master.matrice <- matrix(rep(arr.def.colonna,nrow(tot.sottomatrice)) , nrow = nrow(tot.sottomatrice) ,byrow = T)
    matrice.completa <- cbind(master.matrice,tot.sottomatrice  )
    # browser()
    return(matrice.completa)
    
  }
  create.csv<-function(  ){

    # prendi gli ID ri riga da analizzare
    arr.id <- global.descriptor.lst$rows$id$text
    arr.id <- unlist(str_split(string = arr.id,pattern = ","))
    
    # prendi la parent.class
    altra.sezione.nidificata <- TRUE
    main.class <- TRUE
    subElementList <- global.descriptor.lst$section
    
    mega.matrice <- c()
    for(id in 1:length(arr.id)) { 
      tmp.res <- calcola.sottoblocco( lista = subElementList , id = arr.id[id] )
      mega.matrice <- rbind( mega.matrice,tmp.res )
    }
    return(mega.matrice)
  }
  load.LLL <- function(filename = NA, script = NA, folder = NA){
    if( is.na(filename) & is.na(script) & is.na(folder) ) {
      stop("qualcosa devi passare...")
    }
    text <- paste(readLines(con = filename,warn = F),collapse = "\n")
    global.obj.LLL$parseScript(script = text )
  }
  load.HLL <- function(filename = NA, script = NA, folder = NA){
    if( is.na(filename) & is.na(script) & is.na(folder) ) {
      stop("qualcosa devi passare...")
    }
    text <- paste(readLines(con = filename,warn = F),collapse = "\n")
    global.obj.HLL$parseScript(script = text )
  }  
  # ----------------------------------------------------------------
  # Costruttore
  # ----------------------------------------------------------------
  costructor<-function( ) {
    global.descriptor.xml <<- NA
    global.descriptor.xml.fileName <<- NA
    global.descriptor.lst <<- NA
    global.obj.LLL <<- LLL()
    global.obj.HLL <<- HLL()
  }
  costructor()
  # ----------------------------------------------------------------
  # RETURN di classe
  # ----------------------------------------------------------------
  return(
    list(
      "create.csv"=create.csv,
      "load.descriptor"=load.descriptor,
      "load.LLL"=load.LLL,
      "load.HLL"=load.HLL
    )
  )  
  
}
