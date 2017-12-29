

ricavaElementiDaRisolvereDaStringa<-function( stringa ) { 
  # cat("\n ===> ",stringa)
  debug.var <- FALSE
  # if(stringa =="Tools.getDeltaDays( ultimaData, dataOdierna )") debug.var <- TRUE
    
  lst.Transizioni <- list()
  lst.Transizioni[["ready"]]<-c( "int"="int", "char"="token",   "sep"="ready", "quote"="quotedString"   )
  lst.Transizioni[["int"]]  <-c( "int"="int", "char"="ERROR", "sep"="ready", "quote"="ERROR"   )
  lst.Transizioni[["sep"]]  <-c( "int"="int", "char"="token",   "sep"="ready", "quote"="quotedString"   )
  lst.Transizioni[["token"]]  <-c( "int"="token", "char"="token",   "sep"="ready", "quote"="ERROR"   )
  lst.Transizioni[["quotedString"]]  <-c( "int"="quotedString", "char"="quotedString",   "sep"="quotedString", "quote"="ready"   )

  # res <- list()
  # res["obj.implicit.PK"]<- str_extract(string = stringa, pattern = "^[ ]*[a-zA-Z]+[a-zA-Z0-9_]*\\.[a-zA-Z]+[a-zA-Z0-9_]*[ ]*$")
  # res["obj.with.parameters"]<- str_extract(string = stringa, pattern = "^[ ]*^[a-zA-Z _]+(\\(.*\\))*\\.[a-zA-Z _]+\\(.*\\)$")    
  # 
  # if(str_sub(string = stringa,start = 1,end =  str_length("noduloTiroideo"))=="noduloTiroideo") browser()
  
  lst.tipoSimbolo <- list()
  lst.tipoSimbolo[["int"]] <- c("0","1","2","3","4","5","6","7","8","9",".")
  lst.tipoSimbolo[["char"]] <- c("a","b","c","d","e","f","g","h","i","l","m","n","o","p","q","r","s","t","u","v","z","x","y","j","y","_")
  lst.tipoSimbolo[["char"]] <- c(lst.tipoSimbolo[["char"]],str_to_upper(lst.tipoSimbolo[["char"]]))
  lst.tipoSimbolo[["sep"]] <- c("(",")","+","-","*","/"," ")
  lst.tipoSimbolo[["quote"]] <- c("'")

  stato.iniziale <- "ready"
  stato.attuale <- stato.iniziale 
  matrice.log.transizioni <- c()
  log.posizione.inizio.stato <- 0
  for( i in 1:str_length(stringa)) {
    simbolo <- str_sub(string = stringa,start = i,end = i)
    
    # Estrai il tipo di simbolo
    # Non e' bello un apply cosi', ma fa risparmiare tempo: perdoname por mi vida loca.... 
    tipo.Simbolo <- unlist(lapply( names(lst.tipoSimbolo), function(x) {  if(simbolo %in% lst.tipoSimbolo[[x]] ) return(x)  }  ))
    
    # if(debug.var==TRUE) browser()
    # if(debug.var==TRUE) cat("\n simbolo=",simbolo, " tipo.simbolo=",tipo.Simbolo)
    
    # prendi il prossimo stato previsto
    prossimo.stato <- lst.Transizioni[[stato.attuale]][ which( names(lst.Transizioni[[stato.attuale]])==tipo.Simbolo )]
    
    # se la transizione non e' ammessa, dai errore
    if( length(prossimo.stato) == 0 ) { return(list("exitCode"=-1) )}
    if( prossimo.stato == "ERROR" ) { return(list("exitCode"=-2) )} 
    
    if( prossimo.stato != stato.attuale )  {
      # Aggiorna la matrice delle transizioni rilevate
      matrice.log.transizioni <- rbind( matrice.log.transizioni, c( stato.attuale, log.posizione.inizio.stato ,i-1,str_sub(stringa,log.posizione.inizio.stato,i-1)  ) )
      colnames(matrice.log.transizioni)<-c("stato","from","to","substr")
      
      # Aggiorna lo stato e tieni traccia di dove e' l'inizio di questo
      stato.attuale <- prossimo.stato
      log.posizione.inizio.stato <- i
    }
  }
  # if(debug.var==TRUE) browser()
  # Aggiorna la matrice delle transizioni rilevate (l'ultima non e' stata 
  # detectata dal for perche' e' uscito prima)
  matrice.log.transizioni <- rbind( matrice.log.transizioni, c( stato.attuale, log.posizione.inizio.stato ,i,str_sub(stringa,log.posizione.inizio.stato,i)  ) )
  colnames(matrice.log.transizioni)<-c("stato","from","to","substr")  
  
  
  # if(str_sub(string = stringa,start = 1,end =  str_length("noduloTiroideo"))=="noduloTiroideo") browser()
  
  return( list( "exitCode" = 0,
                "matriceElementiRilevati"=matrice.log.transizioni
              ) )
}