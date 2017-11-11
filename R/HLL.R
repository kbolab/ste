#' High Level Language Class
#'
#' @description  The engine class to parse and execute HLL scripts
#' @import stringr
#' @export
HLL <- function() {

  LLL.env <- NA
  mem.struct <- list()

  parseScript<-function( script ) {
    # splitta le righe ed elimina le righe vuote
    arr.righe <- str_trim(unlist(str_split(string = script,pattern = "\n")))

    # Fai il pre-processing per identificare strutture quali cicli, if-then-else, etc..
    # e butta tutto nella memoria di elaborazione    
    mem.struct$script.structures <<- preProcessing.Script( script.lines = arr.righe )
    
    script.terminato <- FALSE
    indice.riga <- 1
    while( script.terminato == FALSE ) {
      
      # il comando in qeustione e' una istruzione di salto?
      # (setta il def a no)
      jump.statement <- FALSE
      
      # Se superi il numero di righe massime... esci
      if(indice.riga > length(arr.righe)) { script.terminato <- TRUE; break  }

      # trimma e skippa le righe vuote. Non dovrei averne, ma non si sa mai.
      str.riga <- str_trim(arr.righe[indice.riga])
      if(str.riga=="") { indice.riga<-indice.riga+1; next; }
      if(str_sub(str_trim(str.riga),start = 1,end = 1)==":") { indice.riga<-indice.riga+1; next; }
      if(str_sub(str_trim(str.riga),start = 1,end = 1)=="#") { indice.riga<-indice.riga+1; next; }

      # cat("\nS:",str.riga)
      
      # Parse della stringa:
      # SE NON E' APERTO UN CONTESTO :
      if( length(mem.struct$define.context)==0) {
        res <- execute(script = str.riga , complete.script = arr.righe , script.cursor = indice.riga)
        # Se era un return, restituisci
        if(res$operation.token=="return") return(res)
      }
      # SE E' APERTO UN CONTESTO
      # Registra le righe che incontri, senza eseguirle ed associandole alla
      # coppia <classe, metodo> opportuna
      if( length(mem.struct$define.context)>0) {
        proxy.mem.contesto.addLine(stringa = str.riga)
        # Se pero' la riga era "enddefine", be', chiudi il contesto :)
        if(!is.na(str_extract(string = str.riga, pattern = "^[ ]*enddefine[ ]*$")))  {
          proxy.mem.contesto.set(destroy.contest = TRUE)
        }
      }
      
      if(res$operation.token=="IF") { jump.statement <-TRUE }
      if(res$operation.token=="ENDIF") { jump.statement <-TRUE }
      
      # Se l'istruzione eseguita era una istruzione che prevedeva un salto, comportati di conseguenza
      # (non aggiornare il cursore in avanti di uno, ma fai il jump previsto)
      if( jump.statement == TRUE ) 
        indice.riga <- res$setScriptCursorTo
      else  
        indice.riga <- indice.riga + 1
    }
  }
  # ---------------------------------------------------------------
  # invoca.ricorsivamente.HLL
  # Risolve ricorsivamente una chiamata (crea l'oggetto, copia l'env)
  # esegue la chiamata e manipola il risultato
  # ---------------------------------------------------------------
  invoca.ricorsivamente.HLL<-function( HLL.script ) {
    # Crea un oggetto HLL
    obj.HLL<-HLL()
    # setta l'environment
    obj.HLL$setEnv( env = LLL.env , mem = mem.struct )
    # Fai il parse dello script
    # Esegue il corrispettivo di una sola riga
    # res <- obj.HLL$parseScript( script = HLL.script )
    res <- obj.HLL$execute( script = HLL.script )
    # browser()
    # browser()
    # restituisci il risultato
    return(res)
  }
  # ---------------------------------------------------------------
  # risolvi.metodo.HLL
  # Risolve ricorsivamente una metodo. Simile invoca.ricorsivamente.HLL
  # deve gestire tuttavia differentemente lo scoping delle variabili
  # ---------------------------------------------------------------
  risolvi.metodo.HLL<-function( classe, metodo, implicit.PK, script = NA ) {

    # setta l'environment
    # devi passare LLL ed eventuali metodi definiti in HLL, NON le variabili
    tmp.mem.struct <- mem.struct
    tmp.mem.struct$var<-list()
    tmp.mem.struct$define.context<-list()
    tmp.mem.struct$implicit.PK<-NA

    # Crea un oggetto HLL
    obj.HLL<-HLL()
    # prendi lo script da mandare (togli 'define' ed 'enddefine')
    HLL.script <- mem.struct$class.methods[[classe]][[metodo]]
    HLL.script <- HLL.script[ 2: (length(HLL.script)-1) ]
    HLL.script <- paste(HLL.script, collapse = "\n")

    # Aggiungi la PK implicita (eventualmente sovrascrivendo)
    tmp.mem.struct$implicit.PK<-implicit.PK

    # Fai il push dell'ENV
    obj.HLL$setEnv( env = LLL.env , mem = tmp.mem.struct )
    # Fai il parse dello script
    res <- obj.HLL$parseScript( script = HLL.script )
    # restituisci il risultato
    return(res)
  }
  # ---------------------------------------------------------------
  # proxy.mem.struct.set
  # proxy per il SET delle variabili in memoria
  # ---------------------------------------------------------------
  proxy.mem.struct.set<-function( varName=NA, value=NA, type=NA) {
    # Se la variabile non c'e', creala nella struttura
    if( !(varName %in% mem.struct[["var"]]) ) mem.struct[["var"]][[varName]] <<- list()
    mem.struct[["var"]][[varName]]$type <<- type
    mem.struct[["var"]][[varName]]$value <<- value
  }
  # ---------------------------------------------------------------
  # proxy.mem.contesto.set
  # proxy per il SET del contesto in memoria
  # ---------------------------------------------------------------
  proxy.mem.contesto.set<-function( method.name=NA, class.name=NA , destroy.contest = FALSE) {
    # Se devi distruggere il contesto, non perdere tempo!
    if(destroy.contest==TRUE ) {
      mem.struct$define.context <<- list()
      return()
    }
    # se no, continua...
    mem.struct$define.context$method.name <<- method.name
    mem.struct$define.context$class.name <<- class.name
    mem.struct$define.context$script <<- c()
    mem.struct$class.method
    # browser()
    if(!("class.methods" %in% mem.struct)) mem.struct$class.methods<<-list()
    if(! ( class.name %in% mem.struct$class.methods ) ) mem.struct$class.methods[[class.name]]<<-list()
    if(!(method.name %in% mem.struct$class.methods[[class.name]])) mem.struct$class.methods[[class.name]][[method.name]]<<-c()
  }
  getClassMethods<-function(  ) {
    return(mem.struct$class.methods)
  }
  # ---------------------------------------------------------------
  # proxy.mem.contesto.addLine
  # proxy per l'aggiunta di una linea al contesto
  # ---------------------------------------------------------------
  proxy.mem.contesto.addLine<-function(  stringa ) {
    if( length(mem.struct$define.context)==0 ) stop("\n errore, non e' stato definito alcun contesto in cui copiare le righe")
    mem.struct$define.context$script <<- c( mem.struct$define.context$script , stringa)
    class.name <- mem.struct$define.context$class.name
    method.name <- mem.struct$define.context$method.name
    mem.struct$class.methods[[class.name]][[method.name]] <<- c(mem.struct$class.methods[[class.name]][[method.name]],stringa)
  }
  # ---------------------------------------------------------------
  # execute
  # Esegui un singolo comando (o una linea: per quanto possibile
  # risolveraa' ricorsivamente le chiamate)
  # ---------------------------------------------------------------
  execute <-  function( script , complete.script = NA , script.cursor = NA) {
    cat("\n\t (",script.cursor,")",script)
# if(script=="'prova'") browser()
    stringa <- script
    match.trovato <- FALSE
    res<-list()
    res["set"]<- str_extract(string = stringa, pattern = "^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=")
    res["obj"]<- str_extract(string = stringa, pattern = "^[ ]*^[a-zA-Z _]+\\(.*\\)[ ]*\\..*$")
    res["obj.implicit.PK"]<- str_extract(string = stringa, pattern = "^[ ]*[a-zA-Z]+[a-zA-Z0-9]*\\.[a-zA-Z]+[a-zA-Z0-9]*[ ]*$")
    res["return"]<- str_extract(string = stringa, pattern = "^[ ]*return\\(.*\\)[ ]*$")
    res["define"]<- str_extract(string = stringa, pattern = "^[ ]*define[ ]+[a-zA-Z0-9_]+[ ]+as[ ]+method[ ]+of[ ]+[a-zA-Z0-9_]+[ ]*$")
    res["enddefine"]<- str_extract(string = stringa, pattern = "^[ ]*enddefine[ ]*$")
    res["str_cat"]<- str_extract(string = stringa, pattern = "^[ ]*str_cat\\(.*\\)[ ]*$")
    res["if"]<- str_extract(string = stringa, pattern = "^[ ]*if[ ]*\\(.*\\)[ ]*then[ ]*$")
    res["else"]<- str_extract(string = stringa, pattern = "^[ ]*else[ ]*$")
    res["endif"]<- str_extract(string = stringa, pattern = "^[ ]*endif[ ]*$")

    #  SET
    if(!is.na(res["set"])) {
      toReturn <- risolvi.set( stringa )
      return(toReturn)
    }
    # Se e' un' accesso ad un oggetto
    if(!is.na(res["obj"]) | !is.na(res["obj.implicit.PK"])) {
      toReturn <- risolvi.accessoAMetodo( stringa, res )
      return(toReturn)      
    }
    #  RETURN
    if(!is.na(res["return"])) {
      toReturn <- risolvi.return( stringa )
      return(toReturn)     
    }
    # DEFINE
    if(!is.na(res["define"])) {
      toReturn <- risolvi.define( stringa )
      return(toReturn)           
    }
    #  ENDDEFINE
    if(!is.na(res["enddefine"])) {
      stop("\n errore, qui ci dovrei arrivare solo senza un contesto aperto... (e se sono qui vuol dire che non ci sono contesti aperti)")
    }
    # STR_CAT
    if(!is.na(res["str_cat"])) {
      toReturn <- risolvi.str_cat( stringa )
      return(toReturn)  
    }
    #  IF
    if(!is.na(res["if"])) {
      toReturn <- risolvi.if( stringa, complete.script = complete.script , script.cursor = script.cursor)
      return(toReturn)  
    }    
    #  ELSE
    if(!is.na(res["else"])) {
      toReturn <- risolvi.else( stringa, complete.script = complete.script , script.cursor = script.cursor)
      return(toReturn)  
    }      
    #  ELSE
    if(!is.na(res["endif"])) {
      toReturn <- risolvi.endif( stringa, complete.script = complete.script , script.cursor = script.cursor)
      return(toReturn)  
    }        

    # =========================================
    # SYNTAX ERROR!
    # =========================================
    if(match.trovato== FALSE) {
      cat( "\n syntax error in resolving: ", stringa )
      stop()
    }
  }
  # ********************************************************************
  # INIZIO Sezione di risoluzione della semantica
  # ********************************************************************
  
  # ----------------------------------------------------
  # ENDIF
  # ----------------------------------------------------   
  risolvi.endif<-function( stringa , complete.script , script.cursor  ) {
    arr.endif <- unlist(lapply(mem.struct$script.structures$if.else.endif, function(x){ x$riga.endif } ))
    quale_posizione <- which( arr.endif == script.cursor,arr.ind = T)
    if(length(quale_posizione)==0) stop("ERRORE: non riesco a trovare la posizione cui e' associato l'ENDIF")
    
    script.cursor <- as.numeric(script.cursor)
    
    # in ogni caso, poche seghe: devi andare alla posizione successiva :)
    return( 
      list( "valore" = NA,
            "operation.token" = "ENDIF",
            "operation" = stringa,
            "setScriptCursorTo" = script.cursor  + 1
      )
    )   
  }  
  # ----------------------------------------------------
  # ELSE
  # ----------------------------------------------------   
  risolvi.else<-function( stringa , complete.script , script.cursor  ) {
    
    arr.else <- unlist(lapply(mem.struct$script.structures$if.else.endif, function(x){ x$riga.else } ))
    quale_posizione <- which( arr.else == script.cursor,arr.ind = T)
    if(length(quale_posizione)==0) stop("ERRORE: non riesco a trovare la posizione cui e' associato l'ELSE")

    # Se sto leggendo un ELSE e' perche' ero nella condizione ed ora devo saltare all'endif
    # quindi prendi l'endif associato e zompa!
    new.script.cursor <- mem.struct$script.structures$if.else.endif[[quale_posizione]]$riga.endif
    
    return( 
      list( "valore" = NA,
            "operation.token" = "ENDIF",
            "operation" = stringa,
            "setScriptCursorTo" = as.numeric(new.script.cursor)
      )
    )     
  }
  # ----------------------------------------------------
  # IF
  # ----------------------------------------------------  
  risolvi.if<-function( stringa , complete.script , script.cursor ) {

    # Bene! Dato che sono stato figo ed ho costruito in pre-processing degli 
    # script la struttura degli if, facciamo che ora vado a ripigliarla!
    # (prima vediamo che ci sia, senno': ERROR! )
    if( !(as.character(script.cursor) %in% names(mem.struct$script.structures$if.else.endif)) ){
      stop("Errore! mi sarei aspettato di trovar parlato di questo if, dal pre-processor dello script!")
    }
    
    # prendi i nomi delle sospette variabili da sostituire
    sospette.variabili <- mem.struct$script.structures$if.else.endif[[as.character(script.cursor)]]$toResolve
    valore.trovato<-list(); tipo.variabile <- list()
    for( variabile in sospette.variabili ) {
      if(!(variabile %in% names(mem.struct$var)))  {
        cat("\n Errore! La variabile'",variabile,"' non mi risulta definita....")
        stop()
      } 
      valore.trovato[[variabile]] <- mem.struct$var[[variabile]]$value
      tipo.variabile[[variabile]] <- mem.struct$var[[variabile]]$type
    }
    
    # ora scorri l'array delle posizioni da ricostruire, dove abbiamo indicato con il nome
    # "token" le posizioni in cui e' stata trovata (stimata) una variabile di cui sostituire il valore
    arrayCondizioneDaRicostruire <- mem.struct$script.structures$if.else.endif[[as.character(script.cursor)]]$arrayCondizioneDaRicostruire
    condizione.finale.da.parsare<-c()
    if(length(arrayCondizioneDaRicostruire)>0) { 
      for( i in 1:length(arrayCondizioneDaRicostruire)) { 
        tipo <- names(arrayCondizioneDaRicostruire)[i]
        pezzo <- arrayCondizioneDaRicostruire[i]
        if(tipo=="token")  { 
          if(tipo.variabile[[variabile]]=="string")
            pezzo <- paste(c("'",valore.trovato[[variabile]],"'"),collapse = '')
          else
            pezzo <- valore.trovato[[variabile]]
        }
        condizione.finale.da.parsare <- paste(c(condizione.finale.da.parsare,pezzo),collapse = '')
      }
    } else  { 
      condizione.finale.da.parsare <- arrayCondizioneDaRicostruire
    }
    # maremma maiala, sono esausto. Comunque ci sono: infine possiamo interpretare la condizione
    esito.condizione <- eval(parse(text=condizione.finale.da.parsare))
    
    # Ora cerca di capire dove il cursore di eseuczione dello script dovrebbe venire mosso!
    riga.else <- mem.struct$script.structures$if.else.endif$`2`$riga.else
    riga.endif <- mem.struct$script.structures$if.else.endif$`2`$riga.endif
    
    if(esito.condizione==TRUE) nuova.posizione.cursore <- script.cursor + 1
    if(esito.condizione==FALSE) { 
      if(!is.na(riga.else))
          nuova.posizione.cursore <- as.numeric(riga.else)
        else 
          nuova.posizione.cursore <- as.numeric(riga.endif)
    }

    return( 
      list( "valore" = esito.condizione,
            "operation.token" = "IF",
            "operation" = stringa,
            "setScriptCursorTo" = as.numeric(nuova.posizione.cursore)
      )
    )
  }
  # ----------------------------------------------------
  # STR_CAT
  # ----------------------------------------------------  
  risolvi.str_cat<-function( stringa ) {
    tmp.stringa <- str_trim(stringa)
    tmp.stringa <- str_sub(tmp.stringa,start = 9,end = str_length(tmp.stringa)-1)
    
    arr.membri <- unlist(str_split(string = tmp.stringa,pattern = ","))
    
    valore.finale <- c()
    for( membro.1 in arr.membri) {
      
      # E' una stringa?'
      if(is.a.quoted.string(membro.1)) valore.membro.1 <- togli.apici.esterni.stringa(membro.1)
      else {  # E' una variabile?
        if(membro.1 %in% names(mem.struct$var))  {
          if(mem.struct$var[[membro.1]]$type != "string") stop("errore: non posso fare lo str_cat fra non stringhe")
          valore.membro.1 <- mem.struct$var[[membro.1]]$value
        }
        else {  # ultimo tentativo: Risolvilo!
          tmp.res.HLL <- invoca.ricorsivamente.HLL( membro.1 )
          valore.membro.1 <- tmp.res.HLL$valore
        }
      }
      valore.finale <- str_c(valore.finale,valore.membro.1)
    }
    return(list( "valore"=valore.finale, "operation.token" = "str_cat", "operation"=stringa))
    # Se gnon ia' era aperta una definizione, dai errore
    stop("\n errore, qui ci dovrei arrivare solo senza un contesto aperto...")    
  }
  # ----------------------------------------------------
  # DEFINE
  # ----------------------------------------------------  
  risolvi.define<-function( stringa ) {
    # Se gia' era aperta una definizione, dai errore
    if(length(mem.struct$define.context)>0) stop("\n sono gia' nel contesto di una definizione: non posso aprirle un'altra")
    nome.metodo <- str_trim(sub("+[ ]+as[ ]+method[ ]+of[ ]+[a-zA-Z0-9_]+[ ]*$","\\1",stringa) )
    nome.metodo <- str_trim(str_sub(string = nome.metodo,start = 7,end = str_length(nome.metodo)))
    nome.classe <- str_trim(sub("^[ ]*define[ ]+[a-zA-Z0-9_]+[ ]+as[ ]+method[ ]+of[ ]+","\\1",stringa) )
    proxy.mem.contesto.set(method.name = nome.metodo,class.name = nome.classe)
    return(list( "valore"=NA, "operation.token" = "define", "operation"=stringa))    
  }
  # ----------------------------------------------------
  # RETURN
  # ----------------------------------------------------  
  risolvi.return<-function( stringa ) {
    
    argomento <- sub("^[ ]*return\\([ ]*","\\1",stringa)
    argomento <- str_sub(argomento,start = 1,end = str_length(argomento)-1)
    # browser()
    # e' un intero?
    if(is.a.number(argomento)) {
      return(list( "valore"=as.numeric(argomento), "operation.token" = "return", "operation"=stringa))
    }
    # e' una stringa?
    if(is.a.quoted.string(argomento)) {
      argomento <- togli.apici.esterni.stringa(argomento)
      return(list( "valore"=argomento, "operation.token" = "return", "operation"=stringa))
    }
    # e' una variabile?
    if( argomento %in% names(mem.struct$var) ) {
      return(list( "valore"=mem.struct$var[[argomento]]$value, "operation.token" = "return", "operation"=stringa))
    }
    # ... allora dai errore perche' va risolto ricorsivamente
    stop("-TODO: invoca ricorsivamente il calcolo dell' argomento da restituire")    
  }
  
  # ----------------------------------------------------
  # <oggetto>(<id>).<attributo> OPPURE un <oggetto>.<attributo> con un PK implicito
  # ----------------------------------------------------  
  risolvi.accessoAMetodo<-function( stringa , res) {
    
    # toReturn <- risolvi.accessoAMetodo( stringa )
    # Due controlli formali di apertura, giusto per gradire (se caso implicito ma manca la PK)
    if(!is.na(res["obj.implicit.PK"])) {
      if( !("implicit.PK" %in% names(mem.struct)) ) stop("\n non e' stato dichiarata la PK implicita (1)")
      if( is.na(mem.struct$implicit.PK)) stop("\n non e' stato dichiarata la PK implicita (2)")
      
      nome.oggetto <- str_trim(sub("\\.[a-zA-Z]+[a-zA-Z0-9]*[ ]*$" ,"\\1", stringa ))
      attributo <- str_trim(sub("^[ ]*[a-zA-Z]+[a-zA-Z0-9]*\\." ,"\\1", stringa ))
      obj.pk <- mem.struct$implicit.PK
    }
    else {  # CASO ESPLICITO
      # Estrai 'nome.oggetto', 'obj.pk' e 'attributo'
      obj.pk <- str_trim(sub("[ ]*\\..*$","\\1",stringa))
      obj.pk <- str_trim(sub("^[ ]*^[a-zA-Z _]+\\(","\\1",obj.pk))
      obj.pk <- str_sub(string = obj.pk,  start=1, end=str_length(obj.pk)-1)
      nome.oggetto <- str_trim(sub("+\\(.*\\)[ ]*\\..*$","\\1",stringa))
      attributo <- sub("^[ ]*^[a-zA-Z _]+\\(.*\\)[ ]*\\.[ ]*","\\1",stringa)
    }
    
    # invoca eventuali calcoli ricorsivi, per risolvere 'obj.pk' o il valore di 'secondo.membro'
    if( is.a.number(obj.pk) == FALSE) {
      # Vediamo se e' una variabile :)
      if( obj.pk %in% names(mem.struct[["var"]]) ) {
        if( mem.struct[["var"]][[obj.pk]]$type=="numeric"  ) {
          obj.pk <- mem.struct[["var"]][[obj.pk]]$value
        }
        else { stop("ERRORE: l'id non puo' essere usato come chiave in quanto non numerico") }
      }
      else  {
        stop("-TODO: invoca ricorsivamente il calcolo dell' PK dell'oggetto")
      }
    }
    
    # due controlli formali, giusto per gradire
    if(obj.pk=="") {  cat( "\nmmmhhhh, sono restato senza obj.pk: ", stringa );  stop()  }
    if(nome.oggetto=="") {  cat( "\nmmmhhhh, sono restato senza nome.oggetto: ", stringa );  stop()  }
    if(attributo=="") {  cat( "\nmmmhhhh, sono restato senza attributo: ", stringa );  stop()  }
    
    # E' un metodo? (in HLL)
    if(attributo %in% names(mem.struct$class.methods[[nome.oggetto]])) {
      res <- risolvi.metodo.HLL( classe = nome.oggetto , metodo = attributo, implicit.PK = obj.pk )
      return( res )
    }
    else {
      # E' l'attributo di una classe? ( in LLL )
      if(LLL.env$is.attribute.of(className = nome.oggetto, attrName=attributo) == TRUE) {
        res <- LLL.env$getEntityAttribute(obj.name = nome.oggetto,id = obj.pk,attr.name = attributo)
        return(list( "valore"=res,
                     "operation.token" = "getEntityAttribute",
                     "operation"=stringa))
      }
      else {
        # E' una relazione fra due classi?
        if(LLL.env$is.relation.of(className = nome.oggetto, relName=attributo) == TRUE) {
          res <- LLL.env$getEntityRelation(obj.name = nome.oggetto,id = obj.pk, relation.name = attributo)
          return(list( "valore"=res,
                       "operation.token" = "getEntityRelation",
                       "operation"=stringa))
        }
        else stop("\n non e' un attributo ne' un metodo ne' una relazione... errore: che e'?")
      }
      stop("\n non dovrei essere qui!!! (v2)")
    }
    stop("\n non dovrei essere qui!!! (v1)")    
  }
  
  # ----------------------------------------------------
  # SET
  # ----------------------------------------------------
  risolvi.set<-function( stringa ) {
    
    nome.variabile <- str_extract(string = sub("^[ ]*set[ ]*", "\\1", stringa),pattern = "[A-Za-z0-9._]*")
    secondo.membro <- sub("^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=[ ]*","\\1",stringa)
    
    # Se il secondo membro e' un numero non stare a farti tante seghe...
    if( is.a.number(secondo.membro) == TRUE ) {
      proxy.mem.struct.set(varName = nome.variabile, value = as.numeric(secondo.membro), type = "numeric")
      return( list(
        "valore" = NA,
        "operation.token" = "SET",
        "operation"=stringa
      ) )
    }
    if( is.a.quoted.string(secondo.membro) ) {
      proxy.mem.struct.set(varName = nome.variabile, value = togli.apici.esterni.stringa(secondo.membro), type = "string")
      return( list(
        "valore" = secondo.membro,
        "operation.token" = "SET",
        "operation"=stringa
      ) )      
    }

    res <- invoca.ricorsivamente.HLL(HLL.script =  secondo.membro)
    
    tipo.variabile.restituita <- "unknown"
    if(is.a.number(res$valore)==TRUE) { tipo.variabile.restituita <- "numeric" }
    if(is.a.string(res$valore)==TRUE) { tipo.variabile.restituita <- "string" }
    if(is.a.quoted.string(res$valore)==TRUE) { tipo.variabile.restituita <- "quoted.string" }
    if(is.a.numeric.array(res$valore)==TRUE) { tipo.variabile.restituita <- "numeric.array" }
    if(is.a.string.array(res$valore)==TRUE) { tipo.variabile.restituita <- "string.array" }
    
    if(tipo.variabile.restituita == "unknown") nuovo.valore <- res$valore
    if(tipo.variabile.restituita == "numeric") nuovo.valore <- as.numeric(res$valore)
    if(tipo.variabile.restituita == "string") nuovo.valore <- res$valore
    if(tipo.variabile.restituita == "quoted.string") nuovo.valore <- res$valore
    if(tipo.variabile.restituita == "numeric.array") nuovo.valore <- as.numeric(res$valore)
    if(tipo.variabile.restituita == "string.array") nuovo.valore <- res$valore
    
    if(tipo.variabile.restituita == "unknown") stop("\n caso strano di tipo variabile non identificata")
    
    # Aggiorna la memoria
    proxy.mem.struct.set(varName = nome.variabile,value = nuovo.valore,type = tipo.variabile.restituita)
    
    # ritorna il risultato (cioè un NA e la definizione dell'operazione effettuata)
    return( list(
      "valore" = NA,
      "operation.token" = "SET",
      "operation"=stringa
    ) ) 
    
  }
  # ********************************************************************
  # FINE Sezione di risoluzione della semantica
  # ********************************************************************  

  # ----------------------------------------------------------------
  # preProcessing.Script
  # Fai il pre-processing dello script per identificare le posizioni degli 
  # if-then-else, for, etc..
  # ----------------------------------------------------------------
  preProcessing.Script<-function( script.lines ) {
    command <- list()
    # browser()
    # Scorri tutte le righe alla ricerca degli elementi che possono interessare
    # (IF, FOR, etc..)
    matrice <- c(); lst.tmp.ris<-list()
    for(riga in 1:length(script.lines)) {
      command["if"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*if[ ]*\\(.*\\)[ ]*then[ ]*$")
      command["endif"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*endif[ ]*$")
      command["else"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*else[ ]*$")
      # E' un IF?
      if(!is.na(command["if"])) {
        # Fai il preprocessing della riga di IF, per estrarre su quali variabili lavora
        lst.tmp.ris[[as.character(riga)]] <- pre.processing.if( script = script.lines, num.riga = riga)
        matrice <- rbind(matrice, c(riga, "if",FALSE))
      }
      # E' un endif?
      if(!is.na(command["endif"])) {
        matrice <- rbind(matrice, c(riga, "endif",FALSE))
      } 
      # E' un endif?
      if(!is.na(command["else"])) {
        matrice <- rbind(matrice, c(riga, "else",FALSE))
      }        
    }
    
    # Ora ricava la struttura degli if-then-else in tutto lo script, arricchendo la lista
    # costruita fino ad ora. (cosi' mi sara' piu' facile, a run-time, zompare qua e la' perche'
    # gia' conoscero' la struttura)
    lst.tmp.ris <- ricava.struttura.if( matrice = matrice, lst.tmp.ris = lst.tmp.ris )
  
    return( 
      list( "if.else.endif"= lst.tmp.ris )
    )
    
  }
  
  ricava.struttura.if<-function( matrice , lst.tmp.ris ) {
    # Se ci sono degli IF, ricava le posizioni dei corrispondenti ELSE, ENDIF, cosi' da rendere
    # piu' facile il calcolo successivamente
    if(length(matrice)>0) {
      matrice <- rbind(matrice, c("dummy","dummy",FALSE))
      colnames(matrice)<-c("row","command","assigned")
      numeri.if <- length(matrice[ which( matrice[,"command"]=="if" ),"row" ])
      submatrice <- matrice
      if.assegnati <- 0
      # browser()
      # cicla fino a che non hai assegnato tutti gli IF
      while(if.assegnati < numeri.if)  {
        cur <- 1
        just.assigned <- FALSE
        while(cur <= nrow(submatrice)) { 
          # Se hai appena assegnato, riposiziona il cursore all'inizio
          if( just.assigned == TRUE)  {
            cur <- 1;  submatrice <- submatrice[ which( submatrice[,"assigned"]=="FALSE" ), ]
          }  
          just.assigned <- FALSE
          
          if( submatrice[cur,"command"]=="if" & submatrice[cur+1,"command"]=="endif"  ) {
            submatrice[cur,"assigned"]<-TRUE; submatrice[cur+1,"assigned"]<-TRUE
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.if <- submatrice[cur,"row"]
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.else <- NA
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.endif <- submatrice[cur+1,"row"]
            if.assegnati <-  if.assegnati +1
            just.assigned <- TRUE
          }
          if( submatrice[cur,"command"]=="if" & submatrice[cur+1,"command"]=="else" &
              submatrice[cur+2,"command"]=="endif" ) {
            submatrice[cur,"assigned"]<-TRUE; submatrice[cur+1,"assigned"]<-TRUE; submatrice[cur+2,"assigned"]<-TRUE
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.if <- submatrice[cur,"row"]
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.else <- submatrice[cur+1,"row"]
            lst.tmp.ris[[ submatrice[cur,"row"] ]]$riga.endif <- submatrice[cur+2,"row"]
            if.assegnati <-  if.assegnati +1
            just.assigned <- TRUE
          }
          if(if.assegnati >= numeri.if) break
          cur <- cur +1
        }
      }
    }
    
    return( "lst.tmp.ris"=lst.tmp.ris)
  }
  pre.processing.if<-function(  script , num.riga ) {
    
    stringa <- script[num.riga]
    # ESTRAI LA CONDIZIONE dell'IF
    tmp.stringa <- str_sub(string = stringa,
                           start = str_locate(string = stringa,pattern = "\\(")[1,1]+1,
                           end = str_length(stringa))
    finale <- str_locate_all(string = tmp.stringa, pattern = "\\)")
    tmp.stringa <- str_sub( string = tmp.stringa,
                            start = 1,
                            end = unlist(finale)[  length(unlist(finale)) ]-1 )
    tmp.stringa <- str_trim(tmp.stringa)
    # browser()
    # preferisco lavorare con 'stringa'
    stringa <- tmp.stringa
    
    # sostituisci "#" laddove il contenuto della condizione e' fra virgolette
    dentro <- FALSE
    new.stringa <- ''
    for( i in 1:str_length(tmp.stringa)) {
      if( str_sub( tmp.stringa , i, i)=="'" & dentro == FALSE) { dentro <- TRUE  } 
      else {
        if( str_sub( tmp.stringa , i, i)=="'" & dentro == TRUE) {   dentro <- FALSE   }
      }
      if(dentro == TRUE ) { new.stringa <- str_c(str_sub(new.stringa,1,i-1),"#")  } 
      else {
        if( str_sub( tmp.stringa , i, i)=="'")       new.stringa <- str_c(str_sub(new.stringa,1,i-1),"#")
        else  new.stringa <- str_c(new.stringa,str_sub(tmp.stringa,i,i))
      }
    }
    # browser()
    # splitta il testo rispetto ai marcatori di fine per identificare le possibili variabili
    sep <- c("'","\""," ","+","-","*","/","(",")","|","&","=","!",">","<")
    # browser()
    lst.parole <- list()
    num.parola <- 1
    new.new.stringa <- ""
    for(i in seq(1:str_length(new.stringa))) {
      if( str_sub(string = new.stringa,start = i,end = i) %in% sep ) {
        new.new.stringa <- str_c(str_sub( new.new.stringa , start = 1,end = i-1),"#")
      }
      else { 
        new.new.stringa <- str_c(str_sub( new.new.stringa , start = 1,end = i-1),str_sub(string = new.stringa,start = i,end = i))
      }
    }
    
    # aggiungi ai margini il carattere speciale, per consentire in find anche di elementi agli estremi
    new.new.stringa <- str_c("#",new.new.stringa,"#")
    # browser()
    # cerca ed identifica la posizione per ogni possibile variabile
    tmp.interestingWords <- unlist(str_split(new.new.stringa,pattern = "#"))
    interestingWords <- c()
    for(i in 1:length(tmp.interestingWords)) { 
      if(str_length(tmp.interestingWords[i])>0) interestingWords <- c(interestingWords,tmp.interestingWords[i])
    }
    
    # mi basta prenderle una volta sola
    interestingWords <- unique(interestingWords)
    interestingWords <- interestingWords[ unlist(lapply(interestingWords,is.a.number)) ==FALSE ]
    
    # estrai le posizioni
    lst.Words <- list()
    matriciazza<- c()
    if( length(interestingWords) > 0 ) { 
      for(i in 1:length(interestingWords)) {
        dove <- str_locate_all(string = new.new.stringa,pattern = paste(c("#",interestingWords[i],"#"),collapse = '') )
        for(kkk in 1:nrow(dove[[1]])) {
          # browser()
          dove[[1]][kkk,"start"] <- dove[[1]][kkk,"start"] - 1
          dove[[1]][kkk,"end"] <- dove[[1]][kkk,"end"] - 2
          matriciazza <- rbind(matriciazza,c(  as.character(interestingWords[i]) , dove[[1]][kkk,]) )
        }
        lst.Words[[ as.character(interestingWords[i])]] <- dove
        
      }
      
      colnames(matriciazza)<-c("token","from","to")
      # mettila in ordine perche' le occorrenze multiple potrebbero non vedere 
      # le righe nello stesso ordine con cui compaiono nella stringa
      matriciazza <- matriciazza[  sort(x = as.numeric(matriciazza[,"from"]),index.return=TRUE)$ix, ]
      matriciazza <- matrix(matriciazza,ncol=3)
      colnames(matriciazza)<-c("token","from","to")
  
      
      new.new.stringa <- substr(new.new.stringa,2,str_length(new.new.stringa)-1)
      arr.da.ricomporre <- c()
      # browser()
      tmptmptmp <- str_sub(stringa,0,as.numeric(matriciazza[1,"from"]) )
      arr.da.ricomporre <- c(arr.da.ricomporre,tmptmptmp)
      for(riga in 1:nrow(matriciazza)-1) {
        arr.da.ricomporre <- c(arr.da.ricomporre, matriciazza[riga,"token"])
        tmptmptmp <- str_sub(stringa,as.numeric(matriciazza[riga,"to"])+1,as.numeric(matriciazza[riga+1,"from"]) )
        arr.da.ricomporre <- c(arr.da.ricomporre,tmptmptmp)
      }
      arr.da.ricomporre <- c(arr.da.ricomporre,matriciazza[nrow(matriciazza),"token"])
      tmptmptmp<-str_sub(stringa,as.numeric(matriciazza[nrow(matriciazza),"to"])+1,str_length(stringa) )
      arr.da.ricomporre <- c(arr.da.ricomporre,tmptmptmp)
    }
    else {  
      # browser()
      arr.da.ricomporre <- c(stringa)
      names(arr.da.ricomporre)<-""
    }
    # browser()
    
    return(list("toResolve"=interestingWords,
                "positions"=lst.Words,
                "tmp.stringa"=tmp.stringa,
                "matriceCompletaPosizioniToken" = matriciazza,
                "arrayCondizioneDaRicostruire" = arr.da.ricomporre
          )
    )    
  }
  
  # ----------------------------------------------------------------
  # setEnv
  # Setta il contesto, ovvero carica lo schema LLL da usare
  # ----------------------------------------------------------------
  setEnv<-function( env = list(), mem = list(), classMethods=list() ) {
    if(length(env)>0) LLL.env <<- env
    if(length(mem)>0) mem.struct <<- mem
    if(length(classMethods)>0) mem.struct$class.methods <<- classMethods
  }
  # ----------------------------------------------------------------
  # Costruttore
  # ----------------------------------------------------------------
  costructor<-function( ) {
    LLL.env<<-NA
    mem.struct<<-list()
    mem.struct$var<<-list()
    mem.struct$define.context<<-list()
    mem.struct$implicit.PK<<-NA
    mem.struct$script.structures <<-list()
  }
  costructor()
  # ----------------------------------------------------------------
  # RETURN di classe
  # ----------------------------------------------------------------
  return(
    list(
      "parseScript"=parseScript,
      "setEnv"=setEnv,
      "get"=get,
      "execute"=execute,
      "getClassMethods"=getClassMethods
    )
  )

}
