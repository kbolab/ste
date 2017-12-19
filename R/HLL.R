#' High Level Language Class
#'
#' @description  The engine class to parse and execute HLL scripts
#' @import stringr
#' @export
HLL <- function( debug.mode = FALSE, deep.level = 1, executed.statements = 0, max.debug.deep = Inf, arr.breakpoints = c() ) {

  LLL.env <- NA
  mem.struct <- list()
  global.null.value <- ""
  global.debug.mode <- ""
  global.deep.level <- ""
  global.executed.statements <- ""
  global.max.debug.deep <- ""
  global.arr.breakpoints<- c()

  # ---------------------------------------------------------------
  # Fai il PARSE di uno script
  # (Fai anche il Parsing SEMANTICO)
  # ---------------------------------------------------------------  
  parseScript<-function( script ) {

    # setta il modo di debug
    # global.debug.mode <<- debug.mode
    # global.deep.level <<- deep.level
    
    # splitta le righe ed elimina le righe vuote
    arr.righe <- str_trim(unlist(str_split(string = script,pattern = "\n")))

    script.terminato <- FALSE
    indice.riga <- 1
    while( script.terminato == FALSE ) {
      
      # il comando in questione e' una istruzione di salto?
      # (setta il def a no)
      jump.statement <- FALSE
      
      # Se superi il numero di righe massime... esci
      if(indice.riga > length(arr.righe)) { script.terminato <- TRUE; break  }

      # trimma e skippa le righe vuote. Non dovrei averne, ma non si sa mai.
      str.riga <- str_trim(arr.righe[indice.riga])
      if(str.riga=="") { indice.riga<-indice.riga+1; next; }
      if(str_sub(str_trim(str.riga),start = 1,end = 1)==":") { indice.riga<-indice.riga+1; next; }
      if(str_sub(str_trim(str.riga),start = 1,end = 1)=="#") { indice.riga<-indice.riga+1; next; }

      # Parse della stringa:
      # SE NON E' APERTO UN CONTESTO :
      if( length(mem.struct$define.context)==0) {
      # browser()
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
      
      if(res$operation.token=="IF") { jump.statement <-TRUE ; }
      if(res$operation.token=="ENDIF") { jump.statement <-TRUE ; }
      if(res$operation.token=="ELSE") { jump.statement <-TRUE ;  }
      if(res$operation.token=="ENDFOREACH") { jump.statement <-TRUE ;  }

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
    
    # # DEBUGGER -im
    # if(global.debug.mode==TRUE & global.deep.level <= global.max.debug.deep  ) {
    #   cat("\n#",global.executed.statements,": HLL::execute(",HLL.script,")")
    #   global.executed.statements <<- global.executed.statements + 1
    # }
    # # DEBUGGER -fm
    
    # Crea un oggetto HLL
    obj.HLL<-HLL( debug.mode = global.debug.mode , deep.level = (global.deep.level+1) , 
                  executed.statements = global.executed.statements, max.debug.deep = global.max.debug.deep,
                  arr.breakpoints = global.arr.breakpoints )
    # setta l'environment
    obj.HLL$setEnv( env = LLL.env , mem = mem.struct )
    # Fai il parse dello script
    # Esegue il corrispettivo di una sola riga
    # browser()
    res <- obj.HLL$execute( script = HLL.script )
    # restituisci il risultato
    return(res)
  }
  # ---------------------------------------------------------------
  # risolvi.metodo.HLL
  # Risolve ricorsivamente una metodo. Simile invoca.ricorsivamente.HLL
  # deve gestire tuttavia differentemente lo scoping delle variabili
  # ---------------------------------------------------------------
  risolvi.metodo.HLL<-function( classe, metodo, implicit.PK, script = NA , lst.argomenti=list()) {

    # setta l'environment
    # devi passare LLL ed eventuali metodi definiti in HLL, NON le variabili
    tmp.mem.struct <- mem.struct
    tmp.mem.struct$var<-list()
    tmp.mem.struct$define.context<-list()
    tmp.mem.struct$implicit.PK<-NA
    tmp.mem.struct$lst.parameters<-list()
    # browser()
    # Crea un oggetto HLL
    # obj.HLL<-HLL()
    obj.HLL<-HLL( debug.mode = global.debug.mode, deep.level = (global.deep.level+1),
                  executed.statements = global.executed.statements, max.debug.deep = global.max.debug.deep, 
                  arr.breakpoints = global.arr.breakpoints )

    HLL.script <- mem.struct$class.methods[[classe]][[metodo]]$script
    HLL.script <- HLL.script[ 2: (length(HLL.script)-1) ]
    HLL.script <- paste(HLL.script, collapse = "\n")

    # Aggiungi la PK implicita (eventualmente sovrascrivendo)
    tmp.mem.struct$implicit.PK<-implicit.PK
    tmp.mem.struct$running.class<-classe
    tmp.mem.struct$running.method<-metodo
    tmp.mem.struct$lst.parameters<-lst.argomenti

    # DEBUGGER -im -less important
    if(global.debug.mode==TRUE & global.deep.level <= global.max.debug.deep  ) {
      if(is.na(script))
        cat(paste(c("\n#",global.executed.statements,"CALL - HLL::",classe,"::",metodo),collapse = ' '))
      else
        cat(paste(c("\n#",global.executed.statements,"CALL - HLL::",script),collapse = ' '))
      
      if( global.executed.statements %in% global.arr.breakpoints ) handle.debug.console.GUI()      
      global.executed.statements <<- global.executed.statements + 1
    }
    # DEBUGGER -fm    
    
    # Fai il push dell'ENV
    obj.HLL$setEnv( env = LLL.env , mem = tmp.mem.struct )
    # Fai il parse dello script
    res <- obj.HLL$parseScript( script = HLL.script )
    
    # DEBUGGER -im -less important
    # browser()
    if(global.debug.mode==TRUE & global.deep.level <= global.max.debug.deep  ) {
      if(is.na(script))
        cat(paste(c("\n#",global.executed.statements,"BACK - HLL::",classe,"::",metodo),collapse = ' '))
      else
        cat(paste(c("\n#",global.executed.statements,"BACK - HLL::",script),collapse = ' '))
      if( global.executed.statements %in% global.arr.breakpoints ) handle.debug.console.GUI()      
      global.executed.statements <<- global.executed.statements + 1
    }
    # DEBUGGER -fm    
    
    # restituisci il risultato
    return(res)
  }
  # ---------------------------------------------------------------
  # Fai il LAOD di uno script
  # ---------------------------------------------------------------  
  loadScript <- function(filename = NA, script = NA){
    if(!is.na(filename)) {  
      text <- paste(readLines(con = filename,warn = F),collapse = "\n")
      parseScript(script = text )
      return()
    }      
    if(!is.na(script)) { 
      parseScript(script = text)
      return()
    }
    stop("qualcosa devi passare...")
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

      # prima di chiudere il contesto, fai il parsing di quanto fino ad ora accumulato.
      # Considera che lo script e' gia' stato aggiundo dagli "addline"
      method.name <- mem.struct$define.context$method.name
      class.name <- mem.struct$define.context$class.name
      mem.struct$class.methods[[class.name]][[method.name]]$struttura <<- preProcessing.Script( script.lines = mem.struct$define.context$script )
      mem.struct$define.context <<- list()
      return()
    }
    # se no, continua...
    mem.struct$define.context$method.name <<- method.name
    mem.struct$define.context$class.name <<- class.name
    mem.struct$define.context$script <<- c()
    
    if(!("class.methods" %in% names(mem.struct))) mem.struct$class.methods<<-list()
    if(! ( class.name %in% names(mem.struct$class.methods) ) ) mem.struct$class.methods[[class.name]]<<-list()
    if(!(method.name %in% names(mem.struct$class.methods[[class.name]]))) mem.struct$class.methods[[class.name]][[method.name]]<<-list("script"=c(),"struttura"=c())
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
    mem.struct$class.methods[[class.name]][[method.name]]$script <<- c(mem.struct$class.methods[[class.name]][[method.name]]$script,stringa)
  }
  # ---------------------------------------------------------------
  # execute
  # Esegui un singolo comando (o una linea: per quanto possibile
  # risolveraa' ricorsivamente le chiamate)
  # ---------------------------------------------------------------
  execute<-  function( script , complete.script = NA , script.cursor = NA ) {

    stringa <- script
    match.trovato <- FALSE
    res<-list()
    res["set"]<- str_extract(string = stringa, pattern = "^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=")
    res["obj"]<- str_extract(string = stringa, pattern = "^[ ]*^[a-zA-Z _]+\\(.*\\)[ ]*\\..*$")
    res["obj.implicit.PK"]<- str_extract(string = stringa, pattern = "^[ ]*[a-zA-Z]+[a-zA-Z0-9_]*\\.[a-zA-Z]+[a-zA-Z0-9_]*[ ]*$")
    res["obj.with.parameters"]<- str_extract(string = stringa, pattern = "^[ ]*^[a-zA-Z _]+(\\(.*\\))*\\.[a-zA-Z _]+\\(.*\\)$")    
    res["return"]<- str_extract(string = stringa, pattern = "^[ ]*return\\(.*\\)[ ]*$")
    res["define"]<- str_extract(string = stringa, pattern = "^[ ]*define[ ]+[a-zA-Z0-9_]+[ ]+as[ ]+method[ ]+of[ ]+[a-zA-Z0-9_]+[ ]*$")
    res["enddefine"]<- str_extract(string = stringa, pattern = "^[ ]*enddefine[ ]*$")
    res["str_cat"]<- str_extract(string = stringa, pattern = "^[ ]*str_cat\\(.*\\)[ ]*$")
    res["if"]<- str_extract(string = stringa, pattern = "^[ ]*if[ ]*\\(.*\\)[ ]*then[ ]*$")
    res["else"]<- str_extract(string = stringa, pattern = "^[ ]*else[ ]*$")
    res["endif"]<- str_extract(string = stringa, pattern = "^[ ]*endif[ ]*$")
    res["foreach"]<- str_extract(string = stringa, pattern = "^[ ]*foreach[ ]*([a-zA-Z]+[a-zA-Z0-9_]*)[ ]+as[ ]+([a-zA-Z]+[a-zA-Z0-9_]*)[ ]*do[ ]*$")
    res["endforeach"]<- str_extract(string = stringa, pattern = "^[ ]*endforeach[ ]*$")
    
    #  SET
    if(!is.na(res["set"]) & match.trovato == FALSE) {
      toReturn <- risolvi.set( stringa , script.cursor = script.cursor )
      match.trovato <- TRUE
    }
     if(!is.na(res["obj.with.parameters"]) & match.trovato == FALSE) {
       res["obj"] <- NA; res["obj.implicit.PK"] <- NA;
       toReturn <- risolvi.accessoAMetodo.con.parametri( stringa, res )
       match.trovato <- TRUE
     }     
    # Se e' un' accesso ad un oggetto
    if( (!is.na(res["obj"]) | !is.na(res["obj.implicit.PK"])) & match.trovato == FALSE ) {
      toReturn <- risolvi.accessoAMetodo( stringa, res )
      if(is.null(toReturn$valore)) toReturn$valore <- global.null.value
      match.trovato <- TRUE
    }
    #  RETURN
    if(!is.na(res["return"]) & match.trovato == FALSE) {
      toReturn <- risolvi.return( stringa )
      match.trovato <- TRUE
    }
    # DEFINE
    if(!is.na(res["define"]) & match.trovato == FALSE) {
      toReturn <- risolvi.define( stringa )
      match.trovato <- TRUE
    }
    #  ENDDEFINE
    if(!is.na(res["enddefine"]) & match.trovato == FALSE) {
      stop("\n errore, qui ci dovrei arrivare solo senza un contesto aperto... (e se sono qui vuol dire che non ci sono contesti aperti)")
    }
    # STR_CAT
    if(!is.na(res["str_cat"]) & match.trovato == FALSE) {
      toReturn <- risolvi.str_cat( stringa )
      match.trovato <- TRUE
    }
    #  IF
    if(!is.na(res["if"]) & match.trovato == FALSE) {
      toReturn <- risolvi.if( stringa, complete.script = complete.script , script.cursor = script.cursor)
      match.trovato <- TRUE
    }    
    #  ELSE
    if(!is.na(res["else"]) & match.trovato == FALSE) {
      toReturn <- risolvi.else( stringa, complete.script = complete.script , script.cursor = script.cursor)
      match.trovato <- TRUE
    }      
    #  ENDIF
    if(!is.na(res["endif"]) & match.trovato == FALSE) {
      toReturn <- risolvi.endif( stringa, complete.script = complete.script , script.cursor = script.cursor)
      match.trovato <- TRUE
    }        
    #  FOREACH
    if(!is.na(res["foreach"]) & match.trovato == FALSE) {
      toReturn <- risolvi.foreach( stringa, complete.script = complete.script , script.cursor = script.cursor)
      match.trovato <- TRUE
    }       
    #  ENDFOREACH
    if(!is.na(res["endforeach"]) & match.trovato == FALSE) {
      toReturn <- risolvi.endforeach( stringa, complete.script = complete.script , script.cursor = script.cursor)
      match.trovato <- TRUE
    }       
    
    if(match.trovato== TRUE) { 
      if(global.debug.mode==TRUE & global.deep.level <= global.max.debug.deep  & is.numeric(script.cursor) ) {
        barra.t <- paste(c("|",rep("-",global.deep.level),">"),collapse='')
        cat("\n",barra.t,"Lvl:",global.deep.level,"Line:",script.cursor,":#",global.executed.statements,":",script)
        if( global.executed.statements %in% global.arr.breakpoints ) handle.debug.console.GUI()
        global.executed.statements <<- global.executed.statements + 1
      }
      return(toReturn)
    }
    
    # =========================================
    # SYNTAX ERROR!
    # =========================================
    if(match.trovato== FALSE) {
      browser()
      cat( "\n syntax error in resolving: ", stringa )
      stop()
    }
  }
  handle.debug.console.GUI <- function( ){
    keyPressed = ""
    valid.keypressed.keys = c("c","n") 
    while( !(keyPressed %in% valid.keypressed.keys) ) { 
      cat("\n ----------------------------------------------------------------------")
      cat("\n press :")
      cat("\n\t[n]+[enter]: next (1 more line)")
      cat("\n\t[c]+[enter]: continue (shutdown debug mode and continue)")
      cat("\n\t[v]+[enter]: see variables")
      cat("\n\t[q]+[enter]: quit")
      
      keyPressed = readline()
      
      if( keyPressed == "q" ) stop()
      if( keyPressed == "n" )  {
        global.arr.breakpoints <<- unique(c(global.arr.breakpoints, (global.executed.statements+1) ))
      }
      if( keyPressed == "c" )  {
        global.arr.breakpoints <<- c()
      }      
      if( keyPressed == "v" )  {
        cat("\n ----------------------------------------------------------------------")
        print.vars()
      }      
    }
    cat("\n ----------------------------------------------------------------------")
  }
  print.vars<- function( ) {
    cat("\n")
    matrice <- c()
    if(length(mem.struct$var)==0) {
      cat("\n <no vars>"); return()
    }
    for(  ct in seq(1:length(mem.struct$var))) {
      nome <- names(mem.struct$var)[ct]
      tipo <- mem.struct$var[[nome]]$type
      valore <- mem.struct$var[[nome]]$value
      if(length(valore)>1) valore <- paste(valore, collapse = ',')
      matrice <- rbind(matrice, c(nome, tipo, valore) ) 
    }
    colnames(matrice)<-c("nome","tipo","valore")
    print(matrice)
    return()
  }
  # ********************************************************************
  # INIZIO Sezione di risoluzione della semantica
  # ********************************************************************
  
  # ----------------------------------------------------
  # ENDFOREACH
  # ----------------------------------------------------   
  risolvi.endforeach<-function( stringa , complete.script , script.cursor  ) {
    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    matrice <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$foreach$matrice
    riga.di.interesse <- matrice[ which(matrice[,"riga"]==as.character(script.cursor)), ]
    
    cursore <- riga.di.interesse["cursore"]
    arr2run <- riga.di.interesse["array"] 

    # incrementa il CURSOR.INDEX
    mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos <<- mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos +1 

    # e zompa
    return( 
      list( "valore" = NA,
            "operation.token" = "ENDFOREACH",
            "operation" = stringa,
            "setScriptCursorTo" = as.numeric(riga.di.interesse["linkedTo"] )
      )
    )      
    
  }  
  # ----------------------------------------------------
  # FOREACH
  # ----------------------------------------------------   
  risolvi.foreach<-function( stringa , complete.script , script.cursor  ) {

    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    matrice <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$foreach$matrice
    riga.di.interesse <- matrice[ which(matrice[,"riga"]==as.character(script.cursor)), ]

    cursore <- riga.di.interesse["cursore"]
    arr2run <- riga.di.interesse["array"] 
    endForEachLine <- riga.di.interesse["linkedTo"] 
    
    # Prima cerca di capire se c'e' un CONTEXT aperto per questo FOREACH
    # se no, crealo (sempre che la condizione abbia senso)
    if(!(as.character(script.cursor) %in% names(mem.struct$active.loops))) {
      mem.struct$active.loops[[as.character(script.cursor)]] <<- list("active"=TRUE,
                                                                   "cursorIndexPos"=0,
                                                                   "cursorName"=cursore)
    }
    # idem, nel caso in cui fosse stato chiuso precedentemente
    if(mem.struct$active.loops[[as.character(script.cursor)]]$active==FALSE) {
      mem.struct$active.loops[[as.character(script.cursor)]] <<- list("active"=TRUE,
                                                                   "cursorIndexPos"=0,
                                                                   "cursorName"=cursore)
    }

    # Prendi i dati dell'array (in prima battuta vedi solo se esiste nelle variabili in memoria)
    if( !(arr2run %in% names(mem.struct$var))) {
      stop("\n ERRORE, l'array per il FOREACH non e' presente fra le variabili dichiarate")
    }
    # Spiana il cursore, anche se gia' esiste nello spazion delle variabili: RUSPA!
    # (intanto mettici dentro la cosa piu' simile al 'NULL' che conosca)
    mem.struct$var[[cursore]] <<- list( "type" = "NULL", "value" = global.null.value )
    
    # Verifica la condizione per capire che fare
    # L'array ha almeno un elemento? E' diverso dal global.null.value?
    if(mem.struct$var[[arr2run]]$type=="NULL") {
      # devo zompare al ENDOFOREACH: chiudi il loop (settalo come non attivo)
      mem.struct$active.loops[[as.character(script.cursor)]] <<- list("active"=FALSE)
      # e zompa
      return( 
        list( "valore" = NA,
              "operation.token" = "ENDFOREACH",
              "operation" = stringa,
              "setScriptCursorTo" = as.numeric(endForEachLine)+1
        )
      )    
    }
    # Verifica la condizione per capire che fare
    # L'array ha almeno un elemento? E' diverso dal global.null.value?    
    if( length(mem.struct$var[[arr2run]]$value)<=1 ) { 
      if(mem.struct$var[[arr2run]]$value==global.null.value | 
         length(mem.struct$var[[arr2run]]$value)==0) {
        # devo zompare al ENDOFOREACH: chiudi il loop (settalo come non attivo)
        mem.struct$active.loops[[as.character(script.cursor)]] <<- list("active"=FALSE)
        # e zompa
        return( 
          list( "valore" = NA,
                "operation.token" = "ENDFOREACH",
                "operation" = stringa,
                "setScriptCursorTo" = as.numeric(endForEachLine)+1
          )
        )    
      }
    }
    # Ora verifica se il cursor.index e' gia' arrivato in fondo
    if(mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos >= length(mem.struct$var[[arr2run]]$value)) {
      # devo zompare al ENDOFOREACH: chiudi il loop (settalo come non attivo)
      mem.struct$active.loops[[as.character(script.cursor)]] <<- list("active"=FALSE)
      # e zompa
      return( 
        list( "valore" = NA,
              "operation.token" = "ENDFOREACH",
              "operation" = stringa,
              "setScriptCursorTo" = as.numeric(endForEachLine)+1
        )
      )    
    }
    # Ok, se sono qui significa che sono in ballo: 
    # aggiorna il CURSOR.INDEX
    mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos <<- mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos + 1
    # setta il valore del CURSOR
    mem.struct$var[[cursore]]$value <<- mem.struct$var[[arr2run]]$value[ mem.struct$active.loops[[as.character(script.cursor)]]$cursorIndexPos ]
    # Ora prova ad indovinare il tipo di sto' cazzo di cursore
    if( is.na(mem.struct$var[[cursore]]$value ))  { stop("\n ERRORE... vorrei proprio capire come fa ad arrivare NA il cursore, a questo punto...") }
    mem.struct$var[[cursore]]$type <<- definisci.tipo.variabile( mem.struct$var[[cursore]]$value )$tipo.variabile.restituita

    return( 
      list( "valore" = NA,
            "operation.token" = "FOREACH",
            "operation" = stringa
      )
    )  
  }
  # ----------------------------------------------------
  # ENDIF
  # ----------------------------------------------------   
  risolvi.endif<-function( stringa , complete.script , script.cursor  ) {
    
    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    
    arr.endif <- unlist(lapply(mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif, function(x){ x$riga.endif } ))

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
    # browser()
    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    
    arr.else <- unlist(lapply(mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif, function(x){ x$riga.else } ))

    quale_posizione <- which( arr.else == script.cursor,arr.ind = T)
    if(length(quale_posizione)==0) stop("ERRORE: non riesco a trovare la posizione cui e' associato l'ELSE")

    # Se sto leggendo un ELSE e' perche' ero nella condizione ed ora devo saltare all'endif
    # quindi prendi l'endif associato e zompa!
    new.script.cursor <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif[[quale_posizione]]$riga.endif

    return( 
      list( "valore" = NA,
            "operation.token" = "ELSE",
            "operation" = stringa,
            "setScriptCursorTo" = as.numeric(new.script.cursor)
      )
    )     
  }

  # ----------------------------------------------------
  # IF
  # ----------------------------------------------------  
  risolvi.if<-function( stringa , complete.script , script.cursor ) {

    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    # Bene! Dato che sono stato figo ed ho costruito in pre-processing degli 
    # script la struttura degli if, facciamo che ora vado a ripigliarla!
    # (prima vediamo che ci sia, senno': ERROR! )
    if( (as.character(script.cursor) %in%  
          names(mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif))==FALSE) {
      stop("Errore! mi sarei aspettato di trovar parlato di questo if, dal pre-processor dello script!")
    }

    # prendi i nomi delle sospette variabili da sostituire
    sospette.variabili <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif[[as.character(script.cursor)]]$toResolve
    
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
    arrayCondizioneDaRicostruire <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif[[as.character(script.cursor)]]$arrayCondizioneDaRicostruire

    condizione.finale.da.parsare<-c()
    if(length(arrayCondizioneDaRicostruire)>0) { 
      for( i in 1:length(arrayCondizioneDaRicostruire)) { 
        tipo <- names(arrayCondizioneDaRicostruire)[i]
        pezzo <- arrayCondizioneDaRicostruire[i]
        variabile <- arrayCondizioneDaRicostruire[i]
        if(tipo=="token")  { 
          if(tipo.variabile[[variabile]]=="string")
            pezzo <- paste(c("'",valore.trovato[[variabile]],"'"),collapse = '')
          else
            pezzo <- valore.trovato[[variabile]]
          
          if(tipo.variabile[[variabile]]=="null") { 
            pezzo <-  paste(c("'",global.null.value,"'"),collapse = '')
          }            
        }
        condizione.finale.da.parsare <- paste(c(condizione.finale.da.parsare,pezzo),collapse = '')
      }
    } else  { 
      condizione.finale.da.parsare <- arrayCondizioneDaRicostruire
    }

    esito.condizione <- eval(parse(text=condizione.finale.da.parsare))
    
    # Ora cerca di capire dove il cursore di eseuczione dello script dovrebbe venire mosso!
    riga.else <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif[[as.character(script.cursor)]]$riga.else
    riga.endif <- mem.struct$class.methods[[runningClass]][[runningMethod]]$struttura$if.else.endif[[as.character(script.cursor)]]$riga.endif
    
    if(esito.condizione==TRUE) nuova.posizione.cursore <- script.cursor + 1
    if(esito.condizione==FALSE) { 
      if(!is.na(riga.else)) { nuova.posizione.cursore <- as.numeric(riga.else)+1 }
        else  { nuova.posizione.cursore <- as.numeric(riga.endif) }
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
    argomento <- str_trim(argomento)

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
  risolvi.accessoAMetodo.con.parametri<-function( stringa , res) {
    # browser()
    # estrazione preliminare di sottostringhe
    tmp.pos.2 <- str_trim(str_locate(pattern = "^[a-zA-Z]+[a-zA-Z0-9_]*(\\.|\\([a-zA-Z0-9_ ]+\\))" ,string =  stringa ))
    tmp.pos.2<- as.numeric(tmp.pos.2)
    tmp.classe <- str_sub(string = stringa,start = tmp.pos.2[1],end = tmp.pos.2[2])
    last.char <- str_sub(string = tmp.classe, start = str_length(tmp.classe), end = str_length(tmp.classe))
    if(last.char==".") { 
      # prendi la classe
      classe <- str_sub(string = stringa,start = tmp.pos.2[1],end = tmp.pos.2[2]-1)
      # prendi il metodo
      tmp.str.1 <- str_trim(sub("^[a-zA-Z]+[a-zA-Z0-9_]*" ,"\\1", stringa ))
      metodo <- str_sub(string = tmp.str.1,start = 2,end = str_locate(string = tmp.str.1,pattern = "\\(")[1]-1)
      # verifica la PK      
      # browser()
      # Se non e' una relazione, la PK e' obbligatoria
      PK.to.pass <- NA      
      if(LLL.env$is.relation.of(className = classe, relName=metodo) == FALSE) { 
        if(is.na(mem.struct$implicit.PK)) stop("\n non e' stato dichiarata la PK implicita (4)")
      }
      PK.to.pass <- mem.struct$implicit.PK
      # prendi gli argomenti passati 
      tmp.str.2 <- str_trim(sub("^[a-zA-Z]+[a-zA-Z0-9_]*\\.[a-zA-Z0-9_]+\\(" ,"\\1", stringa ))
      tmp.str.3 <- str_sub(string = tmp.str.2,start = 1,end = -2)
      tmp.str.4 <- str_split(string = tmp.str.3,pattern = ",")[[1]]
      arr.argomento <- unlist(lapply(X = tmp.str.4, str_trim ))
    }
    if(last.char==")") { 
      classe <- str_sub(string = tmp.classe,start = 1,end = str_locate(string = tmp.classe,pattern = "\\(")[1]-1)
      PK.to.pass <- str_sub(string = tmp.classe,start = str_locate(string = tmp.classe,pattern = "\\(")[1]+1,end = str_locate(string = tmp.classe,pattern = "\\)")[1]-1)
      tmp.str.1.5 <- str_sub(string = stringa,start = str_length(tmp.classe)+2)
      # prendi il metodo
      metodo <- str_sub(string = tmp.str.1.5,start = 1,end = str_locate(string = tmp.str.1.5,pattern = "\\(")[1]-1)
      # prendi gli argomenti passati 
      tmp.str.3 <- str_trim(str_sub(string = tmp.str.1.5,start = str_locate(string = tmp.str.1.5,pattern = "\\(")[1]+1,end = -2))
      tmp.str.4 <- str_split(string = tmp.str.3,pattern = ",")[[1]]   
      arr.argomento <- unlist(lapply(X = tmp.str.4, str_trim ))
    }
    
    # Ora sbroglia gli argomenti: non vorrei dovessero risolverseli a valle!
    lst.argomento.valori <- c()
    ct <- 1
    for(argomento in arr.argomento) {
      assigned <- FALSE
      # Se il valore dell'argomento e' gia' passato (es: stringa o int)
      if( is.a.number(argomento) | is.a.quoted.string(argomento) & assigned==FALSE ) {
        lst.argomento.valori[[ct]] <- list("value"=argomento,"type"=definisci.tipo.variabile(argomento))
        assigned <- TRUE
      }
      # Se e' una variabile presente in memoria
      if(argomento %in% names(mem.struct$var)  & assigned==FALSE ) {
        lst.argomento.valori[[ct]] <- mem.struct$var[[argomento]]
        assigned <- TRUE
      }
      if( assigned==FALSE ) {
        # Se invece devo risolverlo
        browser()
        stop("\n Ach! non sono ancora pronto a risolvere questo tipo di arogmento... chiama il mainteiner del pacchetto!")
      }
      ct <- ct + 1
    }
    # if(stringa=="Tools.getDeltaDays( ultimaData, dataOdierna )") browser()
    
    ooo <- risolvi.accessoAMetodo(stringa = stringa, res = res , 
                                  nome.oggetto= classe, attributo=metodo, obj.pk=PK.to.pass, 
                                  lst.argomenti=lst.argomento.valori, complex.invokation = TRUE) 
    return(ooo)  
  }
  # ----------------------------------------------------
  # <oggetto>(<id>).<attributo> OPPURE un <oggetto>.<attributo> con un PK implicito
  # ----------------------------------------------------  
  risolvi.accessoAMetodo<-function( stringa , res , 
                                    nome.oggetto=NA, attributo=NA, obj.pk=NA, 
                                    lst.argomenti=c(), complex.invokation=FALSE) {
    
    # # DEBUGGER -im
    # if(global.debug.mode==TRUE & global.deep.level <= global.max.debug.deep  ) {
    #   cat("\n#",global.executed.statements,": HLL::execute(",stringa,")")
    #   # cat("\n#",global.executed.statements,": HLL::execute(",nome.oggetto,"::",attributo,")")
    #   global.executed.statements <<- global.executed.statements + 1
    # }
    # # DEBUGGER -fm
    
    # Due controlli formali di apertura, giusto per gradire (se caso implicito ma manca la PK)
    RelazioneDiTipo <- FALSE
    # browser()
    if(!is.na(res["obj.implicit.PK"])) {
      # Se non e' un oggetto complesso, estrai classe e metodo
      if(complex.invokation==FALSE) { 
        nome.oggetto <- str_trim(sub("\\.[a-zA-Z]+[a-zA-Z0-9_]*[ ]*$" ,"\\1", stringa ))
        attributo <- str_trim(sub("^[ ]*[a-zA-Z]+[a-zA-Z0-9_]*\\." ,"\\1", stringa ))
      }
      
      # Cerca di capire SE e' una relazione e, se no, dai errore in caso in cui manchi la PK'
      # (nel caso di relazione sul solo master, cio' e' ammissibile)
      RelazioneDiTipo <- LLL.env$is.relation.of( nome.oggetto , attributo , whatInfo="type" )
      if(RelazioneDiTipo!="master-only") { 
        if( !("implicit.PK" %in% names(mem.struct)) ) stop("\n non e' stato dichiarata la PK implicita (1)")
        if( is.na(mem.struct$implicit.PK)) stop("\n non e' stato dichiarata la PK implicita (2)")
      }
      obj.pk <- mem.struct$implicit.PK
    }
    else {  # CASO ESPLICITO
      # sempre nel caso non si tratti di un oggetto complesso
      if(complex.invokation==FALSE) {  
        # Estrai 'nome.oggetto', 'obj.pk' e 'attributo'
        obj.pk <- str_trim(sub("[ ]*\\..*$","\\1",stringa))
        obj.pk <- str_trim(sub("^[ ]*^[a-zA-Z _]+\\(","\\1",obj.pk))
        obj.pk <- str_sub(string = obj.pk,  start=1, end=str_length(obj.pk)-1)
        nome.oggetto <- str_trim(sub("+\\(.*\\)[ ]*\\..*$","\\1",stringa))
        attributo <- sub("^[ ]*^[a-zA-Z _]+\\(.*\\)[ ]*\\.[ ]*","\\1",stringa)
      }
    }
    
    need.PK <- LLL.env$is.relation.of( nome.oggetto , attributo , whatInfo="need.PK" )
    
    # invoca eventuali calcoli ricorsivi, per risolvere 'obj.pk' o il valore di 'secondo.membro'
    if( is.a.number(obj.pk) == FALSE & need.PK == TRUE ) {
    # if( is.a.number(obj.pk) == FALSE & RelazioneDiTipo!="master-only") {      
    # if( is.a.number(obj.pk) == FALSE) {
      # Vediamo se e' una variabile :)
      if( obj.pk %in% names(mem.struct[["var"]]) ) {
        if( mem.struct[["var"]][[obj.pk]]$type=="numeric"  ) {
          obj.pk <- mem.struct[["var"]][[obj.pk]]$value
        }
        else { 
          stop("ERRORE: l'id non puo' essere usato come chiave in quanto non numerico") 
        }
      }
      else  {
        stop("-TODO: invoca ricorsivamente il calcolo dell' PK dell'oggetto")
      }
    }
    # due controlli formali, giusto per gradire
    # if(obj.pk=="" & RelazioneDiTipo!="master-only" ) {  cat( "\nmmmhhhh, sono restato senza obj.pk: ", stringa );  stop()  }
    if(obj.pk=="" & need.PK==TRUE ) {  cat( "\nmmmhhhh, sono restato senza obj.pk: ", stringa );  stop()  }
    if(nome.oggetto=="") {  cat( "\nmmmhhhh, sono restato senza nome.oggetto: ", stringa );  stop()  }
    if(attributo=="") {  cat( "\nmmmhhhh, sono restato senza attributo: ", stringa );  stop()  }
    # browser()
    # Se sto cercando di invocare la classe TOOLS!
    if(nome.oggetto=="Tools") {
      obj.tool <- Ste.tool.class()
      val.from.obj.tool <- obj.tool$proxy( stringa = attributo , lst.parametri = lst.argomenti)
      if(val.from.obj.tool$error == TRUE ) stop("Errore non identificato nell'invocare la classe Tools")

      return(list( "valore"=val.from.obj.tool$value,
                   "operation.token" = "Tools::<method>",
                   "operation"=stringa))
    }
    # browser()
    # E' un metodo? (in HLL)
    if(attributo %in% names(mem.struct$class.methods[[nome.oggetto]])) {
      res <- risolvi.metodo.HLL( classe = nome.oggetto , metodo = attributo, implicit.PK = obj.pk , 
                                 lst.argomenti = lst.argomenti)
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
          res <- LLL.env$getEntityRelation(obj.name = nome.oggetto,id = obj.pk, relation.name = attributo, lst.argomenti = lst.argomenti)
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
  risolvi.set<-function( stringa , script.cursor = NA ) {
    
    runningClass <- mem.struct$running.class
    runningMethod <- mem.struct$running.method
    
    nome.variabile <- str_extract(string = sub("^[ ]*set[ ]*", "\\1", stringa),pattern = "[A-Za-z0-9._]*")
    secondo.membro <- sub("^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=[ ]*","\\1",stringa)
    
    # Se il secondo membro e' un parametro, associalo
    if( !is.na(str_extract(string = secondo.membro, pattern = "^[ ]*\\$.*[0-9]+\\$[ ]*$")) ) {
      tmp.secondo.membro <- str_trim(secondo.membro)
      whichParameter <- str_sub(string = tmp.secondo.membro,start = 12,end = str_length(tmp.secondo.membro)-1)
      if( is.numeric(as.numeric(whichParameter)) == FALSE) stop("\n ERRORE: qualcosa non va in come e' stato indicato il parametro: errore di sintassi?")
      whichParameter <- as.numeric(whichParameter)
      if( whichParameter > length(mem.struct$lst.parameters) ) stop("\n ERRORE: qualcosa non va in come e' stato indicato il parametro: parametro non esistente")
      secondo.membro <- mem.struct$lst.parameters[[whichParameter]]$type$risultatoElemento
    }    
    
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
    # Se il secondo membro e' un oggetto, diretto od indiretto, risolvilo senza tante seghe
    test.str<-list()
    test.str["obj"]<- str_extract(string = secondo.membro, pattern = "^[ ]*^[a-zA-Z _]+\\(.*\\)[ ]*\\..*$")
    test.str["obj.implicit.PK"]<- str_extract(string = secondo.membro, pattern = "^[ ]*[a-zA-Z]+[a-zA-Z0-9_]*\\.[a-zA-Z]+[a-zA-Z0-9_]*[ ]*$")
    if( !is.na(test.str["obj"]) | !is.na(test.str["obj.implicit.PK"]) ) {
      # invoca il calcolo
      risultatoElemento <- invoca.ricorsivamente.HLL(HLL.script =  secondo.membro)
      # indovina il tipo
      lst.dati.tipo.variabile <- definisci.tipo.variabile(risultatoElemento  = risultatoElemento$valore)
      # aggiorna la memoria e chiudi
      proxy.mem.struct.set(varName = nome.variabile, value = lst.dati.tipo.variabile$risultatoElemento, type = lst.dati.tipo.variabile$tipo.variabile.restituita)
      return( list(
        "valore" = lst.dati.tipo.variabile$risultatoElemento,
        "operation.token" = "SET",
        "operation"=stringa
      ) )
    }
    
    argomento.multi.token <- FALSE
    if(!is.na(script.cursor)) { 

      if( mem.struct$class.methods[[mem.struct$running.class]][[mem.struct$running.method]]$struttura$set.statement[[as.character(script.cursor)]]$exitCode == 0 ) {
        argomento.multi.token <- TRUE
        mmatrice <- mem.struct$class.methods[[mem.struct$running.class]][[mem.struct$running.method]]$struttura$set.statement[[as.character(script.cursor)]]$matriceElementiRilevati
        # risolvi ogni elemento della matrice (escludendo il primo)
        stringa.parziale <- "";
        for(ct in 1:(dim(mmatrice)[1]))    {

          if(mmatrice[ct,"stato"] == "token" ) {
            # cerca: se e' gia' presente in memoria, usa quello, altrimenti cerca di risolverlo chiamando
            # ricorisvamente il risolutore
            if( mmatrice[ct,"substr"] %in% names(mem.struct$var)) {
              risultatoElemento <- mem.struct$var[[ mmatrice[ct,"substr"] ]]$value
            }
            else {
              tmptmp.risultato <- invoca.ricorsivamente.HLL(HLL.script =  mmatrice[ct,"substr"])
              risultatoElemento <- tmptmp.risultato$valore
            }
            
            # Fai le elucubrazioni per cercare di indovinare il tipo di variabile 
            lst.dati.tipo.variabile <- definisci.tipo.variabile(risultatoElemento  = risultatoElemento) 
            # estrai il tipo ed il valore
            tipo.variabile.restituita <- lst.dati.tipo.variabile$tipo.variabile.restituita
            risultatoElemento <- lst.dati.tipo.variabile$risultatoElemento
            if(tipo.variabile.restituita=="string") risultatoElemento <- str_c("'",risultatoElemento,"'")
            
            if((dim(mmatrice)[1])>2 & (tipo.variabile.restituita == "null" |
                                                  tipo.variabile.restituita == "numeric.array" |
                                                  tipo.variabile.restituita == "string.array") ) {
              stop("\n Errore, in un set ci sono piu' elementi che concorrono alla risoluzione ma almeno uno di essi e' un array o vale NULL")
            }

          } else {
            risultatoElemento <- mmatrice[ct,"substr"]
          }

          stringa.parziale <- str_trim(str_c( stringa.parziale , risultatoElemento ))
        }
        if(stringa.parziale!="null" ) {
          stringa.settatrice <- paste(c("stringa.parziale <- ",stringa.parziale),collapse = '')
          eval(parse(text=stringa.settatrice))              
        }
        else {
          stringa.parziale <- stringa.parziale
        }       
      }
    }
    
    if(argomento.multi.token == FALSE) { 
      res <- invoca.ricorsivamente.HLL(HLL.script =  secondo.membro)
    } else { res <- list("valore"=stringa.parziale ) }
    
    tipo.variabile.restituita <- "unknown"
    if(is.null(res$valore)==TRUE) { tipo.variabile.restituita <- "null" }
    else { 
      if(is.a.number(res$valore)==TRUE) { tipo.variabile.restituita <- "numeric" }
      if(is.a.string(res$valore)==TRUE) { tipo.variabile.restituita <- "string" }
      if(is.a.quoted.string(res$valore)==TRUE) { tipo.variabile.restituita <- "quoted.string" }
      if(is.a.numeric.array(res$valore)==TRUE) { tipo.variabile.restituita <- "numeric.array" }
      if(is.a.string.array(res$valore)==TRUE) { tipo.variabile.restituita <- "string.array" }
    }
      
    if(tipo.variabile.restituita == "null") nuovo.valore <- global.null.value
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
  # ----------------------------------------------------------------
  # definisci.tipo.variabile
  # cerca di indovinare il tipo della variabile in funzione del suo contenuto
  # ----------------------------------------------------------------  
  definisci.tipo.variabile<-function(risultatoElemento) { 

    tipo.variabile.restituita <- "unknown"
    if(is.null(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "null" }
    else {
      if(is.a.number(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "numeric" }
      if(is.a.string(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "string" }
      if(is.a.quoted.string(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "quoted.string" }
      if(is.a.numeric.array(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "numeric.array" }
      if(is.a.string.array(risultatoElemento)==TRUE) { tipo.variabile.restituita <- "string.array" }
    }
    
    if(tipo.variabile.restituita == "numeric") risultatoElemento <- as.numeric(risultatoElemento)
    if(tipo.variabile.restituita == "string") risultatoElemento <- risultatoElemento
    if(tipo.variabile.restituita == "quoted.string") risultatoElemento <- risultatoElemento
    if(tipo.variabile.restituita == "unknown") stop("\n caso strano di tipo variabile non identificata")
    
    # i casi di 'null'  e/o di array vanno bene SOLO se e' una assegnazione diretta!!!!!
    if(tipo.variabile.restituita == "null") risultatoElemento <- global.null.value
    if(tipo.variabile.restituita == "numeric.array") risultatoElemento <- as.numeric(risultatoElemento)
    if(tipo.variabile.restituita == "string.array") risultatoElemento <- risultatoElemento
    
    return(list(
              "tipo.variabile.restituita"=tipo.variabile.restituita,
              "risultatoElemento"=risultatoElemento
              ))
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
    
    # accorcia script.lines togliendo la prima riga che e' solo 
    # relativa alla definizione della procedura
    script.lines <- script.lines[2:length(script.lines)] 
    
    # Scorri tutte le righe alla ricerca degli elementi che possono interessare
    # (IF, FOR, etc..)
    matrice <- c(); matrice.set <- c(); matrice.foreach <- c()
    lst.tmp.ris<-list(); lst.set.stt <- list(); lst.tmp.foreach <- list()
    
    for(riga in 1:length(script.lines)) {
      command["if"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*if[ ]*\\(.*\\)[ ]*then[ ]*$")
      command["endif"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*endif[ ]*$")
      command["else"]<- str_extract(string = script.lines[riga] , pattern = "^[ ]*else[ ]*$")
      command["set"]<- str_extract(string = script.lines[riga], pattern = "^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=")
      command["foreach"]<- str_extract(string = script.lines[riga], pattern = "^[ ]*foreach[ ]*([a-zA-Z]+[a-zA-Z0-9_]*)[ ]+as[ ]+([a-zA-Z]+[a-zA-Z0-9_]*)[ ]*do[ ]*$")
      command["endforeach"]<- str_extract(string = script.lines[riga], pattern = "^[ ]*endforeach[ ]*$")
      
      # E' un FOREACH?
      if(!is.na(command["foreach"])) {
        # Fai il preprocessing della riga di IF, per estrarre su quali variabili lavora
        matrice.foreach <- rbind(matrice.foreach, pre.processing.foreach( script = script.lines, num.riga = riga))
        colnames(matrice.foreach)<-c("riga","tipo","cursore","array","linkedTo","associato")
      }  
      # E' un ENDFOREACH?
      if(!is.na(command["endforeach"])) {
        # Fai il preprocessing della riga di IF, per estrarre su quali variabili lavora
        matrice.foreach <- rbind(matrice.foreach,c(riga,"endforeach","","","",""))
      }      
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
      # E' un else?
      if(!is.na(command["else"])) {
        matrice <- rbind(matrice, c(riga, "else",FALSE))
      }        
      # E' un set?
      if(!is.na(command["set"])) {
        tmp.poss <- str_locate(string = script.lines[riga],pattern = "=" )
        tmp.set <- str_locate(string= script.lines[riga],pattern = "set" )
        variabile <- str_trim(str_sub(string = script.lines[riga],start = tmp.set[1,"end"]+1,end = tmp.poss[1,"end"]-1))
        argomento <- str_trim(str_sub(string = script.lines[riga],start = tmp.poss[1,"end"]+1 ,end = str_length(script.lines[riga])))

        # Prendila matrice dei token con le relative posizioni
        lst.set.stt[[as.character(riga)]]  <-  ricavaElementiDaRisolvereDaStringa( argomento )
      }           
    }

    # Ora ricava la struttura degli if-then-else in tutto lo script, arricchendo la lista
    # costruita fino ad ora. (cosi' mi sara' piu' facile, a run-time, zompare qua e la' perche'
    # gia' conoscero' la struttura)
    lst.tmp.ris <- ricava.struttura.if( matrice = matrice, lst.tmp.ris = lst.tmp.ris )
    # browser()
    if(!is.null(matrice.foreach))
      matrice.foreach <- completa.matrice.foreach( matrice = matrice.foreach )

    return( 
      list( "if.else.endif"= lst.tmp.ris,
            "set.statement" = lst.set.stt,
            "foreach" = list("matrice"=matrice.foreach) )
    )
    
  }
  # ----------------------------------------------------------
  # completa.matrice.foreach
  #     Completa la matrice dei foreach associando i numeri fra i
  #     foreach e gli endforeach, cosi' da consentire in fase di calcolo di non
  #     stare a doverli cercare
  # ----------------------------------------------------------
  completa.matrice.foreach<-function( matrice  ) {
    # Fallo di default per un numero di volte pari al numero dei foreach 
    # (caso in cui siano tutti nidificati ). Se anche lo fa qualche volta di piu'
    # non succede niente
    for( i in 1:sum(matrice[,"tipo"]=="endforeach") ) { 
      # scorri tutta la matrice, a scendere
      for( numRiga in 1:nrow(matrice))  {  
        # se trovi delle coppie adiacenti, non ancora linkate, linkale
        if(matrice[numRiga,"tipo"]=="endforeach" & matrice[numRiga,"linkedTo"]=="")  {
          dove.possibile <- which(matrice[,"tipo"]=="foreach" & matrice[,"linkedTo"]=="" 
                                  & as.numeric(matrice[numRiga,"riga"])> as.numeric(matrice[,"riga"]) )
          candidato <- sort(dove.possibile,decreasing = T)[1]
          if( length(candidato)>0 )  {
            # ovviamente il link e' reciproco
            matrice[numRiga,"linkedTo"] <- matrice[candidato,"riga"]
            matrice[candidato,"linkedTo"] <- matrice[numRiga,"riga"]
          }
        }
      }
    }
    # se ancora ho qualcosa di non linkato, errore!
    aaa <- which(matrice[,"linkedTo"]=="" )
    if(length(aaa)>0) stop("\nmaremma, no.... l'architetture dei foreach/endforeach non e' corretta: verifica!")
    return(matrice)
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
  pre.processing.foreach<-function( script , num.riga, complete.script  ) {
    stringa <- script[num.riga]
    # browser()
    # prima di tutto estrai il nome del cursore e dell'array su cui si scorre
    pos.1 <- str_locate( string = stringa,pattern = "^[ ]*foreach[ ]*")
    array.run <- str_sub(string = stringa,start = pos.1[,"end"]+1)
    pos.2 <-str_locate(string = array.run, pattern = "[ ]+as[ ]+([a-zA-Z]+[a-zA-Z0-9_]*)[ ]*do[ ]*$")
    array.run <- str_trim(str_sub(string = array.run,start = 1, end = pos.2[,"start"]-1))
    pos.3 <- str_locate(string = stringa, pattern = "^[ ]*foreach[ ]*([a-zA-Z]+[a-zA-Z0-9_]*)[ ]+as[ ]+")
    pos.4 <- str_locate(string = stringa, pattern = "[ ]*do[ ]*$")
    cursore <- str_trim(str_sub(string = stringa, start = pos.3[ ,"end"]+1, end = pos.4[ ,"start"]-1))

    return(c(num.riga,"foreach",cursore,array.run,"",""))
  }
  
  pre.processing.if<-function(  script , num.riga , argomento.gia.estratto = NA) {
    
    if(is.na(argomento.gia.estratto)) { 
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
      # preferisco lavorare con 'stringa'
      stringa <- tmp.stringa
    } else { stringa <- argomento.gia.estratto; tmp.stringa <- argomento.gia.estratto }
    
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
    # splitta il testo rispetto ai marcatori di fine per identificare le possibili variabili
    sep <- c("'","\""," ","+","-","*","/","(",")","|","&","=","!",">","<")

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
      arr.da.ricomporre <- c(stringa)
      names(arr.da.ricomporre)<-""
    }

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
  setEnv<-function( env = list(), mem = list(), classMethods=list(), debug.mode = NA, max.debug.deep = NA, arr.breakpoints = c() ) {
    if(length(env)>0) LLL.env <<- env
    if(length(mem)>0) mem.struct <<- mem
    if(length(classMethods)>0) mem.struct$class.methods <<- classMethods
    if(!is.na(debug.mode)) global.debug.mode <<- debug.mode
    if(!is.na(max.debug.deep)) global.max.debug.deep<<- max.debug.deep
    # if(length(arr.breakpoints)!=0) global.arr.breakpoints<<- arr.breakpoints
  }
  getClassMethods<-function(  ) {
    return(mem.struct$class.methods)
  }  
  get<-function(  ) {
    return(mem.struct)
  }    
  # ----------------------------------------------------------------
  # Costruttore
  # ----------------------------------------------------------------
  costructor<-function( debug.mode , deep.level , executed.statements, max.debug.deep, arr.breakpoints  ) {
    LLL.env<<-NA
    mem.struct<<-list()
    mem.struct$var<<-list()
    mem.struct$define.context<<-list()
    mem.struct$implicit.PK<<-NA
    mem.struct$script.structures <<-list()
    mem.struct$active.loops <<-list()
    mem.struct$lst.parameters <<- list()
    global.null.value <<-"null"
    global.debug.mode <<- debug.mode
    global.deep.level <<- deep.level
    global.executed.statements <<- executed.statements
    global.max.debug.deep <<- max.debug.deep
    global.arr.breakpoints <<- arr.breakpoints    
  }
  costructor( debug.mode = debug.mode, deep.level = deep.level, executed.statements = executed.statements, 
              max.debug.deep = max.debug.deep, arr.breakpoints = arr.breakpoints )
  # ----------------------------------------------------------------
  # RETURN di classe
  # ----------------------------------------------------------------
  return(
    list(
      "loadScript"=loadScript,
      "parseScript"=parseScript,
      "setEnv"=setEnv,
      "get"=get,
      "execute"=execute,
      "getClassMethods"=getClassMethods
    )
  )

}
