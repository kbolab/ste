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
    arr.righe<-arr.righe[which(arr.righe!="")]

    for( indice.riga in 1:length(arr.righe) ) {

      # trimma e skippa le righe vuote. Non dovrei averne, ma non si sa mai.
      str.riga <- str_trim(arr.righe[indice.riga])
      if(str.riga=="") next;
      if(str_sub(str_trim(str.riga),start = 1,end = 1)==":") next;
      if(str_sub(str_trim(str.riga),start = 1,end = 1)=="#") next;

      # Parse della stringa:
      # SE NON E' APERTO UN CONTESTO :
      if( length(mem.struct$define.context)==0) {
        # browser()
        # if(str.riga == "set mannaggia = 100") browser()
        # identifica il contesto della definizione ed il nome dell'oggetto
        # if(str.riga=="return('Abbubbate')") browser()
        res <- execute(script = str.riga)
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
  execute <-  function( script ) {
    # cat("\n ",script)

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

    # =========================================
    # Se e' un SET
    # =========================================
    # browser()
    if(!is.na(res["set"])) {
      # browser()
      nome.variabile <- str_extract(string = sub("^[ ]*set[ ]*", "\\1", stringa),pattern = "[A-Za-z0-9._]*")
      secondo.membro <- sub("^[ ]*set[ ]+[A-Za-z0-9._]+[ ]*=[ ]*","\\1",stringa)

      # Se il secondo membro e' un numero non stare a farti tante seghe...
      if( is.a.number(secondo.membro) == TRUE ) {
        proxy.mem.struct.set(varName = nome.variabile, value = as.numeric(secondo.membro), type = "numeric")
        # mem.struct[["var"]][[nome.variabile]]$value <<- as.numeric(secondo.membro)
        # mem.struct[["var"]][[nome.variabile]]$type <<- "numeric"
        return( list(
          "valore" = NA,
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
    # =========================================
    # Se e' un' accesso ad un oggetto
    # <oggetto>(<id>).<attributo> OPPURE un <oggetto>.<attributo> con un PK implicito
    # =========================================
    if(!is.na(res["obj"]) | !is.na(res["obj.implicit.PK"])) {

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
    # =========================================
    # Se e' un' RETURN
    # =========================================
    if(!is.na(res["return"])) {
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
    # =========================================
    # Se e' un' DEFINE
    # =========================================
    if(!is.na(res["define"])) {
      # Se gia' era aperta una definizione, dai errore
      if(length(mem.struct$define.context)>0) stop("\n sono gia' nel contesto di una definizione: non posso aprirle un'altra")
      nome.metodo <- str_trim(sub("+[ ]+as[ ]+method[ ]+of[ ]+[a-zA-Z0-9_]+[ ]*$","\\1",stringa) )
      nome.metodo <- str_trim(str_sub(string = nome.metodo,start = 7,end = str_length(nome.metodo)))
      nome.classe <- str_trim(sub("^[ ]*define[ ]+[a-zA-Z0-9_]+[ ]+as[ ]+method[ ]+of[ ]+","\\1",stringa) )
      proxy.mem.contesto.set(method.name = nome.metodo,class.name = nome.classe)
      return(list( "valore"=NA, "operation.token" = "define", "operation"=stringa))
    }
    # =========================================
    # Se e' un' ENDDEFINE
    # =========================================
    if(!is.na(res["enddefine"])) {
      # Se gnon ia' era aperta una definizione, dai errore
      stop("\n errore, qui ci dovrei arrivare solo senza un contesto aperto...")
    }
    # =========================================
    # Se e' un STR_CAT
    # =========================================
    if(!is.na(res["str_cat"])) {
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


    # SYNTAX ERROR!
    if(match.trovato== FALSE) {
      cat( "\n syntax error in resolving: ", stringa )
      stop()
    }
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
