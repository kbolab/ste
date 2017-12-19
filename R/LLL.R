#' Low Level Language Class
#'
#' @description  The engine class to parse and execute LLL scripts
#' @import stringr
#' @export
LLL <- function( debug.mode = FALSE ) {

  global.DB.connectors<-list()
  global.obj.writer <- ''
  global.debug.mode <- ''

  mem.struct <- list()
  
  # ---------------------------------------------------------------
  # Fai il LAOD di uno script
  # (Fai anche il Parsing SEMANTICO)
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
    global.obj.writer$send(msg = "ERROR: 'filename' and 'script' are both empty!",queue = "NMI")
  }
  # ---------------------------------------------------------------
  # Fai il PARSE di uno script
  # (Parsing SEMANTICO)
  # ---------------------------------------------------------------
  parseScript<- function( script ) {
    # splitta le definizioni sulla base del "enddefine" in quanto è quello che non ha di fatto attributi
    singole.definizioni <- unlist(str_split(string = script, pattern = "\nenddefine"))
    singole.definizioni <- singole.definizioni[  which(singole.definizioni != "")]

    # per ogni definizione trovata
    for( definizione in 1:length(singole.definizioni) ) {
      # estai le righe
      arr.righe <- unlist(str_split(string = singole.definizioni[definizione],pattern = "\n"))

      if(length(arr.righe)==1) {
        if(arr.righe=="") next;
      }
      # elimina le righe vuote
      arr.righe<-arr.righe[which(arr.righe!="")]
      # identifica il contesto della definizione ed il nome dell'oggetto
      res <- get.nome.e.contesto(stringa = arr.righe[1])
      if(res$error == TRUE ) {
        # cat("\n ",res$err.msg)
        global.obj.writer$send(msg = str_c("\n ",res$err.msg),queue = "NMI")
        # stop()
      }
      str.contesto <- res$str.contesto
      str.nome <- res$str.nome

      # Costruisci l'apposita struttura in memoria
      struttura <- list()

      # scorri ogni riga
      num.riga<-2
      strutt.ospitante <- list()
      for( num.riga in 2:length(arr.righe)) {
        riga <- arr.righe[num.riga]
        # estrai il comando della riga
        str.cmd <- get.comando.di.linea(stringa = riga, contesto = str.contesto)
        if(str.cmd$error == TRUE) {
          global.obj.writer$send(msg = str_c("\n ",str.cmd$err.msg),queue = "NMI")
          # cat("\n ",str.cmd$err.msg)
          # stop()
        }

        # Popola la mem.struct in funzione del numero di elementi del comando parsato
        if(length(str.cmd$comando) == 2) {
          strutt.ospitante[[ str.cmd$comando$w1 ]] <- str.cmd$comando$w2
        }
        if(length(str.cmd$comando) == 3) {
          if(is.null(strutt.ospitante[[ str.cmd$comando$w1 ]])) strutt.ospitante[[ str.cmd$comando$w1 ]] <- list()
          strutt.ospitante[[ str.cmd$comando$w1 ]][[ str.cmd$comando$w2 ]] <- str.cmd$comando$w3
        }
      }

      # Operazioni contesto-dipendenti
      # --- DB Connection ---
      if(str.contesto == "SQLDB") {
        mem.struct[[ str.contesto ]][[ str.nome ]] <<- strutt.ospitante

        user <- mem.struct[[ str.contesto ]][[ str.nome ]]$username
        host <- mem.struct[[ str.contesto ]][[ str.nome ]]$host
        database <- mem.struct[[ str.contesto ]][[ str.nome ]]$database
        pwd <- mem.struct[[ str.contesto ]][[ str.nome ]]$password
        type <- mem.struct[[ str.contesto ]][[ str.nome ]]$type
        if(!("type" %in% names(mem.struct[[ str.contesto ]][[ str.nome ]]))) {
          global.obj.writer$send(msg = str_c(" ERROR:  you did not specified the kind of database"),queue = "NMI")
        }
        mem.struct[[ str.contesto ]][[ str.nome ]]$obj.connector <<- RDBMS(RDBMS.type = type, user = user, password = pwd,host = host, dbname = database  )
      }
      if(str.contesto == "RELATION") {
        master.name <- strutt.ospitante$master

        if(is.null(mem.struct$CLASS[[ master.name ]]$relation)) mem.struct$CLASS[[ master.name ]]$relation <<- list()
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]<<-list()
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]][[ "query" ]]<<-strutt.ospitante$query
      }
      if(str.contesto == "CLASS") {

        if(is.null(mem.struct[[ str.contesto ]][[ str.nome ]])) mem.struct[[ str.contesto ]][[ str.nome ]] <<- list()
        # Copia ogni elemento della struttura di appogio precedentemente creata
        for(elemento in names(strutt.ospitante)) {
          mem.struct[[ str.contesto ]][[ str.nome ]][[ elemento ]]  <<-  strutt.ospitante[[ elemento ]]
        }
      }
    }
  }

  # ---------------------------------------------------------------
  # Identifica il CONTESTO
  # ---------------------------------------------------------------
  get.nome.e.contesto <-  function( stringa ) {
    res.1<- str_locate(string = stringa,pattern = "^[ ]*define[ ]+[A-Za-z0-9._]*")
    res.2<-str_locate(string = stringa,pattern = "^[ ]*define[ ]+")
    # browser()
    if(sum(is.na(res.1)) > 0  ) global.obj.writer$send(msg = str_c("SYNTAX ERROR: (bds8f3) in '",stringa,"'"),queue = "NMI")
    if(sum(is.na(res.2)) > 0  ) global.obj.writer$send(msg = str_c("SYNTAX ERROR: (bds8f4) in '",stringa,"'"),queue = "NMI")
    str.contesto <- str_sub(string = stringa, start =res.2[2]+1, end = res.1[2])
    str.nome <- str_replace_all(
      string = str_sub(string = stringa, start =res.1[2]+1, end = str_length(stringa)),
      pattern = " ",replacement = "")

    if( !(str.contesto %in% c("SQLDB","CLASS","RELATION"))){
      return(list("str.contesto"="","str.nome"="", "error"=TRUE,"err.msg"=paste(c("SYNTAX ERROR: ',",str.contesto,",' is not a valid context in '",stringa,"'"),collapse = '')  ))
    }

    return(list("str.contesto"=str.contesto,"str.nome"=str.nome, "error"=FALSE))
  }

  # ---------------------------------------------------------------
  # Estrai il comendo ed i parametri da una stringa sulla base del contesto
  # (Parsing LESSICALE/SINTATTICO)
  # ---------------------------------------------------------------
  get.comando.di.linea <-  function( stringa , contesto ) {

    if(contesto=="SQLDB"){
      lst.possibili.comandi<-list("database"=list("synt.type"=1),
                                  "host"=list("synt.type"=1),
                                  "username"=list("synt.type"=1),
                                  "password"=list("synt.type"=1),
                                  "type"=list("synt.type"=1)
                                  )
    }
    if(contesto=="CLASS"){
      lst.possibili.comandi<-list("link.name"=list("synt.type"=1),
                                  "table.name"=list("synt.type"=1),
                                  "primary.key"=list("synt.type"=1),
                                  # "filter"=list("synt.type"=1),
                                  "attribute"=list("synt.type"=2)
                                  )
    }

    if(contesto=="RELATION"){
      lst.possibili.comandi<-list("master"=list("synt.type"=1),
                                  "slave"=list("synt.type"=1),
                                  "asc order by"=list("synt.type"=1),
                                  "desc order by"=list("synt.type"=1),
                                  "filter"=list("synt.type"=1),
                                  "query"=list("synt.type"=1)
      )
    }

    
    trovato <- FALSE
    # provali tutti e fermati quando trovi quello giusto
    for(comando in names(lst.possibili.comandi)) {

      # Parsing della sintassi di tipo 1
      if( lst.possibili.comandi[[comando]]$synt.type == 1 ) {
        res.1<- str_locate(string = stringa,pattern = paste( c("^[ ]*",comando,"[ ]*=") ,collapse = '') )
        if(sum(is.na(res.1)) == 0  ) {
          trovato <- TRUE;
          secondo.membro <- str_trim(str_sub(string = stringa,start = res.1[1, "end"] +1,end = str_length(stringa)))
          # trovato: ritorna
          return(list("comando"=list("w1"=comando,"w2"=secondo.membro) , "error"=FALSE))
        }
      }
      # Parsing della sintassi di tipo 2
      if( lst.possibili.comandi[[comando]]$synt.type == 2 ) {
        # estrai la prima parte (il nome del comando)
        res.1<- str_locate(string = stringa,pattern = paste( c("^[ ]*",comando,"[ ]+") ,collapse = '') )
        # se non torna, dai errore di sintassi
        if(!(sum(is.na(res.1)) == 0  ) ) {
          return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("SYNTAX ERROR: (1) in: ",stringa,""),collapse = '')  ))
        }
        # ora estrai la seconda parte del nome
        new.stringa <- str_sub(string = stringa,start = res.1[,"end"], end = str_length(stringa))
        res.2<- str_locate(string = new.stringa,pattern = paste( c("^[A-Za-z0-9_. ]*=") ,collapse = '') )
        if(!(sum(is.na(res.2)) == 0  ) ) {
          return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("SYNTAX ERROR: (2) in: ",stringa,""),collapse = '')  ))
        }
        nome.primo.elemento <- str_trim(str_sub(string = new.stringa,start = res.2[ ,"start"],end = res.2[ ,"end"]-1))
        nome.secondo.elemento <- str_trim(str_sub(string = new.stringa,start = res.2[ ,"end"]+1,end = str_length(new.stringa)))
        # devo gestire il ritorno in questo cso. Conviene modificare anche il cso precednete facendo sì
        # che ritorni una lista, anzichè solo un "comndo"=comando
        return(list("comando"=list("w1"=comando,"w2"=nome.primo.elemento,"w3"=nome.secondo.elemento) , "error"=FALSE))
      }
    }
    # errore di sintassi
    return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("SYNTAX ERROR: (3) in: ",stringa,""),collapse = '')  ))
  }
  is.attribute.of<-function( className , attrName ) {
    if( attrName %in% names(mem.struct$CLASS[[className]]$attribute) ) return(TRUE)
    else return(FALSE)
  }
  is.relation.of<-function( className , relName , whatInfo="exist" ) {
    if(whatInfo=="exist") { 
      if( relName %in% names(mem.struct$CLASS[[className]]$relation) ) return(TRUE)
      else return(FALSE)
    }
    if(whatInfo=="type") { 
      if(is.relation.of( className= className , relName = relName )==FALSE) return(FALSE)
      if("slave.name" %in% names(mem.struct$CLASS[[className]]$relation[[relName]])) return("master-slave")
      return("master-only")
    }    
    if(whatInfo=="need.PK") { 
      if(is.relation.of( className= className , relName = relName )==FALSE) return(TRUE)
      if(is.na(str_locate(string = mem.struct$CLASS[[className]]$relation[[relName]]$query,pattern = "\\$primary.key\\$")[1,1])) return(FALSE)
      return(TRUE)
    }    
    
  }
  getEntityAttribute<-  function( obj.name , id, attr.name  ) {
    regex.sql <- "^[ ]*SQL[ ]*"
    # qualche sano controllo formale
    if(is.null(mem.struct$CLASS[[obj.name]])) global.obj.writer$send(msg = "ERROR: (8yfd87s) missing ENTITY!",queue = "NMI")
    if(is.null(mem.struct$CLASS[[obj.name]]$attribute[[attr.name]])) global.obj.writer$send(msg = "ERROR: (8yfd47s) missing ATTRIBUTE!",queue = "NMI")
    strutt <- mem.struct$CLASS[[obj.name]]
    
    # popola le variabili per fare la query
    campoTabella <- strutt$attribute[[attr.name]]
    nomeTabella <- strutt$table.name
    primary.key <- strutt$primary.key
    table.field <- strutt$attribute[[attr.name]]
    link.name <- strutt$link.name
    DBType <- mem.struct$SQLDB[[link.name]]$type

    # E' di tipo SQL? 
    tipo.attributo <- "normale"
    if(sub(regex.sql,"\\1",table.field) != table.field) tipo.attributo <- "sql"
    
    if(tipo.attributo=="sql")   {
      first <- str_locate_all(string = table.field,pattern = "\\{")[[1]][1]
      second <- str_locate_all(string = table.field,pattern = "\\}")[[1]][1]
      query <- str_trim(str_sub(string = table.field,start = first+1,end = second-1))
      q <- str_replace_all(string = query,pattern = "\\$primary.key\\$",as.character(id))
    }

    if(tipo.attributo=="normale")   {
      q <- ""
      if(DBType=="mysql")
        q <- str_c("select ",table.field," as res from ",nomeTabella," where ",primary.key," = '",id,"';")
      if(DBType=="postgres" | DBType=="postgresql")
        q <- str_c('select "',table.field,'" as res from "',nomeTabella,'" where "',primary.key,'" = \'',id,'\';')
      if( q == "" ) global.obj.writer$send(msg = " ERROR: at the moment the only supported types are 'mysql', 'postgres' and 'postgresql'",queue = "NMI")
        # stop("\n ERRORE: gli unici tipi di DB supportati ad ora sono 'mysql', 'postgres' e 'postgresql'")
    }

    # DEBUGGER -im
    if(global.debug.mode==TRUE ) {
      barra.t <- paste(c(" ",rep(" ",20)," "),collapse='')
      cat("\n",barra.t,q)
      # global.executed.statements <<- global.executed.statements + 1
    }
    # DEBUGGER -fm
    
    # lancia la query
    tmp.res <- mem.struct$SQLDB[[ link.name ]]$obj.connector$query(query = q)

    # Prepara il risultato
    if(dim(tmp.res)[1]==0) res <- c()
    else res <- tmp.res[,1]
    return(res)
  }
  getEntityRelation<-function( obj.name , id , relation.name, lst.argomenti=list()) {
    # browser()

    if(is.null(mem.struct$CLASS[[obj.name]])) 
      global.obj.writer$send(msg = str_c(" ERROR:  missing class named '",obj.name,"' (err: 8yf54)"),queue = "NMI")
    if(is.null(mem.struct$CLASS[[obj.name]]$relation)) 
      global.obj.writer$send(msg = str_c(" ERROR:  no relation found for '",obj.name,"' class"),queue = "NMI")
    if(is.null(mem.struct$CLASS[[obj.name]]$relation[[relation.name]])) 
      global.obj.writer$send(msg = str_c(" ERROR:  the relation ",relation.name," is not available for the class '",obj.name,"'"),queue = "NMI")

    strutt.ospitante <- mem.struct$CLASS[[obj.name]]$relation[[relation.name]]
    
    regex.sql <- "^[ ]*SQL[ ]*"
    
    if(sub(regex.sql,"\\1",strutt.ospitante$query) != strutt.ospitante$query) tipo.attributo <- "sql"
    
    if(tipo.attributo=="sql")   {
      first <- str_locate_all(string = strutt.ospitante$query,pattern = "\\{")[[1]][1]
      second <- str_locate_all(string = strutt.ospitante$query,pattern = "\\}")[[1]][1]
      query <- str_trim(str_sub(string = strutt.ospitante$query,start = first+1,end = second-1))
      q <- query
      # browser()
      if(!is.na(id))  { 
        q <- str_replace_all(string = q,pattern = "\\$primary.key\\$",as.character(id))
      }
      # browser()
      if(length(lst.argomenti)>0) { 
        for(i in seq(1,length(lst.argomenti))) {
          if(is.a.quoted.string(lst.argomenti[[i]]$value)==TRUE)
            q <- str_replace_all(string = q,pattern = str_c("\\$parameter_",i,"\\$"),togli.apici.esterni.stringa(lst.argomenti[[i]]$value))
          else
            q <- str_replace_all(string = q,pattern = str_c("\\$parameter_",i,"\\$"),lst.argomenti[[i]]$value)
        }
      }

      link.name <- mem.struct$CLASS[[obj.name]]$link.name
      
      # DEBUGGER -im
      if(global.debug.mode==TRUE ) {
        barra.t <- paste(c(" ",rep(" ",20)," "),collapse='')
        cat("\n",barra.t,q)
        # global.executed.statements <<- global.executed.statements + 1
      }
      # DEBUGGER -fm
      
      tmp.res <- mem.struct$SQLDB[[ link.name ]]$obj.connector$query(query = q)
      
      # Prepara il risultato
      if(dim(tmp.res)[1]==0) res <- c()
      else res <- tmp.res[,1]
      
      return(res)      
    }    
    global.obj.writer$send(msg = str_c(" ERROR:  at the moment the relations can be solved only with explicit SQL queries"),queue = "NMI")
  }
  # ----------------------------------------------------------------
  # RUN
  # ----------------------------------------------------------------
  run <-  function( script ) {
    print(script)
  }
  get <- function() {
    return(mem.struct)
  }
  # ----------------------------------------------------------------
  # Costruttore
  # ----------------------------------------------------------------
  costructor<-function( debug.mode ) {
    mem.struct <<- list()
    mem.struct$SQLDB <<- list()
    mem.struct$CLASS <<- list()
    global.DB.connectors <<- list()
    global.obj.writer <<- writer()
    global.debug.mode <<- debug.mode
  }
  costructor( debug.mode )
  # ----------------------------------------------------------------
  # RETURN di classe
  # ----------------------------------------------------------------
  return(
    list(
          "loadScript" =loadScript,
          "parseScript"=parseScript,
          "get"=get,
          "getEntityAttribute"=getEntityAttribute,
          "getEntityRelation" = getEntityRelation,
          "is.attribute.of"=is.attribute.of,
          "is.relation.of"=is.relation.of
        )
    )
}

