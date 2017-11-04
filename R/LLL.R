#' Low Level Language Class
#'
#' @description  The engine class to parse and execute LLL scripts
#' @import stringr
#' @export
LLL <- function() {

  global.DB.connectors<-list()

  mem.struct <- list()
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
        cat("\n ",res$err.msg)
        stop()
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
          cat("\n ",str.cmd$err.msg)
          stop()
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
        mem.struct[[ str.contesto ]][[ str.nome ]]$obj.connector <<- RDBMS(user = user, password = pwd,host = host, dbname = database  )
      }
      if(str.contesto == "RELATION") {
        master.name <- unlist(str_split(strutt.ospitante$master,"\\."))[1]
        master.field <- unlist(str_split(strutt.ospitante$master,"\\."))[2]
        slave.name <- unlist(str_split(strutt.ospitante$slave,"\\."))[1]
        slave.field <- unlist(str_split(strutt.ospitante$slave,"\\."))[2]
        asc.order.by <- strutt.ospitante[[ "asc order by" ]]
        desc.order.by <- strutt.ospitante[[ "desc order by" ]]
        # if(is.null(mem.struct$CLASS[[ master.name ]])) mem.struct$CLASS[[ master.name ]]<<- list()
        if(is.null(mem.struct$CLASS[[ master.name ]]$relation)) mem.struct$CLASS[[ master.name ]]$relation <<- list()
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]<<-list()
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]$master.name <<- master.name
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]$master.field <<- master.field
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]$slave.name <<- slave.name
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]]$slave.field <<- slave.field
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]][[ "asc order by" ]] <<- asc.order.by
        mem.struct$CLASS[[ master.name ]]$relation[[ str.nome ]][[ "desc order by" ]] <<- desc.order.by
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
    if(sum(is.na(res.1)) > 0  ) stop("errore di sintassi")
    if(sum(is.na(res.2)) > 0  ) stop("errore di sintassi")
    str.contesto <- str_sub(string = stringa, start =res.2[2]+1, end = res.1[2])
    str.nome <- str_replace_all(
      string = str_sub(string = stringa, start =res.1[2]+1, end = str_length(stringa)),
      pattern = " ",replacement = "")

    if( !(str.contesto %in% c("SQLDB","CLASS","RELATION"))){
      return(list("str.contesto"="","str.nome"="", "error"=TRUE,"err.msg"=paste(c("Syntax error: ',",str.contesto,",' is not a valid context"),collapse = '')  ))
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
                                  "password"=list("synt.type"=1)
                                  )
    }
    if(contesto=="CLASS"){
      lst.possibili.comandi<-list("link.name"=list("synt.type"=1),
                                  "table.name"=list("synt.type"=1),
                                  "primary.key"=list("synt.type"=1),
                                  "attribute"=list("synt.type"=2)
                                  )
    }

    if(contesto=="RELATION"){
      lst.possibili.comandi<-list("master"=list("synt.type"=1),
                                  "slave"=list("synt.type"=1),
                                  "asc order by"=list("synt.type"=1),
                                  "desc order by"=list("synt.type"=1)
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
          # trovato. ritorna
          return(list("comando"=list("w1"=comando,"w2"=secondo.membro) , "error"=FALSE))
        }
      }
      # Parsing della sintassi di tipo 2
      if( lst.possibili.comandi[[comando]]$synt.type == 2 ) {
        # estrai la prima parte (il nome del comando)
        res.1<- str_locate(string = stringa,pattern = paste( c("^[ ]*",comando,"[ ]+") ,collapse = '') )
        # se non torna, dai errore di sintassi
        if(!(sum(is.na(res.1)) == 0  ) ) {
          return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("Syntax error in: ",stringa,""),collapse = '')  ))
        }
        # ora estrai la seconda parte del nome
        new.stringa <- str_sub(string = stringa,start = res.1[,"end"], end = str_length(stringa))
        res.2<- str_locate(string = new.stringa,pattern = paste( c("^[A-Za-z0-9_. ]*=") ,collapse = '') )
        if(!(sum(is.na(res.2)) == 0  ) ) {
          return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("Syntax error in: ",stringa,""),collapse = '')  ))
        }
        nome.primo.elemento <- str_trim(str_sub(string = new.stringa,start = res.2[ ,"start"],end = res.2[ ,"end"]-1))
        nome.secondo.elemento <- str_trim(str_sub(string = new.stringa,start = res.2[ ,"end"]+1,end = str_length(new.stringa)))
        # devo gestire il ritorno in questo cso. Conviene modificare anche il cso precednete facendo sì
        # che ritorni una lista, anzichè solo un "comndo"=comando
        return(list("comando"=list("w1"=comando,"w2"=nome.primo.elemento,"w3"=nome.secondo.elemento) , "error"=FALSE))
      }
    }
    # errore di sintassi
    return(list("comando"=list() , "error"=TRUE, "err.msg"=paste(c("Syntax error in: ",stringa,""),collapse = '')  ))
  }
  is.attribute.of<-function( className , attrName ) {
    if( attrName %in% names(mem.struct$CLASS[[className]]$attribute) ) return(TRUE)
    else return(FALSE)
  }
  is.relation.of<-function( className , relName ) {
    # if(relName=="dimensioneTiroide") browser()
    if( relName %in% names(mem.struct$CLASS[[className]]$relation) ) return(TRUE)
    else return(FALSE)
  }
  getEntityAttribute<-  function( obj.name , id, attr.name  ) {
    regex.sql <- "^[ ]*SQL[ ]*"
    # qualche sano controllo formale
    if(is.null(mem.struct$CLASS[[obj.name]])) stop(" missing ENTITY! (err: 8yfd87s)")
    if(is.null(mem.struct$CLASS[[obj.name]]$attribute[[attr.name]])) stop(" missing ATTRIBUTE! (err: 84yfd87s)")
    strutt <- mem.struct$CLASS[[obj.name]]
    # popola le variabili per fare la query
    campoTabella <- strutt$attribute[[attr.name]]
    nomeTabella <- strutt$table.name
    primary.key <- strutt$primary.key
    # table.field <- strutt$attribute$nome
    table.field <- strutt$attribute[[attr.name]]
    link.name <- strutt$link.name
    # cat("\n ",table.field)
    # browser()
    
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
      q <- str_c("select ",table.field," as res from ",nomeTabella," where ",primary.key," = '",id,"';")
    }
    # browser()
    # lancia la query
    tmp.res <- mem.struct$SQLDB[[ link.name ]]$obj.connector$query(query = q)

    # Prepara il risultato
    if(dim(tmp.res)[1]==0) res <- c()
    else res <- tmp.res[,1]
    return(res)
  }
  getEntityRelation<-function( obj.name , id , relation.name) {

    if(is.null(mem.struct$CLASS[[obj.name]])) stop(" missing ENTITY! (err: 8yfd87s)")
    if(is.null(mem.struct$CLASS[[obj.name]]$relation)) stop(" there are no relations for that class")
    if(is.null(mem.struct$CLASS[[obj.name]]$relation[[relation.name]])) stop(" there aren't such relation for the indicated class)")

    # estrai i dati di interesse, giusto un nickname
    blocco <- mem.struct$CLASS[[obj.name]]$relation[[relation.name]]

    # estrai i singoli campi per costruire la query
    master.table <- mem.struct$CLASS[[blocco$master.name]]$table.name
    slave.table <- mem.struct$CLASS[[blocco$slave.name]]$table.name
    master.pk <- mem.struct$CLASS[[blocco$master.name]]$primary.key
    master.link<-mem.struct$CLASS[[blocco$master.name]]$attribute[[blocco$master.field]]
    slave.link<-mem.struct$CLASS[[blocco$slave.name]]$attribute[[blocco$slave.field]]
    slave.pk <- mem.struct$CLASS[[blocco$slave.name]]$primary.key
    link.name <- mem.struct$CLASS[[obj.name]]$link.name
    
    order.by <- ""
    if("asc order by" %in% names(mem.struct$CLASS[[obj.name]]$relation[[relation.name]])) {
      nome.campo.ordinamento <- mem.struct$CLASS[[blocco$slave.name]]$attribute[[mem.struct$CLASS[[obj.name]]$relation[[relation.name]][["asc order by"]]]]
      order.by <- str_c("order by ",nome.campo.ordinamento," asc")
    }
    if("desc order by" %in% names(mem.struct$CLASS[[obj.name]]$relation[[relation.name]])) {
      nome.campo.ordinamento <- mem.struct$CLASS[[blocco$slave.name]]$attribute[[mem.struct$CLASS[[obj.name]]$relation[[relation.name]][["desc order by"]]]]
      order.by <- str_c("order by ",nome.campo.ordinamento," desc")
    }

    # Esegui la query
    q <- str_c("select ",slave.table,".",slave.pk," as res from ",master.table,", ",slave.table," where ",master.table,".",master.link," = ",slave.table,".",slave.link," and ",master.table,".",master.pk," = '",id,"'"," ",order.by," ;")
    tmp.res <- mem.struct$SQLDB[[ link.name ]]$obj.connector$query(query = q)

    # Prepara il risultato
    if(dim(tmp.res)[1]==0) res <- c()
    else res <- tmp.res[,"res"]
    return(res)
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
  costructor<-  function( ) {
    mem.struct<<-list()
    mem.struct$SQLDB<<-list()
    mem.struct$CLASS<<-list()
    # mem.struct$RELATION<<-list()
    global.DB.connectors<<-list()
  }
  costructor()
  # ----------------------------------------------------------------
  # RETURN di classe
  # ----------------------------------------------------------------
  return(
    list(
          "parseScript"=parseScript,
          "get"=get,
          "getEntityAttribute"=getEntityAttribute,
          "getEntityRelation" = getEntityRelation,
          "is.attribute.of"=is.attribute.of,
          "is.relation.of"=is.relation.of
        )
    )
}
