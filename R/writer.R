#' a writer class
#'
#' @description  A  writer for debug, log writing, error handling, etc...
#' @import stringr
#' @export
writer <- function() {
  
  global.behaviourTable<-c()
  global.stack <- list()
  
  #=================================================================================
  # send
  # Send a Message Log according to the object policies for the type 
  # indicated in 'queue'
  #=================================================================================
  send<-function( msg , queue="MSG" ) {
    if(length(msg)>1) msg<-paste(msg,collapse='')
    else msg<-msg 
    
    what2Do<-global.behaviourTable[which(global.behaviourTable[,"queue"]==queue),"behaviour"]
    what2Do <- unlist(str_split(string = what2Do,pattern = ","))
    
    # Fai cio' che devi: occhio all'ordine (e' importante)
    if( "print" %in% what2Do )  {
      cat(msg)
    }
    if( "store" %in% what2Do )  {
      if( !(as.character(msg) %in% names(global.stack))) global.stack[[as.character(msg)]] <- c()
      global.stack[[as.character(msg)]] <<- rbind(global.stack[[as.character(msg)]],c("msg"=msg))
    }    
    if( "stop" %in% what2Do )  {
      if( "print" %in% what2Do ) cat("\n")
      stopQuietly()
    }    
  }
  #=================================================================================
  # setBehaviour
  # change a single line in the behaviour table
  #=================================================================================
  setBehaviour<-function( queue , behaviour, file , screen) {
    if(queue %in% global.behaviourTable[,"queue"]) { 
      global.behaviourTable[which(global.behaviourTable[,"queue"]==queue),"behaviour"] <<- behaviour
      global.behaviourTable[which(global.behaviourTable[,"queue"]==queue),"file"] <<- file
      global.behaviourTable[which(global.behaviourTable[,"queue"]==queue),"screen"] <<- screen
    }
    else {
      global.behaviourTable<<-rbind(global.behaviourTable,c(queue,behaviour,file,screen))
    }
  }
  #=================================================================================
  # getBehaviour
  #=================================================================================
  getBehaviour<-function() {
    return(global.behaviourTable)
  }  
  #=================================================================================
  # getStack
  #=================================================================================
  getStack<-function() {
    return(global.stack)
  }    
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function() {
    # Costruisci la tabella dei comportamento
    bht<-c()
    bht<-rbind(bht,c("DEBUG","ignore","","T"))
    bht<-rbind(bht,c("SQL","print","","T"))
    bht<-rbind(bht,c("WRN","print","","T"))
    bht<-rbind(bht,c("MSG","print,store","","T"))
    bht<-rbind(bht,c("ERR","print","","T"))
    bht<-rbind(bht,c("NMI","print,stop","","T"))
    bht<-rbind(bht,c("LOG","print","./pMineR.default.log","T"))
    colnames(bht)<-c("queue","behaviour","file", "screen")
    global.behaviourTable <<- bht
    
    global.stack <<- list()
  }
  #=================================================================================
  costructor();
  #=================================================================================
  return(
    list(
      "send"=send,
      "setBehaviour"=setBehaviour,
      "getBehaviour"=getBehaviour,
      "getStack"=getStack
    )
  )
}
