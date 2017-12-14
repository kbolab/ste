#' a writer class
#'
#' @description  A  writer for debug, log writing, error handling, etc...
#' @import stringr
#' @export
writer <- function() {
  global.behaviourTable<-c()
  #=================================================================================
  # sendLog
  # Send a Message Log according to the object policies for the type 
  # indicated in 'type'
  #=================================================================================
  sendLog<-function( msg , type="MSG" ) {
    if(length(msg)>1) msg<-paste(msg,collapse='')
    else msg<-msg 
    
    printPrefix <- global.behaviourTable[which(global.behaviourTable[,"msg"]==type),"printPrefix"]
    if(printPrefix=="T") msg <- paste( c("\n",type,": ",msg),collapse = '')
    else 
    msg <- msg
    
    what2Do<-global.behaviourTable[which(global.behaviourTable[,"msg"]==type),"behaviour"]
    
    if(what2Do == "print")  {
      cat(msg)
    }
    if(what2Do == "stop")  {
      cat(msg)
      stop();
    }    
  }
  #=================================================================================
  # setBehaviour
  # change a single line in the behaviour table
  #=================================================================================
  setBehaviour<-function( msg , behaviour) {
    global.behaviourTable[which(global.behaviourTable[,"msg"]==msg),"behaviour"] <<- global.behaviour
  }
  #=================================================================================
  # getBehaviour
  #=================================================================================
  getBehaviour<-function() {
    return(global.behaviourTable)
  }  
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function() {
    # Costruisci la tabella dei comportamento
    bht<-c()
    bht<-rbind(bht,c("WRN","print","T",""))
    bht<-rbind(bht,c("MSG","print","F",""))
    bht<-rbind(bht,c("ERR","print","T",""))
    bht<-rbind(bht,c("NMI","stop","T",""))
    bht<-rbind(bht,c("LOG","print","T","./pMineR.default.log"))
    colnames(bht)<-c("msg","behaviour","printPrefix","file")
    global.behaviourTable <<- bht
  }
  #=================================================================================
  costructor();
  #=================================================================================
  return(
    list(
      "sendLog"=sendLog,
      "setBehaviour"=setBehaviour,
      "getBehaviour"=getBehaviour
    )
  )
}
