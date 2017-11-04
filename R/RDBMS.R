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
RDBMS <- function( RDBMS.type = "mysql", user="root", password="", host="127.0.0.1", dbname ) {
  param.RDBMS.type<-""
  param.user<-""
  param.password<-""
  param.host<-""
  param.dbname<-""
  # db.connection<-''

  query <- function( query ) {
    db.connection <- dbConnect(MySQL(), user=param.user, password=param.password, dbname=dbname, host=param.host)
    rs = dbSendQuery(db.connection, query )
    data.q = fetch(rs, n=-1)
    dbClearResult(rs)
    dbDisconnect(db.connection)
    return(data.q)
  }
  constructor <- function(RDBMS.type, user, password, host, dbname) {
    param.RDBMS.type<<-RDBMS.type
    param.user<<-user
    param.password<<-password
    param.host<<-host
    param.dbname<<-dbname
    # db.connection <<- dbConnect(MySQL(), user='root', password='', dbname='tree_v2_r1', host='127.0.0.1')
  }
  constructor(RDBMS.type=RDBMS.type, user=user, password=password, host=host, dbname = dbname)
  return(list(
    "query"=query
  ))
}