#' @title connect to sql server to pull data
#' @description uses an ini file to connect to sql server
#' @param data_source_name the name of the data source in the ini file
#' @param config_file_path the path to the ini file
#' @return db_conn the connection to sql server
#' @import RODBC
#' @export
#' @examples
#' pull_conn <- sql_server_pull_connection(data_source_name, config_file_path)
sql_server_pull_connection <- function(data_source_name, config_file_path) {

  # load ini file
  my_ini <- read_ini_file(config_file_path)

  # get params
  driver <- paste(my_ini[[data_source_name]][c("driver")], collapse='')
  server <- paste(my_ini[[data_source_name]][c("server")], collapse='')
  database <- paste(my_ini[[data_source_name]][c("database")], collapse='')
  trusted_connection <- paste(my_ini[[data_source_name]][c("trusted_connection")], collapse='')
  uid <- paste(my_ini[[data_source_name]][c("uid")], collapse='')
  pwd <- paste(my_ini[[data_source_name]][c("pwd")], collapse='')
  port <- paste(my_ini[[data_source_name]][c("port")], collapse='')

  # generate connection string
  if (trusted_connection == "yes") {
    conn_str <- paste("DRIVER=","{",driver,"}",";Server=",server,";Database=",database,";trusted_connection=",trusted_connection, sep="")
  } else {
    conn_str <- paste("DRIVER=",driver,";Server=",server,";Database=",database,";UID=",uid,";PWD=",pwd, sep="")
  }

  # create connection
  db_conn <- odbcDriverConnect(conn_str)

  # return connection
  return(db_conn)

}

#' @title pull data from sql server
#' @description pulls data from sql server
#' @param pull_connection pull connection
#' @param sql_statement sql statement
#' @return df dataframe
#' @import RODBC
#' @export
#' @examples
#' df <- pull_data_from_sql_server(pull_conn, sql_statement)
pull_data_from_sql_server <- function(pull_connection, sql_statement) {

  df <- sqlQuery(pull_connection, sql_statement)

  return(df)

}

#' @title connect to sql server to push data
#' @description uses an ini file to connect to sql server
#' @param data_source_name the name of the data source in the ini file
#' @param config_file_path the path to the ini file
#' @return db_conn the connection to sql server
#' @import odbc
#' @export
#' @examples
#' push_conn <- sql_server_push_connection(data_source_name, config_file_path)
sql_server_push_connection <- function(data_source_name, config_file_path) {

  # load ini file
  my_ini <- read_ini_file(config_file_path)

  # get params
  driver <- paste(my_ini[[data_source_name]][c("driver")], collapse='')
  server <- paste(my_ini[[data_source_name]][c("server")], collapse='')
  database <- paste(my_ini[[data_source_name]][c("database")], collapse='')
  trusted_connection <- paste(my_ini[[data_source_name]][c("trusted_connection")], collapse='')
  uid <- paste(my_ini[[data_source_name]][c("uid")], collapse='')
  pwd <- paste(my_ini[[data_source_name]][c("pwd")], collapse='')
  port <- paste(my_ini[[data_source_name]][c("port")], collapse='')

  # create connection
  con <- dbConnect(odbc(),
                   Driver = driver,
                   Server = server,
                   Database = database,
                   UID = uid,
                   PWD = pwd,
                   Port = port)

  # return connection
  return(con)

}

#' @title execute list of sql statements
#' @description executes list of sql statements
#' @param push_connection the connection
#' @param list_of_sql_statements the list
#' @return nothing
#' @import odbc
#' @export
#' @examples
#' execute_sql_list(my_conn, my_list)
execute_sql_list <- function(push_connection, list_of_sql_statements) {
  for (i in list_of_sql_statements) {
    rs <- dbSendQuery(push_connection,i)
    dbClearResult(rs)
  }
}

#' @title execute sql file
#' @description executes sql file where each block of code to be executed ends with ";"
#' @param push_connection the connection
#' @param sql_file_path the list
#' @return nothing
#' @export
#' @examples
#' execute_sql_file(my_conn, my_file)
execute_sql_file <- function(push_connection, sql_file_path) {
  my_list <- read_text_file_as_list(sql_file_path, ";")
  rs <- execute_sql_list(push_connection, my_list)
}

#' @title push sql to sql server
#' @description pushes sql statement to sql server
#' @param push_connection the connection
#' @param sql_statement the sql
#' @return nothing
#' @import odbc
#' @export
#' @examples
#' push_sql_statement(my_conn, my_sql)
push_sql_statement <- function(push_connection, sql_statement) {
  rs <- dbSendQuery(push_connection, sql_statement)
  dbClearResult(rs)
}

#' @title push data to sql server
#' @description pushes data to sql server
#' @param push_connection, the connection
#' @param table_name the table
#' @param dataframe the dataframe
#' @param append_or_overwrite either "append" or "overwrite"
#' @return nothing
#' @import odbc
#' @export
#' @examples
#' push_data_to_sql_server(my_conn, table_name, dataframe, append_or_overwrite)
push_data_to_sql_server <- function(push_connection, table_name, dataframe, append_or_overwrite) {

    if (append_or_overwrite == "append") {
      dbWriteTable(push_connection, table_name, dataframe, append=TRUE)
    }

  if (append_or_overwrite == "overwrite") {
    dbWriteTable(push_connection, table_name, dataframe, overwrite=TRUE)
  }

}
