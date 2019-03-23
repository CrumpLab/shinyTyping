library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = "instanceTheory12"
))
###############3 This is name of theschema that holds your table
databaseName <- "test"
############### this will find the table of the database named "response"
# you need to CREATE table "response" ();


saveData <- function(data,table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields

  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  

  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function(value,table,field) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s WHERE %s ='%s'", table,field,value)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

checkLogin=function(username,password){
  
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  query <- sprintf("SELECT COUNT(*) FROM %s WHERE username ='%s' AND password='%s'", "userpw",username, password)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
  
}

register=function(username,password){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  query <- sprintf("SELECT COUNT(*) FROM %s WHERE username ='%s'", "userpw",username)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  if(as.character(data)=="0")
    {query =  sprintf("INSERT INTO %s (%s) VALUES ('%s','%s')", "userpw",
                     "username,password",
                     username,password)
     dbGetQuery(db, query)
    } 
  dbDisconnect(db)
  data
  
}
#returns the serial id of the most recent session, to label the session_id fields of all the
#characters typed in the session
loadSessionId <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT MAX(id) FROM test.sessions")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

