connectDatabase <- function(){
  
  pw <- {
    ""
  }
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "ScoalaDeSoferi",
                   host = "localhost", port = 5432,
                   user = "postgres", password = pw)
  con
}
