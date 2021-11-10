library(RPostgres)
library(DBI)
library(rjson)
library(jsonlite)

my_secrets <- function() {
  path = "./secrets/secrets.json"
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  fromJSON(path)
}

secrets = my_secrets();

# Connect to a specific postgres database i.e. Heroku
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = 'oasps.student.rit.edu', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = toString(secrets["username"]),
                 password = toString(secrets["password"]))

dbListTables(con)

gfl_1dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_1dig.tsv", sep = "\t", header=TRUE)
gfl_2dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_2dig.tsv", sep = "\t", header=TRUE)
gfl_3dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_3dig.tsv", sep = "\t", header=TRUE)
gfl_4dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_4dig.tsv", sep = "\t", header=TRUE)




res <- dbSendQuery(con, "DROP TABLE IF EXISTS gf1_1dig")
res <- dbSendQuery(con, "DROP TABLE IF EXISTS gf1_2dig")
res <- dbSendQuery(con, "DROP TABLE IF EXISTS gf1_3dig")
res <- dbSendQuery(con, "DROP TABLE IF EXISTS gf1_4dig")

dbWriteTable(con, "gf1_1dig", gfl_1dig, row.names=TRUE, append=FALSE)
dbWriteTable(con, "gf1_2dig", gfl_2dig, row.names=TRUE, append=FALSE)
dbWriteTable(con, "gf1_3dig", gfl_3dig, row.names=TRUE, append=FALSE)
dbWriteTable(con, "gf1_4dig", gfl_4dig, row.names=TRUE, append=FALSE)


dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
