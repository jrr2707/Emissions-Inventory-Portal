library(RPostgres)
library(DBI)
library(rjson)
library(jsonlite)
library(R.utils)
library(magrittr)
library(dplyr)
library(readr)
library(readxl)
library(stringr)

my_secrets <- function() {
  path = "./secrets/secrets.json"
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  fromJSON(path)
}

secrets = my_secrets();

NAICScodes = "https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx";
NAICSdesc = "https://www.census.gov/naics/2017NAICS/2017_NAICS_Descriptions.xlsx";
EnergyEst = "https://data.nrel.gov/system/files/122/Updated%20county%20energy%20estimates.gzip";
download.file(NAICScodes, "downloads/NAICScodes.xlsx", mode = "wb");
download.file(NAICSdesc, "downloads/2017_NAICS_Descriptions.xlsx", mode = "wb");
download.file(EnergyEst, "downloads/EnergyEst.gzip", mode = "wb");

County_FIPS_codes <- read_delim("County FIPS codes.txt", 
                                "\t", escape_double = FALSE, col_names = FALSE, 
                                trim_ws = TRUE) %>% 
  transmute(COUNTY_FIPS = X1, County = X2, State = X3);

gunzip("downloads/EnergyEst.gzip", destname = gsub("[.]gz$", "", "downloads/EnergyEst.csv"), overwrite=TRUE);

X2017_NAICS_Descriptions <- read_excel("downloads/2017_NAICS_Descriptions.xlsx")
NAICS_Descriptions_2017 <- transmute(X2017_NAICS_Descriptions, 
                                     NAICS = suppressWarnings(as.integer(Code)), 
                                     NAICSname = Title, 
                                     NAICSdescr = Description,
                                     origin = "native")

Updated_county_energy_estimates <- read_csv("downloads/EnergyEst.csv",
                                            show_col_types = FALSE)

Updated_county_energy_estimates <- transmute(Updated_county_energy_estimates,
                                     FIPSTATE = FIPSTATE, 
                                     COUNTY_FIPS = COUNTY_FIPS,
                                     MECS_FT= MECS_FT,
                                     MMBTU_TOTAL = MMBTU_TOTAL,
                                     NAICS = suppressWarnings(as.integer(NAICS)), 
                                     YEAR = YEAR,
                                     STATE = STATE,
                                     IND_SECTOR = IND_SECTOR)

NYcountyEnergyEsts <- Updated_county_energy_estimates %>% 
  filter(STATE == "NEW YORK") %>%                       # Keep only the NEW YORK rows
  left_join(County_FIPS_codes, by = "COUNTY_FIPS") %>%  # Add county names
  left_join(NAICS_Descriptions_2017, by = "NAICS")      # Add NAICS code names and 
# descriptions

trimInt <- function(int, toDigits = 1) {
  if (toDigits <= 0) {
    toDigits <- 0
    warning("toDigits <= 0; result coerced to NA(s)\n")
  }
  chrInt <- str_extract(as.character(int), 
                        str_c("([-]){0,1}[[:digit:]]{0,", as.character(toDigits), "}"))
  as.integer(as.numeric(chrInt))
}


# Connect to a specific postgres database
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
