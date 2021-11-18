library(RPostgres)
library(DBI)
library(rjson)
library(jsonlite)
library(R.utils)
library(magrittr)
library(tidyr)
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

n_int_digits = function(x) {
  result = floor(log10(abs(x)))
  result[!is.finite(result)] = 0
  result
}

trimInt <- function(int, toDigits = 1) {
  if (toDigits <= 0) {
    toDigits <- 0
    warning("toDigits <= 0; result coerced to NA(s)\n")
  }
  chrInt <- str_extract(as.character(int), 
                        str_c("([-]){0,1}[[:digit:]]{0,", as.character(toDigits), "}"))
  as.integer(as.numeric(chrInt))
}


#################
#START GIVEN CODE
#################

###
# Start synthisise NAICS
###
NAICS_Addrows1dig <- 
  tibble(
    NAICS = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    NAICSname = c(
      "Agriculture",   "Mining, Utilities, and Construction",
      "Manufacturing", "Trade, Transportation, and Warehousing",
      "General services: Information, Financial, Real Estate, Professional, Management, Waste Management",
      "Education and Training; Health Care and Social Assistance",
      "Arts, Entertainment, and Recreation; Accommodation and Food Services", 
      "Other Services", 
      "Public Administration and Government"
    ),
    NAICSdescr = c(
      "The Agriculture, Forestry, Fishing and Hunting sector comprises establishments primarily engaged in growing crops, raising animals, harvesting timber, and harvesting fish and other animals from a farm, ranch, or their natural habitats.\r\n\r\nThe establishments in this sector are often described as farms, ranches, dairies, greenhouses, nurseries, orchards, or hatcheries.  A farm may consist of a single tract of land or a number of separate tracts which may be held under different tenures.  For example, one tract may be owned by the farm operator and another rented.  It may be operated by the operator alone or with the assistance of members of the household or hired employees, or it may be operated by a partnership, corporation, or other type of organization. When a landowner has one or more tenants, renters, croppers, or managers, the land operated by each is considered a farm.",
      "The Mining, Quarrying, and Oil and Gas Extraction sector comprises establishments that extract naturally occurring mineral solids, such as coal and ores; liquid minerals, such as crude petroleum; and gases, such as natural gas.  The term mining is used in the broad sense to include quarrying, well operations, beneficiating (e.g., crushing, screening, washing, and flotation), and other preparation customarily performed at the mine site, or as a part of mining activity.\r\n\r\nThe Utilities sector comprises establishments engaged in the provision of the following utility services: electric power, natural gas, steam supply, water supply, and sewage removal.  Within this sector, the specific activities associated with the utility services provided vary by utility: electric power includes generation, transmission, and distribution; natural gas includes distribution; steam supply includes provision and/or distribution; water supply includes treatment and distribution; and sewage removal includes collection, treatment, and disposal of waste through sewer systems and sewage treatment facilities.\r\n\r\nThe Construction sector comprises establishments primarily engaged in the construction of buildings or engineering projects (e.g., highways and utility systems).  Establishments primarily engaged in the preparation of sites for new construction and establishments primarily engaged in subdividing land for sale as building sites also are included in this sector.  Construction work done may include new work, additions, alterations, or maintenance and repairs.  Activities of these establishments generally are managed at a fixed place of business, but they usually perform construction activities at multiple project sites.  Production responsibilities for establishments in this sector are usually specified in (1) contracts with the owners of construction projects (prime contracts) or (2) contracts with other construction establishments (subcontracts).",
      "The Manufacturing sector comprises establishments engaged in the mechanical, physical, or chemical transformation of materials, substances, or components into new products.  The assembling of component parts of manufactured products is considered manufacturing, except in cases where the activity is appropriately classified in Sector 23, Construction.\r\n\r\nEstablishments in the Manufacturing sector are often described as plants, factories, or mills and characteristically use power-driven machines and material handling equipment.  However, establishments that transform materials or substances into new products by hand or in the worker's home and those engaged in selling to the general public products made on the same premises from which they are sold, such as bakeries, candy stores, and custom tailors, may also be included in this sector.  The materials, substances, or components transformed by manufacturing establishments are raw materials that are products of agriculture, forestry, fishing, mining, or quarrying as well as products of other manufacturing establishments.  The new product of a manufacturing establishment may be finished in the sense that it is ready for utilization or consumption, or it may be semi-finished to become an input for an establishment engaged in further manufacturing.\r\n\r\nThe subsectors in the Manufacturing sector generally reflect distinct production processes related to material inputs, production equipment, and employee skills. In the machinery area, where assembling is a key activity, parts and accessories for manufactured products are classified in the industry of the finished manufactured item when they are made for separate sale.  For example, a replacement refrigerator door would be classified with refrigerators and an attachment for a piece of metalworking machinery would be classified with metalworking machinery.  However, components, input from other manufacturing establishments, are classified based on the production function of the component manufacturer.", 
      "The Retail Trade sector comprises establishments engaged in retailing merchandise, generally without transformation, and rendering services incidental to the sale of merchandise.  The retailing process is the final step in the distribution of merchandise; retailers are, therefore, organized to sell merchandise in small quantities to the general public.\r\n\r\n
The Wholesale Trade sector comprises establishments engaged in wholesaling merchandise, generally without transformation, and rendering services incidental to the sale of merchandise.  The merchandise described in this sector includes the outputs of agriculture, mining, manufacturing, and certain information industries, such as publishing.\r\n\r\nThe Transportation and Warehousing sector includes industries providing transportation of passengers and cargo, warehousing and storage for goods, scenic and sightseeing transportation, and support activities related to modes of transportation.  Establishments in these industries use transportation equipment or transportation related facilities as a productive asset.  The type of equipment depends on the mode of transportation.  The modes of transportation are air, rail, water, road, and pipeline.\r\n\r\nThe Transportation and Warehousing sector distinguishes three basic types of activities: subsectors for each mode of transportation, a subsector for warehousing and storage, and a subsector for establishments providing support activities for transportation.  In addition, there are subsectors for establishments that provide passenger transportation for scenic and sightseeing purposes, postal services, and courier services.",
      "General services: Information, Financial, Real Estate, Professional, Management, Waste Management",
      "Education and Training; Health Care and Social Assistance",
      "Arts, Entertainment, and Recreation; Accommodation and Food Services", 
      "Other Services", 
      "Public Administration and Government"),
    origin = "synthesized"
  )

# Synthesize 2-digit codes for Manufacturing (31..33); Retail Trade (44, 45), and 
# Transportation (48); and for Postal, courier, and express delivery; Warehousing and 
# Storage (49)
NAICS_Addrows2dig <- 
  tibble(NAICS = c(31, 32, 33, 44, 45, 48, 49), 
         NAICSname = c(
           "Manufacturing (Food, Beverages, Tobacco, Textiles, Apparel, Leather)",   #31
           str_c("Manufacturing (Wood, Paper, Printing, Petroleum, Coal products, ", #32
                 "Chemicals, Plastic and Rubber products, Minerals)"),
           str_c("Manufacturing (Metals, Machinery, Electronics, Transportation, ",  #33
                 "Furniture, Miscellaneous"),
           str_c("Retail Trade (Consumer Goods, Food and Beverages, Health and ",    #44
                 "Personal Care, Home and Garden, Clothing, Luxury; Filling Stations)"),
           str_c("Retail Trade (Leisure Goods, Publications, General Merchandise, ", #45
                 "Miscellaneous Retailers, Nonstore Retailers)"),
           "General Transportation (Passenger and Freight)",                         #48
           str_c("Postal, Courier, and Express Delivery; Storage")                   #49
         ),
         NAICSdescr = c(
           "Manufacturing (Food, Beverages, Tobacco, Textiles, Apparel, Leather)",   #31
           str_c("Manufacturing (Wood, Paper, Printing, Petroleum, Coal products, ", #32 
                 "Chemicals, Plastic and Rubber products, Minerals)"),
           str_c("Manufacturing (Metals, Machinery, Electronics, Transportation, ",  #33
                 "Furniture, Miscellaneous"),
           str_c("Retail Trade (Consumer Goods, Food and Beverages, Health and ",    #44
                 "Personal Care, Home and Garden, Clothing, Luxury; Filling Stations)"),
           str_c("Retail Trade (Leisure Goods, Publications, General Merchandise, ", #45
                 "Miscellaneous Retailers, Nonstore Retailers)"),
           "General Transportation (Passenger and Freight)",                         #48
           str_c("Postal, Courier, and Express Delivery; Warehousing and Storage")), #49
         origin = "synthesized")

NAICS_Addrows3dig <- 
  tibble(NAICS = 119, 
         NAICSname = "Agriculture (unclassified)", 
         NAICSdescr = "Agriculture (unclassified)",
         origin = "synthesized")

NAICS_Addrows4dig <- 
  tibble(NAICS = c(1191, 2369, 2378, 2388),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")

NAICS_Addrows5dig <- 
  tibble(NAICS = c(11910, 23690, 23780, 23880),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")

NAICS_Addrows6dig <- 
  tibble(NAICS = c(119100, 236900, 237800, 238800),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")

###
# end synthisise NAICS
###

###
#Make sliced NCAIS
###
NAICS_Descriptions_2017 <- NAICS_Descriptions_2017 %>% 
  filter(!is.na(NAICS)) %>% 
  bind_rows(NAICS_Addrows1dig) %>%
  bind_rows(NAICS_Addrows2dig) %>%
  bind_rows(NAICS_Addrows3dig) %>%
  bind_rows(NAICS_Addrows4dig) %>% 
  arrange(as.character(NAICS))

# Add columns with NAICS code values trimmed to max 1, 2, or 3 digits
NAICS_Descriptions_2017 <- NAICS_Descriptions_2017 %>% 
  mutate(NAICS1dig = trimInt(NAICS, 1)) %>%  
  mutate(NAICS2dig = trimInt(NAICS, 2)) %>% 
  mutate(NAICS3dig = trimInt(NAICS, 3)) %>% 
  mutate(NAICS4dig = trimInt(NAICS, 4)) 

# Make a table of the one-digit abbreviated NAICS codes
NAICS_Descriptions1dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS1dig) %>%
  transmute(NAICS1dig = NAICS1dig, NAICSname1dig = NAICSname, 
            NAICSdescr1dig = NAICSdescr) %>%
  arrange(NAICS1dig) %>%
  distinct()

# Make a table of the two-digit abbreviated NAICS codes
NAICS_Descriptions2dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS2dig) %>%
  filter(str_length(as.character(NAICS)) == 2) %>%
  transmute(NAICS2dig = NAICS2dig, NAICSname2dig = NAICSname, 
            NAICSdescr2dig = NAICSdescr) %>%
  arrange(NAICS2dig) %>%
  distinct()

# Make an analogous table of 3-digit abbreviated NAICS codes
NAICS_Descriptions3dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS3dig) %>%
  filter(str_length(as.character(NAICS)) == 3) %>%
  transmute(NAICS3dig = NAICS3dig, 
            NAICSname3dig = NAICSname, 
            NAICSdescr3dig = NAICSdescr) %>%
  arrange(NAICS3dig) %>%
  distinct()

# Make an analogous table of 4-digit abbreviated NAICS codes
NAICS_Descriptions4dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS4dig) %>%
  filter(str_length(as.character(NAICS)) == 4) %>%
  transmute(NAICS4dig = NAICS4dig, 
            NAICSname4dig = NAICSname, 
            NAICSdescr4dig = NAICSdescr) %>%
  arrange(NAICS4dig) %>%
  distinct()


###
#end make sliced
###

# A sorted vector of fuel names will be useful. 
fuelNames <- c("Natural_gas", "Diesel", "Net_electricity", "LPG_NGL", 
               "Residual_fuel_oil", "Coal", "Coke_and_breeze", "Other")

# Add the 1-, 2-, 3-, and 4-digit abbreviated NAICS codes, and the corresponding 
# names and descriptions
NYcountyEnergyEsts <- NYcountyEnergyEsts %>%
  mutate(NAICS1dig = trimInt(NAICS, 1)) %>%
  mutate(NAICS2dig = trimInt(NAICS, 2)) %>%
  mutate(NAICS3dig = trimInt(NAICS, 3)) %>%
  mutate(NAICS4dig = trimInt(NAICS, 4)) 
NYcountyEnergyEsts <- NYcountyEnergyEsts %>% 
  left_join(NAICS_Descriptions1dig, by = "NAICS1dig") %>%
  left_join(NAICS_Descriptions2dig, by = "NAICS2dig") %>%
  left_join(NAICS_Descriptions3dig, by = "NAICS3dig") %>%
  left_join(NAICS_Descriptions4dig, by = "NAICS4dig") 

# We keep the abbreviated NAICS codes in the summary dataframes because 
# other uses of the data might want them. We don't carry the NAICS 
# descriptions around because they're extremely bulky. 

# Aggregate the energy use for each 1-digit abbreviated NAICS code
NYcountyEnergyPerFuelYear1dig <- NYcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS1dig, NAICSname1dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
NYcountyEnergyPerFuelYear_1dig <-pivot_wider(
  NYEnergyPerFuelYear1dig, 
  names_from = MECS_FT, 
  values_from = MMBTU)

# Aggregate the energy use for each 2-digit abbreviated NAICS code
NYcountyEnergySumm2dig <- NYcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS2dig, NAICSname2dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
NYcountyEnergyPerFuelYear_2dig <- pivot_wider(NYcountyEnergySumm2dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 3-digit abbreviated NAICS code
NYcountyEnergySumm3dig <- NYcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS3dig, NAICSname3dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
NYcountyEnergyPerFuelYear_3dig <- pivot_wider(NYcountyEnergySumm3dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 4-digit abbreviated NAICS code
NYcountyEnergySumm4dig <- NYcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS4dig, NAICSname4dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
NYcountyEnergyPerFuelYear_4dig <- pivot_wider(NYcountyEnergySumm4dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# The pivot_wider()s created NAs in the columns for individual fuels, so let's clean 
# them up by nonchalantly replacing them with 0s as before. 
for (fuel in fuelNames) {
  NYcountyEnergyPerFuelYear_1dig[[fuel]][
    which(is.na(NYcountyEnergyPerFuelYear_1dig[[fuel]]))] <- 0
  NYcountyEnergyPerFuelYear_2dig[[fuel]][
    which(is.na(NYcountyEnergyPerFuelYear_2dig[[fuel]]))] <- 0
  NYcountyEnergyPerFuelYear_3dig[[fuel]][
    which(is.na(NYcountyEnergyPerFuelYear_3dig[[fuel]]))] <- 0
  NYcountyEnergyPerFuelYear_4dig[[fuel]][
    which(is.na(NYcountyEnergyPerFuelYear_4dig[[fuel]]))] <- 0
}

# These full and summary datasets are useful to look at with other tools, so save them now. 
# write_tsv(GFLcountyEnergyEsts, "GFLcountyEnergyEsts.tsv.gz")    # It's huge, so compress it. 
write_tsv(NYcountyEnergyPerFuelYear_1dig, "NYcountyEnergyPerFuelYear_1dig.tsv")
write_tsv(NYcountyEnergyPerFuelYear_2dig, "NYcountyEnergyPerFuelYear_2dig.tsv")
write_tsv(NYcountyEnergyPerFuelYear_3dig, "NYcountyEnergyPerFuelYear_3dig.tsv.gz")
write_tsv(NYcountyEnergyPerFuelYear_4dig, "NYcountyEnergyPerFuelYear_4dig.tsv.gz")


###############
#END GIVEN CODE
###############

# Connect to a specific postgres database
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = 'oasps.student.rit.edu', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = toString(secrets["username"]),
                 password = toString(secrets["password"]))

dbListTables(con)

gfl_1dig <- read.csv(file = "NYcountyEnergyPerFuelYear_1dig.tsv", sep = "\t", header=TRUE)
gfl_2dig <- read.csv(file = "NYcountyEnergyPerFuelYear_2dig.tsv", sep = "\t", header=TRUE)
gfl_3dig <- read.csv(file = "NYcountyEnergyPerFuelYear_3dig.tsv.gz", sep = "\t", header=TRUE)
gfl_4dig <- read.csv(file = "NYcountyEnergyPerFuelYear_4dig.tsv.gz", sep = "\t", header=TRUE)




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
