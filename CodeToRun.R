# Please restore the renv file first:
# if you have not installed renv, please first install: install.packages("renv")
renv::activate()
renv::restore()

# Required packages
library(CDMConnector)
library(DBI)
library(here)
library(log4r)
library(do)
library(tidyverse)

# Connect to database
# Please fill with your own database information
server_dbi <- Sys.getenv("...")
user       <- Sys.getenv("...")
password   <- Sys.getenv("...")
port       <- Sys.getenv("...")
host       <- Sys.getenv("...")

db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password
)

cdm <- cdm_from_con(
  db,
  cdm_schema = "...",
  write_schema = "..."
)

db.name <- "..."

# Run the code
source(here::here("RunCoverage.R"))
