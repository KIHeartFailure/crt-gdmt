# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load(here(shfdbpath, "data/v412/rsdata412.RData"))
load(file = paste0(shfdbpath, "/data/", datadate, "/patregrsdata.RData"))

# Meta data ect -----------------------------------------------------------

metavars <- read.xlsx(here(shfdbpath, "metadata/meta_variables.xlsx"))
load(here(paste0(shfdbpath, "data/v412/meta_statreport.RData")))

# Munge data --------------------------------------------------------------

source(here("munge/01-vars.R"))
source(here("munge/02-pop-selection-crt.R"))
source(here("munge/03-pop-selection-control.R"))
load(paste0(shfdbpath, "/data/", datadate, "/rawData_scb.RData"))
source(here("munge/04-scb-socioec.R"))
source(here("munge/05-npr-outcome.R"))
source(here("munge/06-charlsoncomorbindex_sos.R"))
source(here("munge/07-pdr-meds.R"))
source(here("munge/08-fix-vars.R"))
# source(here("munge/09-mi.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/data.RData"),
  list = c(
    "rsdata",
    # "imprsdata",
    "flow",
    "modvars",
    "tabvars",
    "metalm",
    "metavars",
    "outcommeta"
  )
)

# create workbook to write tables to Excel
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheet = "Information")
openxlsx::writeData(wb, sheet = "Information", x = "Tables in xlsx format for tables in Statistical report: The role of CRT as enables for optimizing guideline directed medical therapy (GDMT) in patients with heart failure and reduced ejection fraction (HFrEF): data from the SwedeHF Registry", rowNames = FALSE, keepNA = FALSE)
openxlsx::saveWorkbook(wb,
  file = here::here("output/tabs/tables.xlsx"),
  overwrite = TRUE
)

# create powerpoint to write figs to PowerPoint
figs <- officer::read_pptx()
print(figs, target = here::here("output/figs/figs.pptx"))
