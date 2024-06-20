# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load(here(shfdbpath, "data/v421/rsdata421.RData"))
load(file = paste0(shfdbpath, "/data/", datadate, "/patregrsdata.RData"))
load(paste0(shfdbpath, "/data/", datadate, "/rawData_scb.RData"))
load(file = paste0(shfdbpath, "/data/", datadate, "/rawData_sosdors.RData"))
icdpm <- read_sas(paste0(shfdbpath, "/raw-data/SOS/20220908/komp ICDPMreg regstockholm/sos_result.sas7bdat"))

# Meta data ect -----------------------------------------------------------

metavars <- read.xlsx(here(shfdbpath, "metadata/meta_variables.xlsx"))
load(here(paste0(shfdbpath, "data/v421/meta_statreport.RData")))

# Munge data --------------------------------------------------------------

source(here("munge/01-vars.R"))
source(here("munge/02-pop-selection.R"))
source(here("munge/03-scb-socioec.R"))
source(here("munge/04-npr-comorb.R"))
source(here("munge/05-charlsoncomorbindex_sos.R"))
source(here("munge/06-pdr-meds.R"))
source(here("munge/07-pdr-medgdmt.R"))
source(here("munge/08-fix-vars.R"))
source(here("munge/09-mi.R"))
source(here("munge/10-fu.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/data.RData"),
  list = c(
    "rsdata",
    "imprsdata",
    "imprsdatahypo",
    "imprsdatadurhf1",
    "imprsdatadurhf2",
    "imprsdatadurhf3",
    "rsdata_fu",
    "flow",
    "modvars",
    "tabvars",
    "metalm",
    "metavars",
    "outcommeta",
    "gdmt",
    "checkcombdoses",
    "misstabday"
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
