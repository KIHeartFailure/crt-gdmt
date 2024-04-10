load(here(shfdbpath, "data", datadate, "20221012/lmswedehf.RData"))

# Select ATC codes --------------------------------------------------------

lmcrt <- lmswedehf %>%
  filter(
    ANTAL >= 0 &
      !is.na(forpddd)
  )

lmcrt <- left_join(
  rsdata %>%
    select(lopnr, indexdtm),
  lmcrt,
  by = "lopnr",
  relationship = "many-to-many"
)

lmsel <- lmcrt %>%
  mutate(diff = as.numeric(EDATUM - indexdtm)) %>%
  filter(diff >= -120 & diff <= 0) %>%
  select(lopnr, indexdtm, EDATUM, ATC)

rm(lmswedehf)
gc()

rsdata <- create_medvar(
  atc = "^(C09A|C09B|C09C|C09D)", medname = "rasiarni",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C07)", medname = "bbl",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C03DA)", medname = "mra",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C03)", medname = "diuretic",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C01DA)", medname = "nitrate",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(B01AC)", medname = "antiplatelet",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(B01AA|B01AB|B01AE|B01AF|B01AX05)", medname = "anticoagulantia",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C10)", medname = "lipidlowering",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(A10BK|A10BD16|A10BD15|A10BD20|A10BD23|A10BD27|A10BD25|A10BD21|A10BD29|A10BD24)", medname = "sglt2",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C08)", medname = "ccb",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(A10A)", medname = "insulin",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(A10B)", medname = "oralantidiabetic",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C01AA05)", medname = "digoxin",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
rsdata <- create_medvar(
  atc = "^(C01BD01|C01BD07|C01BC04|C01BC03|C07AA07|C01BA03)", medname = "antiarrhythmic",
  cohortdata = rsdata,
  meddata = lmsel,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-0"
)
