# Comorbidities -----------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "hypertension",
  diakod = " I1[0-5]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "diabetes",
  diakod = " E1[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "ihd",
  diakod = " 41[0-4]| I2[0-5]| Z951| Z955",
  opkod = " FNA| FNB| FNC| FND| FNE| FNF| FNH| FNG",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "pci",
  opkod = " FNG",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "cabg",
  diakod = " Z951| Z955",
  opkod = " FNA| FNB| FNC| FND| FNE| FNF| FNH",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "pad",
  diakod = " I7[0-3]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "af",
  diakod = " I48",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "stroke",
  diakod = " 43[0-4]| 438| I6[0-4]| I69[0-4]",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "valvular",
  diakod = " I0[5-8]| I3[4-9]| Q22| Q23[0-3]| Q23[0-3]| Q23[5-9]| Z95[2-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "renal",
  diakod = " N1[7-9]| Z491| Z492",
  opkod = " KAS00| KAS10| KAS20| DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "copd",
  diakod = " J4[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "liver",
  diakod = " B18| I85| I864| I982| K70| K710| K711| K71[3-7]| K7[2-4]| K760| K76[2-9]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "dementia",
  diakod = " F0[0-4]| R54",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "com",
  name = "cancer3y",
  diakod = " C",
  stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "muscoloskeletal3y",
  diakod = " M",
  stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  evar = ekod_all,
  type = "com",
  name = "alcohol",
  diakod = " E244| E52| F10| G312| G621| G721| I426| K292| K70| K860| O354| P043| Q860| T51| Z502| Z714",
  ekod = " Y90| Y91",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "bleed",
  diakod = " S064| S065| S066| I850| I983| K226| K250| K252| K254| K256| K260| K262| K264| K266| K270| K272| K274| K276| K280| K284| K286| K290| K625| K661| K920| K921| K922| H431| N02| R04| R58| T810| D629",
  opkod = " DR029",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "icd",
  opkod = " FPG",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "pm",
  opkod = " FPE| FPF| DF013| ZXG40| ZXG50| TFP00",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

outcommeta <- metaout
rm(metaout)

# Time since last HF hospitalization --------------------------------------

hfhospsos <- patregrsdata %>%
  filter(sos_source == "sv") %>%
  mutate(tmp_hfhospsos = stringr::str_detect(HDIA, global_icdhf)) %>%
  filter(tmp_hfhospsos)

hfhosp <- inner_join(
  rsdata %>% select(lopnr, indexdtm),
  hfhospsos,
  by = "lopnr",
  relationship = "many-to-many"
) %>%
  mutate(tmp_sosdtm = coalesce(UTDATUM, INDATUM)) %>%
  filter(tmp_sosdtm <= indexdtm) %>%
  group_by(lopnr, indexdtm) %>%
  arrange(tmp_sosdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  select(lopnr, indexdtm, tmp_sosdtm)

rsdata <- left_join(
  rsdata,
  hfhosp,
  by = c("lopnr", "indexdtm")
) %>%
  mutate(
    sos_timeprevhosphf = as.numeric(indexdtm - tmp_sosdtm),
    sos_timeprevhosphf = case_when(
      is.na(sos_timeprevhosphf) ~ NA_real_,
      TRUE ~ sos_timeprevhosphf
    )
  ) %>%
  select(-tmp_sosdtm)
