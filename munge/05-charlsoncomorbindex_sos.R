# Charlson comobidity index -----------------------------------------------

# Myocardial infarction

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_mi",
  diakod = " 410| 412| I21| I22| I252",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Congestive heart failure assume all have

# Peripheral vascular disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_pvd",
  diakod = " 440| 441| 443B| 443X| 447B| 557| I70| I71| I731| I738| I739| I771| I790| I792| K55",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Cerebrovascular disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_cd",
  diakod = " 43[0-8]| G45| I6[0-4]| I67| I69",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Chronic pulmonary disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_copd",
  diakod = " 491| 492| 496| J4[3-4]| 490| 49[3-5]| 50[0-8]| 516| 517| J41| J42| J4[5-7]| J6[0-9]| J70",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Rheumatic disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_rheumatic",
  diakod = " 446| 696A| 710[A-E]| 714| 719D| 720| 725| M05| M06| M123| M07[0-3]| M08| M13| M30| M31[3-6]| M32| M33| M34| M350| M351| M353| M45| M46",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Dementia

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_dementia",
  diakod = " 290| 294B| 331[A-C]| 331X| F0[0-3]| F051| G30| G311| G319",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Hemiplegia or paraplegia

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_plegia",
  diakod = " 342| 343| 344[A-F]| G114| G8[0-2]| G83[0-3]| G838",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Diabetes

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_diabetes",
  diakod = " 250[A-C]| E100| E101| E110| E111| E120| E121| E130| E131| E140| E141",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Diabetes with end organ damage

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_diabetescompliation",
  diakod = " 250[D-G]| E10[2-5]| E107| E11[2-7]| E12[2-7]| E13[2-7]| E14[2-7]",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Renal disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_renal",
  diakod = " 403A| 403B| 403X| 582| 583| 585| 586| 588A| V42A| V45B| V56| N03[2-7]| N05[2-7]| N11| N18| N19| N250| I120| I131| Q61[1-4]| Z49| Z940| Z992",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Mild liver disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_livermild",
  diakod = " 070| 571C| 571E| 571F| 573| B1[5-9]| K703| K709| K73| K746| K754",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

## liver spec

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_liverspec",
  diakod = " 789F| R18",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)


# Moderate or severe liver disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_livermodsev",
  diakod = " 789F| 456[A-C]| 572[D-E]| 572C| R18| I850| I859| I982| I983",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Peptic ulcer disease

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_pud",
  diakod = " 53[1-4]| K2[5-8]",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_malignancy",
  diakod = " 1[4-9]| 20[0-8]| C[0-7]| C8[0-6]| C8[8-9]| C9[0-7]",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# Metastatic solid tumor

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_metastatictumor",
  diakod = " 19[6-8]| 199A| 199B| C7[7-9]| C80",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

# AIDS/HIV

rsdata <- create_sosvar(
  sosdata = patregrsdata,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_hiv",
  diakod = " 079J| 279K| B2[0-4]| F024| O987| R75| Z219| Z717",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = FALSE
)

rsdata <- rsdata %>%
  mutate(
    sos_com_cci_chf = 1,
    sos_com_cci_diabetes = if_else(sos_com_cci_diabetescompliation == 1, 0, sos_com_cci_diabetes),
    sos_com_cci_livermodsev = if_else(sos_com_cci_livermild == 1 & sos_com_cci_liverspec == 1, 1, sos_com_cci_livermodsev),
    sos_com_cci_livermild = if_else(sos_com_cci_livermodsev == 1, 0, sos_com_cci_livermild),
    sos_com_cci_malignancy = if_else(sos_com_cci_metastatictumor == 1, 0, sos_com_cci_malignancy),
    sos_com_cci_diabetescompliation = sos_com_cci_diabetescompliation * 2,
    sos_com_cci_plegia = sos_com_cci_plegia * 2,
    sos_com_cci_renal = sos_com_cci_renal * 2,
    sos_com_cci_malignancy = sos_com_cci_malignancy * 2,
    sos_com_cci_livermodsev = sos_com_cci_livermodsev * 3,
    sos_com_cci_metastatictumor = sos_com_cci_metastatictumor * 6,
    sos_com_cci_hiv = sos_com_cci_hiv * 6
  ) %>%
  select(-sos_com_cci_liverspec) %>%
  mutate(sos_com_charlsonci = rowSums(select(., starts_with("sos_com_cci_")))) %>%
  select(-starts_with("sos_com_cci_"))

ccimeta <- metaout
rm(metaout)
rm(patregrsdata)
