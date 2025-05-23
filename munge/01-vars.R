# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "absdiff_crt_shf",
  "diff_crt_shf_cat",
  "crt_type",
  "indexyear_cat",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_qrs",
  "shf_qrs_cat",
  "shf_lbbb",
  "shf_ef",
  "sos_durationhf",
  "sos_durationhf_cat",
  "sos_prevhfh1yr",
  "shf_followuphfunit",
  "shf_followuplocation_cat",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bpsys",
  "shf_bpsys_cat",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",
  # treatments
  # "sos_lm_rasiarni",
  # "sos_lm_bbl",
  # "sos_lm_mra",
  "sos_lm_sglt2",
  # "sos_lm_diuretic",
  "sos_lm_ccb",
  "sos_lm_antiplatelet",
  "sos_lm_anticoagulantia",
  "sos_lm_insulin",
  "sos_lm_oralantidiabetic",
  "sos_lm_lipidlowering",
  "sos_lm_digoxin",
  "sos_lm_nitrate",
  "sos_lm_antiarrhythmic",
  "icdpm_com_icd",
  "icdpm_com_pm",

  # comorbs
  "sos_com_diabetes",
  # "sos_com_renal",
  "sos_com_af",
  "shf_ekg",
  "sos_com_ihd",
  "sos_com_hypertension",
  "sos_com_pad",
  "sos_com_pci",
  "sos_com_cabg",
  "sos_com_stroke",
  "sos_com_valvular",
  "sos_com_cancer3y",
  "sos_com_copd",
  "sos_com_liver",
  "sos_com_dementia",
  "sos_com_bleed",
  "sos_com_muscoloskeletal3y",
  # "sos_com_alcohol",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat"
)

# Variables for models (imputation, log, cox reg) ----------------------------

tabvars_not_in_mod <- c(
  "shf_qrs",
  "shf_qrs_cat",
  "shf_lbbb",
  "diff_crt_shf_cat",
  "absdiff_crt_shf",
  "crt_type",
  # demo
  "sos_durationhf",
  "shf_age",
  "shf_ef",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_ntprobnp",

  # treatments
  "sos_lm_sglt2",
  "sos_lm_insulin",
  "sos_lm_oralantidiabetic",
  "icdpm_com_icd",
  "icdpm_com_pm",

  # comorbs
  "shf_ekg",
  "sos_com_pci",
  "sos_com_cabg",
  "sos_com_dementia",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

metavars <- bind_rows(
  metavars,
  tibble(
    variable = c(
      "diff_crt_shf",
      "indexyear",
      "sos_prevhfh1yr",
      "rasiarni",
      "bbl",
      "mra",
      "loop",
      "sos_lm_sglt2",
      "sos_lm_ccb",
      "sos_lm_antiplatelet",
      "sos_lm_anticoagulantia",
      "sos_lm_insulin",
      "sos_lm_oralantidiabetic",
      "sos_lm_lipidlowering",
      "sos_lm_digoxin",
      "sos_lm_nitrate",
      "sos_lm_antiarrhythmic",
      "icdpm_com_icd",
      "icdpm_com_pm",
      "shf_qrslbbb",
      "crt_type"
    ),
    label = c(
      "Time CRT to SwedeHF registration",
      "Calender year",
      "Previous heart failure hospitalization < 1 year",
      "ACEi/ARB/ARNi",
      "Beta-blocker",
      "MRA",
      "Loop diuretic",
      "SGLT2i",
      "Calcium channel blockers",
      "Antiplatelet",
      "Anticoagulant",
      "Insulin",
      "Oral glucose lowering",
      "Lipid lowering",
      "Digoxin",
      "Nitrate",
      "Antiarrhythmic",
      "Previous ICD",
      "Previous Pacemaker",
      "QRS/LBBB",
      "CRT type"
    ),
    unit = c("days", rep(NA, 20))
  )
)

metavars <- metavars %>%
  mutate(unit = if_else(variable == "sos_durationhf", "months", unit))

gdmt <- tibble(
  var = c("bbl", "rasiarni", "mra", "loop"),
  label = c(
    "Beta-blocker",
    "ACEi/ARB/ARNi",
    "MRA",
    "Loop diuretic"
  ),
  ATC = c(
    "C07",
    "C09A|C09B|C09C|C09D",
    "C03DA",
    "C03C"
  )
)
