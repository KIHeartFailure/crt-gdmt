# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "diff_crt_shf_cat",
  "indexyear_cat",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "sos_durationhf",
  "sos_prevhfh1yr",
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # treatments
  # "sos_lm_rasiarni",
  # "sos_lm_bbl",
  # "sos_lm_mra",
  "sos_lm_sglt2",
  # "sos_lm_sglt2_empa_dapa",
  "sos_lm_diuretic",
  # "sos_lm_loopdiuretic",
  # "sos_lm_furosemide",
  "sos_lm_ccb",
  "sos_lm_antiplatelet",
  "sos_lm_anticoagulantia",
  # "sos_lm_oralanticoagulantia",
  "sos_lm_insulin",
  "sos_lm_oralantidiabetic",
  "sos_lm_lipidlowering",
  "sos_lm_digoxin",
  "sos_lm_nitrate",
  # "sos_lm_ratecontrol",
  "sos_lm_antiarrhythmic",
  "sos_com_icd",
  "sos_com_pm",

  # comorbs
  "sos_com_diabetes",
  "sos_com_renal",
  "sos_com_af",
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
  "sos_com_alcohol",
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
  "diff_crt_shf_cat",
  # demo
  "shf_age",
  "sos_durationhf",

  # treatments
  "sos_lm_sglt2",
  "sos_lm_insulin",
  "sos_lm_oralantidiabetic",
  "sos_com_icd",
  "sos_com_pm",

  # comorbs
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
      "sos_lm_sglt2",
      "sos_lm_diuretic",
      "sos_lm_ccb",
      "sos_lm_antiplatelet",
      "sos_lm_anticoagulantia",
      "sos_lm_insulin",
      "sos_lm_oralantidiabetic",
      "sos_lm_lipidlowering",
      "sos_lm_digoxin",
      "sos_lm_nitrate",
      "sos_lm_antiarrhythmic",
      "sos_com_icd",
      "sos_com_pm"
    ),
    label = c(
      "Time CRT to SwedeHF registration",
      "Calender year",
      "Previous heart failure hospitalization < 1 year",
      "ACEi/ARB/ARNi",
      "Beta-blocker",
      "MRA",
      "SGLT2i",
      "Diuretic",
      "Calcium channel blockers",
      "Antiplatelet",
      "Anticoagulant",
      "Insulin",
      "Oral glucose lowering",
      "Lipid lowering",
      "Digoxin",
      "Nitrate",
      "Antiarrhythmic",
      "ICD",
      "Pacemaker"
    ),
    unit = c("days", rep(NA, 18))
  )
)

gdmt <- tibble(
  var = c("bbl", "rasiarni", "mra"),
  label = c(
    "Beta-blocker",
    "ACEi/ARB/ARNi",
    "MRA"
  ),
  ATC = c(
    "C07",
    "C09A|C09B|C09C|C09D",
    "C03DA"
  )
)
