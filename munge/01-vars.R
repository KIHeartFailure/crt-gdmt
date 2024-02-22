# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
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
  "sos_lm_rasiarni",
  "sos_lm_bbl",
  "sos_lm_mra",
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
  # demo
  "shf_age",
  "sos_durationhf",

  # treatments
  "sos_lm_sglt2",
  "sos_lm_insulin",
  "sos_lm_oralantidiabetic",

  # comorbs
  "sos_com_pci",
  "sos_com_cabg",
  "sos_com_dementia",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

metavars <- bind_rows(
  metavars,
  tibble(
    variable = c(
      "indexyear",
      "sos_prevhfh1yr",
      "sos_lm_rasiarni",
      "sos_lm_bbl",
      "sos_lm_mra",
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
      "sos_lm_antiarrhythmic"
    ),
    label = c(
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
      "Antiarrhythmic"
    )
  )
)
