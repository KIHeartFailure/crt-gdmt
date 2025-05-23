flow <- flow[c(1:8, 10), 1:2]
names(flow) <- c("Criteria", "N")

flow <- flow %>%
  mutate(Criteria = if_else(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)",
    "Exclude posts with with index date > 2023-12-31", Criteria
  ))

flow <- flow %>%
  mutate(
    Ncrt = NA,
    Ncontrol = NA
  ) %>%
  add_row(
    Criteria = "General inclusion/exclusion criteria",
    N = NA, .before = 1
  )
flow <- flow %>%
  add_row(
    Criteria = "Project specific inclusion/exclusion criteria",
    N = NA
  )

rsdata <- rsdata421 %>%
  mutate(sos_deathdtm = if_else(sos_out_death == "Yes", shf_indexdtm + sos_outtime_death, NA_Date_)) %>%
  select(
    lopnr, shf_indexdtm, shf_centre, shf_followuphfunit, shf_followuplocation_cat, shf_sex, shf_age, shf_age_cat, sos_durationhf, shf_ef_cat, shf_ef, shf_qrs, shf_lbbb, shf_bpsys,
    shf_bpdia, shf_map, shf_map_cat, shf_heartrate, shf_gfrckdepi, shf_gfrckdepi_cat, shf_ntprobnp, shf_nyha, shf_nyha_cat, censdtm, shf_ekg, sos_deathdtm
  ) %>%
  filter(!is.na(shf_ef_cat))
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with missing EF",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(shf_ef_cat == "HFrEF") %>%
  mutate(shf_ef_cat = droplevels(shf_ef_cat), 
         shf_ef = droplevels(shf_ef)
         )
flow <- flow %>%
  add_row(
    Criteria = "Include posts with HFrEF",
    N = nrow(rsdata)
  )

# crt only
crtdata <- icdpm %>%
  filter(EVENTTYPE == "Intervention" & !PATIENTTYPE %in% c("null", "ILR") &
    INTERVENTYIONTASKTYPE == "Implantation" &
    DEVICETYPE %in% c("Pacemaker", "ICD") &
    (CRT %in% c(1, 2) | MODE == "DDDR+CRT")) %>%
  mutate(crt_type = factor(if_else(DEVICETYPE %in% c("Pacemaker"), 1, 2), levels = 1:2, labels = c("CRT-P", "CRT-D"))) %>%
  select(LopNr, EVENTDATE, crt_type) %>%
  distinct() %>%
  group_by(LopNr) %>%
  arrange(EVENTDATE, desc(crt_type)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    lopnr = LopNr,
    crtdtm = EVENTDATE
  )

icdpmdata <- icdpm %>%
  filter(EVENTTYPE == "Intervention" & !PATIENTTYPE %in% c("null", "ILR") &
    INTERVENTYIONTASKTYPE == "Implantation" &
    DEVICETYPE %in% c("Pacemaker", "ICD", "ICD-elektrod", "Pacemakerelektrod")) %>%
  select(LopNr, EVENTDATE, DEVICETYPE) %>%
  distinct() %>%
  group_by(LopNr, DEVICETYPE) %>%
  arrange(EVENTDATE) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    lopnr = LopNr
  )

rsdata <- left_join(rsdata, crtdata, by = "lopnr") %>%
  mutate(
    diff_crt_shf = as.numeric(crtdtm - shf_indexdtm),
    crt = case_when(
      is.na(diff_crt_shf) ~ 0,
      diff_crt_shf >= -365 & diff_crt_shf <= 30 ~ 1,
      TRUE ~ 0
    ),
    indexdtm = if_else(crt == 1, crtdtm, shf_indexdtm),
    indexyear = year(indexdtm),
    sos_durationhf = if_else(crt == 1, sos_durationhf + diff_crt_shf, sos_durationhf),
    control = case_when(
      is.na(shf_qrs) | is.na(shf_lbbb) ~ 0,
      shf_qrs >= 130 & shf_lbbb == "Yes" ~ 1,
      shf_qrs >= 150 & shf_lbbb == "No" ~ 1,
      TRUE ~ 0
    ),
    crt = if_else(crt == 0 & control == 0, NA, crt),
    crt_type = if_else(crt == 1, crt_type, NA)
  ) %>%
  select(-shf_indexdtm)

rsdata <- rsdata %>%
  filter(!is.na(crt)) %>%
  select(-control)

flow <- flow %>%
  add_row(
    Criteria = "Fullfills criteria for CRT, first CRT post in ICD/PM Registry and SwedeHF post within 1 year prior to 30 days after CRT",
    Ncrt = nrow(rsdata %>% filter(crt == 1))
  )

flow <- flow %>%
  add_row(
    Criteria = "Fullfills criteria for control, QRS >= 130 and LBBB or QRS >= 150 and no LBBB",
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )


# prior icd pm
icdpmdata2 <- left_join(rsdata %>% select(lopnr, indexdtm, crt),
  icdpmdata,
  by = "lopnr"
) %>%
  filter(EVENTDATE < indexdtm) %>%
  mutate(
    icdpm_com_pm = if_else(DEVICETYPE %in% c("Pacemaker", "Pacemakerelektrod"), 1, 0),
    icdpm_com_icd = if_else(DEVICETYPE %in% c("ICD", "ICD-elektrod"), 1, 0),
  ) %>%
  select(-EVENTDATE, -DEVICETYPE)

rsdata <- left_join(rsdata,
  icdpmdata2 %>%
    filter(icdpm_com_pm == 1) %>%
    select(-icdpm_com_icd) %>%
    distinct(),
  by = c("lopnr", "indexdtm", "crt")
) %>%
  mutate(icdpm_com_pm = ynfac(replace_na(icdpm_com_pm, 0)))

rsdata <- left_join(rsdata,
  icdpmdata2 %>%
    filter(icdpm_com_icd == 1) %>%
    select(-icdpm_com_pm) %>%
    distinct(),
  by = c("lopnr", "indexdtm", "crt")
) %>%
  mutate(icdpm_com_icd = ynfac(replace_na(icdpm_com_icd, 0)))

rsdata <- rsdata %>%
  filter(indexdtm >= ymd("2009-01-01"))
flow <- flow %>%
  add_row(
    Criteria = "Include posts >= 2009-01-01",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  filter(indexdtm <= ymd("2022-08-31"))
flow <- flow %>%
  add_row(
    Criteria = "Include posts <= 2022-08-31 (data available from the ICD/PM Registry)",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  filter(sos_durationhf >= 92) # 183)
flow <- flow %>%
  add_row(
    Criteria = "Include posts with HF duration in the NPR > 3 months",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(diff = as.numeric(sos_deathdtm - indexdtm)) %>%
  filter(diff >= 610 | is.na(diff)) %>% # 1.5 years fu + 2 mo
  select(-diff)
flow <- flow %>%
  add_row(
    Criteria = "Include posts alive at >= 1.5 years + 2 months follow-up",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(diff = as.numeric(censdtm - indexdtm)) %>%
  filter(diff >= 610) %>% # 1.5 years fu + 2 mo
  select(-diff)
flow <- flow %>%
  add_row(
    Criteria = "Include posts with >= 1.5 years + 2 months follow-up",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(keep = case_when(
    crt == 1 ~ 1,
    is.na(diff_crt_shf) ~ 1,
    diff_crt_shf < 0 ~ 0,
    diff_crt_shf > 610 ~ 1,
    TRUE ~ 1
  )) %>%
  filter(keep == 1) %>%
  select(-keep)
flow <- flow %>%
  add_row(
    Criteria = "Include control posts without CRT prior to or up until 30 days after index",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdatacontrol <- rsdata %>%
  filter(crt == 0) %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

rsdatacrt <- rsdata %>%
  filter(crt == 1) %>%
  group_by(lopnr) %>%
  arrange(abs(diff_crt_shf)) %>%
  slice(1) %>%
  ungroup()

rsdata <- bind_rows(rsdatacontrol, rsdatacrt)

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- flow %>%
  add_row(
    Criteria = "First patient of first post (control), closest post (CRT) / patient",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(
    shf_qrs = if_else(diff_crt_shf > 0 & crt == "CRT", NA_real_, shf_qrs),
    shf_lbbb = if_else(diff_crt_shf > 0 & crt == "CRT", NA, shf_lbbb),
    senspop = case_when(
      is.na(shf_qrs) | is.na(shf_lbbb) | is.na(shf_ekg) ~ F,
      shf_qrs >= 150 & shf_lbbb == "Yes" & shf_ekg %in% c("Sinus") & shf_ef == "<30" ~ T,
      TRUE ~ F
    ),
    senscrtfu = case_when(
      indexdtm > (ymd("2022-08-31") - 610) ~ F,
      crt == 0 & as.numeric(crtdtm - indexdtm) <= 610 ~ F,
      TRUE ~ T
    )
  )

flow <- flow %>%
  add_row(
    Criteria = "  Sensitivity analyses (Class I recommendation according to 2021 guidelines)",
    Ncrt = nrow(rsdata %>% filter(crt == 1 & senspop)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0 & senspop))
  )

flow <- flow %>%
  add_row(
    Criteria = "  Sensitivity analyses (exclude controls who recieved CRT during fu, exclude also patients included > 2022-08-31 - (1.5 years + 2 months), full coverage of the ICD/PM registry during the fu period)",
    Ncrt = nrow(rsdata %>% filter(crt == 1 & senscrtfu)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0 & senscrtfu))
  )
