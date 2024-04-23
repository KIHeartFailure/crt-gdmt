flow <- flow[c(1:8, 10), 1:2]
names(flow) <- c("Criteria", "N")
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

rsdata <- rsdata412 %>%
  filter(!is.na(shf_ef_cat))
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with missing EF",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(shf_ef_cat == "HFrEF")
flow <- flow %>%
  add_row(
    Criteria = "Include posts with HFrEF",
    N = nrow(rsdata)
  )

# crt only
crtdata <- icdpm %>%
  filter(EVENTTYPE == "Intervention" &
    INTERVENTYIONTASKTYPE == "Implantation" &
    DEVICETYPE %in% c("Pacemaker", "ICD") &
    (CRT %in% c(1, 2) | MODE == "DDDR+CRT")) %>%
  select(LopNr, EVENTDATE) %>%
  distinct() %>%
  group_by(LopNr) %>%
  arrange(EVENTDATE) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    lopnr = LopNr,
    crtdtm = EVENTDATE
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
    crt = if_else(crt == 0 & control == 0, NA, crt)
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

rsdata <- rsdata %>%
  filter(indexdtm >= ymd("2009-01-01"))
flow <- flow %>%
  add_row(
    Criteria = "Include posts >= 2009-01-01",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  filter(sos_durationhf >= 92)
flow <- flow %>%
  add_row(
    Criteria = "Include posts with HF duration in the NPR > 3 months",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(diff = as.numeric(censdtm - indexdtm)) %>%
  filter(diff >= 426) %>% # 365.25 + 61
  select(-diff)
flow <- flow %>%
  add_row(
    Criteria = "Include posts with >= 1 year + 2 months follow-up",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )

rsdata <- rsdata %>%
  mutate(keep = case_when(
    crt == 1 ~ 1,
    is.na(diff_crt_shf) ~ 1,
    diff_crt_shf < 0 ~ 0,
    diff_crt_shf > 426 ~ 1,
    TRUE ~ 1
  )) %>%
  filter(keep == 1) %>%
  select(-keep)
flow <- flow %>%
  add_row(
    Criteria = "Include control posts without prior CRT",
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
    Criteria = "First post (control), closest post (CRT) / patient",
    Ncrt = nrow(rsdata %>% filter(crt == 1)),
    Ncontrol = nrow(rsdata %>% filter(crt == 0))
  )
