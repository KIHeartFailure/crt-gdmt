# Inclusion/exclusion criteria --------------------------------------------------------
crtdata <- patregrsdata %>%
  filter(str_detect(OP_all, " FPE26| FPG36")) %>%
  mutate(indexdtm = coalesce(UTDATUM, INDATUM)) %>%
  select(lopnr, indexdtm, OP_all)
flowcrt <- c("Posts with CRT (KVÅ FPE26, FPG36) in NPR and at some point registred in SwedeHF", nrow(crtdata))

crtdata <- crtdata %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()
flowcrt <- rbind(flowcrt, c("First post/patient with CRT in NPR", nrow(crtdata)))
crtdatakeep <- crtdata

excludecrtdata <- patregrsdata %>%
  filter(str_detect(OP_all, " DF001| DF002| DF003| DF004") |
    str_detect(OP_all, " I978| 4260| I442")) %>%
  mutate(exdtm = coalesce(UTDATUM, INDATUM)) %>%
  select(lopnr, exdtm) %>%
  group_by(lopnr) %>%
  arrange(exdtm) %>%
  slice(1) %>%
  ungroup()

crtdata <- left_join(crtdata,
  excludecrtdata,
  by = "lopnr"
) %>%
  mutate(diff = as.numeric(exdtm - indexdtm)) %>%
  filter(diff > 0 | is.na(diff)) %>%
  select(-exdtm, -diff)
flowcrt <- rbind(flowcrt, c("Exclude with ICD-9/10: I978, 4260, I442 or KVÅ: DF001-4 before or at CRT", nrow(crtdata)))

crtdata <- crtdata %>%
  filter(indexdtm >= ymd("2009-01-01"))
flowcrt <- rbind(flowcrt, c("Include >= 2009-01-01", nrow(crtdata)))

rsdataunique <- rsdata412 %>%
  select(lopnr, shf_indexdtm, censdtm, sos_durationhf) %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

crtdata <- left_join(crtdata,
  rsdataunique,
  by = "lopnr"
) %>%
  mutate(
    diff = as.numeric(indexdtm - shf_indexdtm),
    sos_durationhf = sos_durationhf + diff
  ) %>%
  filter(sos_durationhf >= 92) %>%
  select(-shf_indexdtm)
flowcrt <- rbind(flowcrt, c("Include HF duration in the NPR > 3 months", nrow(crtdata)))

crtdata <- crtdata %>%
  mutate(diff = as.numeric(censdtm - indexdtm)) %>%
  filter(diff >= 426) # 365.25 + 61
flowcrt <- rbind(flowcrt, c("Include with >= 1 year + 2 months follow-up", nrow(crtdata)))

crtdata <- left_join(crtdata,
  rsdata412 %>% select(lopnr, shf_indexdtm, shf_sex, shf_age, shf_age_cat, shf_ef_cat, shf_followuphfunit, shf_followuplocation_cat),
  by = "lopnr"
) %>%
  mutate(diff = as.numeric(shf_indexdtm - indexdtm)) %>%
  filter(shf_ef_cat == "HFrEF" & diff >= -365 & diff <= 0) %>%
  group_by(lopnr) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  select(-OP_all, -diff, -shf_ef_cat)
flowcrt <- rbind(flowcrt, c("CRT withn 1 year prior from SwedeHF post with HFrEF", nrow(crtdata))) %>%
  as_tibble()

names(flowcrt) <- c("Criteria", "Ncrt")
