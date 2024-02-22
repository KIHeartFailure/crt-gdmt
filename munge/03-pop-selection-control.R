# Inclusion/exclusion criteria --------------------------------------------------------
controldata <- left_join(rsdata412,
  crtdatakeep,
  by = "lopnr"
) %>%
  mutate(diff = as.numeric(shf_indexdtm - indexdtm)) %>%
  filter(is.na(diff) | diff < -30) %>%
  select(-indexdtm, -diff)
flowcontrol <- c("Posts for patients without CRT in NPR (KVÅ 10 FPE26, FPG36) within 30 days prior to registration in SwedeHF", nrow(controldata))

controldata <- controldata %>%
  filter(is.na(shf_device) | shf_device %in% c("No", "Pacemaker", "ICD"))
flowcontrol <- rbind(flowcontrol, c("Posts without CRT in SwedeHF", nrow(controldata)))

controldata <- controldata %>%
  filter(shf_indexdtm >= ymd("2009-01-01"))
flowcontrol <- rbind(flowcontrol, c("Include >= 2009-01-01", nrow(controldata)))

controldata <- controldata %>%
  filter(sos_durationhf >= 92)
flowcontrol <- rbind(flowcontrol, c("Include HF duration in the NPR > 3 months", nrow(controldata)))

controldata <- controldata %>%
  mutate(diff = as.numeric(censdtm - shf_indexdtm)) %>%
  filter(diff >= 426) # 365.25 + 61
flowcontrol <- rbind(flowcontrol, c("Include with >= 1 year + 2 months follow-up", nrow(controldata)))

controldata <- controldata %>%
  filter(shf_ef_cat == "HFrEF" & !is.na(shf_ef_cat))
flowcontrol <- rbind(flowcontrol, c("Posts with HFrEF", nrow(controldata)))

flowcontrol <- rbind(flowcontrol, c("Add LBBB+QRS criteria ect when Daniela sends onfo", NA))

controldata <- controldata %>%
  group_by(lopnr) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  rename(indexdtm = shf_indexdtm) %>%
  select(lopnr, indexdtm, shf_followuphfunit, shf_followuplocation_cat, shf_sex, shf_age, shf_age_cat, censdtm, sos_durationhf)
flowcontrol <- rbind(flowcontrol, c("First post/patient for controls", nrow(controldata))) %>%
  as_tibble()

names(flowcontrol) <- c("Criteria", "Ncontrol")

flowboth <- full_join(flowcrt %>% mutate(ordercrt = 1:n()),
  flowcontrol %>% mutate(ordercontrol = 1:n()),
  by = "Criteria"
) %>%
  mutate(order = coalesce(ordercrt, ordercontrol)) %>%
  arrange(order) %>%
  select(Criteria, Ncrt, Ncontrol)

flow <- flow[c(1:8, 10), 1:2]

names(flow) <- c("Criteria", "N")
flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)
flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))
flow <- bind_rows(flow, flowboth)

# join crt and controls

rsdata <- bind_rows(
  crtdata %>% mutate(pop = 1),
  controldata %>% mutate(pop = 0)
) %>%
  mutate(
    pop = factor(pop, 0:1, labels = c("Control", "CRT")),
    indexyear = year(indexdtm)
  )

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

ntmp <- rsdata %>% count(pop)

flow <- flow %>%
  add_row(
    Criteria = "First post/patient WILL NEED TO ADJUST THIS DEPENDING ON QRS, LBB. Also check limit above (30 days)",
    N = NA,
    Ncrt = paste0(ntmp %>% filter(pop == "CRT") %>% pull(n)),
    Ncontrol = paste0(ntmp %>% filter(pop == "Control") %>% pull(n))
  )
