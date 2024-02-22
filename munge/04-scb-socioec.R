# child
child <- bind_rows(barnadop, barnbio)

child2 <- inner_join(
  rsdata %>% select(lopnr, indexdtm, indexyear),
  child,
  by = c("lopnr" = "LopNr")
)

child2 <- child2 %>%
  filter(fodelsearbarn < indexyear) %>%
  group_by(lopnr, indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(scb_child = 1) %>%
  select(lopnr, indexdtm, scb_child)

rsdata <- left_join(
  rsdata,
  child2,
  by = c("lopnr", "indexdtm")
) %>%
  mutate(scb_child = factor(replace_na(scb_child, 0),
    levels = 0:1,
    labels = c("No", "Yes")
  ))

# lisa
rsdata <- rsdata %>%
  mutate(scbyear = indexyear - 1)

lisa <- lisa %>%
  mutate(
    scb_famtype = case_when(
      FamTypF %in% c(11, 12, 13, 21, 22, 23, 31, 32, 41, 42) ~ "Cohabitating",
      FamTypF %in% c(50, 60) ~ "Living alone"
    ),
    Sun2000niva = coalesce(Sun2000niva_old, Sun2000niva_Old, Sun2020Niva_Old),
    scb_education = case_when(
      Sun2000niva %in% c(1, 2) ~ "Compulsory school",
      Sun2000niva %in% c(3, 4) ~ "Secondary school",
      Sun2000niva %in% c(5, 6, 7) ~ "University"
    ),
    scb_dispincome = coalesce(DispInk04, DispInk)
  ) %>%
  select(LopNr, year, starts_with("scb_"))

rsdata <- left_join(
  rsdata,
  lisa,
  by = c("lopnr" = "LopNr", "scbyear" = "year")
) %>%
  select(-scbyear)

rm(child)
rm(lisa)
