# Add fu until 31 Aug 2022
# There is death dates until 28th sep 2022.
# Visual inspection not use the data in September since there seems to be missing data for patients looking at the distribution.
# Therefore, cut the follow-up at 31 Aug 2022.

migration <- inner_join(
  rsdata412 %>%
    select(lopnr, shf_indexdtm),
  migration %>%
    filter(Posttyp == "Utv"),
  by = c("lopnr" = "LopNr"),
  relationship = "many-to-many"
) %>%
  mutate(tmp_migrationdtm = ymd(Datum)) %>%
  filter(
    tmp_migrationdtm > shf_indexdtm,
    tmp_migrationdtm <= global_endfu
  ) %>%
  group_by(lopnr, shf_indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, shf_indexdtm, tmp_migrationdtm)

rsdata412 <- left_join(rsdata412,
  migration,
  by = c("lopnr", "shf_indexdtm")
)

dors <- dors %>%
  group_by(LopNr) %>%
  arrange(ULORSAK) %>%
  slice(n()) %>% # select ULORSAk not ""
  ungroup()

dors <- dors %>%
  mutate(sos_deathdtm = ymd(case_when(
    substr(DODSDAT, 5, 8) == "0000" ~ paste0(substr(DODSDAT, 1, 4), "0701"),
    substr(DODSDAT, 7, 8) == "00" ~ paste0(substr(DODSDAT, 1, 6), "15"),
    TRUE ~ DODSDAT
  ))) %>%
  rename(
    sos_deathcause = ULORSAK,
    lopnr = LopNr
  ) %>%
  select(lopnr, sos_deathdtm, sos_deathcause) %>%
  filter(sos_deathdtm <= global_endfu)

rsdata412 <- left_join(
  rsdata412 %>%
    select(
      lopnr, shf_indexdtm, shf_followuphfunit, shf_followuplocation_cat, shf_sex, shf_age, shf_age_cat, sos_durationhf, shf_ef_cat, shf_qrs, shf_lbbb, shf_bpsys,
      shf_bpdia, shf_map, shf_map_cat, tmp_migrationdtm
    ),
  dors,
  by = "lopnr"
) %>%
  mutate(
    censdtm = coalesce(
      pmin(sos_deathdtm, tmp_migrationdtm, na.rm = TRUE),
      global_endfu
    )
  ) %>%
  select(-tmp_migrationdtm)
