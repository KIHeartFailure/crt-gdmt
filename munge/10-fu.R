rsdata_fu <- left_join(rsdata,
  rsdata421 %>% select(lopnr, shf_indexdtm, shf_gfrckdepi, shf_bpsys),
  by = "lopnr",
  suffix = c("_base", "_1yr")
)

rsdata_fu <- rsdata_fu %>%
  mutate(
    diff = as.numeric(shf_indexdtm - indexdtm)
  ) %>%
  filter(diff >= 306 & diff <= 425) %>%
  group_by(lopnr) %>%
  arrange(abs(diff)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    shf_bpsys_diff = shf_bpsys_1yr - shf_bpsys_base,
    shf_gfrckdepi_diff = shf_gfrckdepi_1yr - shf_gfrckdepi_base
  )
