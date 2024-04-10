rsdata <- rsdata %>%
  mutate(
    crt = factor(crt, levels = 0:1, labels = c("Control", "CRT")),
    diff_crt_shf_cat = factor(
      case_when(
        crt == "Control" ~ 0,
        diff_crt_shf <= -182 ~ 1,
        diff_crt_shf <= 0 ~ 2
      ),
      levels = 0:2,
      labels = c("Control", "365-182", "183-0")
    ),
    indexyear_cat = case_when(
      indexyear <= 2015 ~ "2009-2015",
      indexyear <= 2018 ~ "2016-2018",
      indexyear <= 2021 ~ "2019-2021"
    ),
    sos_prevhfh1yr = factor(if_else(sos_timeprevhosphf >= 365 & !is.na(sos_timeprevhosphf), 1, 0), levels = 0:1, labels = c("No", "Yes")),
    sos_com_charlsonci_cat = factor(
      case_when(
        sos_com_charlsonci <= 1 ~ 1,
        sos_com_charlsonci <= 3 ~ 2,
        sos_com_charlsonci <= 7 ~ 3,
        sos_com_charlsonci >= 8 ~ 4
      ),
      levels = 1:4,
      labels = c(
        "0-1",
        "2-3",
        "4-7",
        ">=8"
      )
    ),
    sos_lm_mra1 = factor(if_else(is.na(targetdose_mra_1), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    sos_lm_bbl1 = factor(if_else(is.na(targetdose_bbl_1), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    sos_lm_rasiarni1 = factor(if_else(is.na(targetdose_rasiarni_1), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    sos_lm_mra2 = factor(if_else(is.na(targetdose_mra_2), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    sos_lm_bbl2 = factor(if_else(is.na(targetdose_bbl_2), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    sos_lm_rasiarni2 = factor(if_else(is.na(targetdose_rasiarni_2), 0, 1), levels = 0:1, labels = c("No", "Yes")),
    mradiff = case_when(
      sos_lm_mra1 == "No" & sos_lm_mra2 == "No" ~ 0,
      sos_lm_mra1 == "No" & sos_lm_mra2 == "Yes" ~ 1,
      sos_lm_mra1 == "Yes" & sos_lm_mra2 == "No" ~ -1,
      round(targetdose_mra_1) == round(targetdose_mra_2) ~ 0,
      round(targetdose_mra_1) < round(targetdose_mra_2) ~ 1,
      round(targetdose_mra_1) > round(targetdose_mra_2) ~ -1
    ),
    bbldiff = case_when(
      sos_lm_bbl1 == "No" & sos_lm_bbl2 == "No" ~ 0,
      sos_lm_bbl1 == "No" & sos_lm_bbl2 == "Yes" ~ 1,
      sos_lm_bbl1 == "Yes" & sos_lm_bbl2 == "No" ~ -1,
      round(targetdose_bbl_1) == round(targetdose_bbl_2) ~ 0,
      round(targetdose_bbl_1) < round(targetdose_bbl_2) ~ 1,
      round(targetdose_bbl_1) > round(targetdose_bbl_2) ~ -1
    ),
    rasiarnidiff = case_when(
      sos_lm_rasiarni1 == "No" & sos_lm_rasiarni2 == "No" ~ 0,
      sos_lm_rasiarni1 == "No" & sos_lm_rasiarni2 == "Yes" ~ 1,
      sos_lm_rasiarni1 == "Yes" & sos_lm_rasiarni2 == "No" ~ -1,
      round(targetdose_rasiarni_1) == round(targetdose_rasiarni_2) ~ 0,
      round(targetdose_rasiarni_1) < round(targetdose_rasiarni_2) ~ 1,
      round(targetdose_rasiarni_1) > round(targetdose_rasiarni_2) ~ -1
    ),
    gdmtdiff = mradiff + bbldiff + rasiarnidiff,
    gdmtdiff_cat = factor(
      case_when(
        gdmtdiff < 0 ~ -1,
        gdmtdiff == 0 ~ 0,
        gdmtdiff > 0 ~ 1
      ),
      levels = -1:1, labels = c("Decrease", "No change", "Increase")
    ),
    mradiff = factor(mradiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    bbldiff = factor(bbldiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    rasiarnidiff = factor(rasiarnidiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    gdmtdiff_cat2 = fct_collapse(gdmtdiff_cat, "Decrease/No change" = c("Decrease", "No change")),
    mradiff2 = fct_collapse(mradiff, "Decrease/No change" = c("Decrease", "No change")),
    bbldiff2 = fct_collapse(bbldiff, "Decrease/No change" = c("Decrease", "No change")),
    rasiarnidiff2 = fct_collapse(rasiarnidiff, "Decrease/No change" = c("Decrease", "No change"))
  )

# income
inc <- rsdata %>%
  reframe(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = indexyear) %>%
  unnest(cols = c(incsum)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "indexyear"
) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < `33%` ~ 1,
        scb_dispincome < `66%` ~ 2,
        scb_dispincome >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within year", "2nd tertile within year", "3rd tertile within year")
    )
  ) %>%
  select(-`33%`, -`66%`)

rsdata <- rsdata %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    sos_deathcause = as.character(sos_deathcause)
  )
