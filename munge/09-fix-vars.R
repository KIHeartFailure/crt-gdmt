rsdata <- rsdata %>%
  mutate(
    crt = factor(crt, levels = 0:1, labels = c("No CRT", "CRT")),
    diff_crt_shf_cat = factor(
      case_when(
        diff_crt_shf <= -182 ~ 1,
        diff_crt_shf <= -91 ~ 2,
        diff_crt_shf <= -31 ~ 3,
        diff_crt_shf <= 0 ~ 4,
        diff_crt_shf > 0 ~ 5
      ),
      levels = 1:5,
      labels = c("365-182 prior CRT", "181-91 prior CRT", "90-31 prior CRT", "30-0 prior CRT", "1-30 after CRT")
    ),
    absdiff_crt_shf = if_else(crt == "CRT", abs(diff_crt_shf), NA_real_),
    indexyear_cat = case_when(
      indexyear <= 2015 ~ "2009-2015",
      indexyear <= 2018 ~ "2016-2018",
      indexyear <= 2021 ~ "2019-2021"
    ),
    sos_prevhfh1yr = factor(if_else(sos_timeprevhosphf >= 365 & !is.na(sos_timeprevhosphf), 1, 0), levels = 0:1, labels = c("No", "Yes")),
    shf_bpsys_cat = factor(
      case_when(
        is.na(shf_bpsys) ~ NA_real_,
        shf_bpsys < 140 ~ 1,
        shf_bpsys >= 140 ~ 2
      ),
      levels = 1:2,
      labels = c("<140", ">=140")
    ),
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
    mradiff = case_when(
      sos_lm_mra1 == "Not treated" & sos_lm_mra2 == "Not treated" ~ 0,
      sos_lm_mra1 == "Not treated" ~ 1,
      sos_lm_mra2 == "Not treated" ~ -1,
      round(mra_1) == round(mra_2) ~ 0,
      round(mra_1) < round(mra_2) ~ 1,
      round(mra_1) > round(mra_2) ~ -1
    ),
    bbldiff = case_when(
      sos_lm_bbl1 == "Not treated" & sos_lm_bbl2 == "Not treated" ~ 0,
      sos_lm_bbl1 == "Not treated" ~ 1,
      sos_lm_bbl2 == "Not treated" ~ -1,
      round(bbl_1) == round(bbl_2) ~ 0,
      round(bbl_1) < round(bbl_2) ~ 1,
      round(bbl_1) > round(bbl_2) ~ -1
    ),
    rasiarnidiff = case_when(
      sos_lm_rasiarni1 == "Not treated" & sos_lm_rasiarni2 == "Not treated" ~ 0,
      sos_lm_rasiarni1 == "Not treated" ~ 1,
      sos_lm_rasiarni2 == "Not treated" ~ -1,
      is.na(arni_rasiarni_1) & !is.na(arni_rasiarni_2) ~ 1,
      !is.na(arni_rasiarni_1) & is.na(arni_rasiarni_2) ~ -1,
      round(rasiarni_1) == round(rasiarni_2) ~ 0,
      round(rasiarni_1) < round(rasiarni_2) ~ 1,
      round(rasiarni_1) > round(rasiarni_2) ~ -1
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

    # ex arni
    rasiarnidiff_exarni = case_when(
      !is.na(arni_rasiarni_1) | !is.na(arni_rasiarni_2) ~ NA_real_,
      sos_lm_rasiarni1 == "Not treated" & sos_lm_rasiarni2 == "Not treated" ~ 0,
      sos_lm_rasiarni1 == "Not treated" ~ 1,
      sos_lm_rasiarni2 == "Not treated" ~ -1,
      round(rasiarni_1) == round(rasiarni_2) ~ 0,
      round(rasiarni_1) < round(rasiarni_2) ~ 1,
      round(rasiarni_1) > round(rasiarni_2) ~ -1
    ),
    gdmtdiff_exarni = mradiff + bbldiff + rasiarnidiff_exarni,
    gdmtdiff_cat_exarni = factor(
      case_when(
        is.na(gdmtdiff_exarni) ~ NA_real_,
        gdmtdiff_exarni < 0 ~ -1,
        gdmtdiff_exarni == 0 ~ 0,
        gdmtdiff_exarni > 0 ~ 1
      ),
      levels = -1:1, labels = c("Decrease", "No change", "Increase")
    ),
    mradiff = factor(mradiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    bbldiff = factor(bbldiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    rasiarnidiff = factor(rasiarnidiff, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    gdmtdiff_cat2 = fct_collapse(gdmtdiff_cat, "Decrease/No change" = c("Decrease", "No change")),
    mradiff2 = fct_collapse(mradiff, "Decrease/No change" = c("Decrease", "No change")),
    bbldiff2 = fct_collapse(bbldiff, "Decrease/No change" = c("Decrease", "No change")),
    rasiarnidiff2 = fct_collapse(rasiarnidiff, "Decrease/No change" = c("Decrease", "No change")),
    rasiarnidiff_exarni = factor(rasiarnidiff_exarni, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    gdmtdiff_cat2_exarni = fct_collapse(gdmtdiff_cat_exarni, "Decrease/No change" = c("Decrease", "No change")),
    rasiarnidiff2_exarni = fct_collapse(rasiarnidiff_exarni, "Decrease/No change" = c("Decrease", "No change"))
    #
    # # using categorical levels
    # mradiff_alt = case_when(
    #   as.numeric(sos_lm_mra1) == as.numeric(sos_lm_mra2) ~ 0,
    #   as.numeric(sos_lm_mra1) < as.numeric(sos_lm_mra2) ~ 1,
    #   as.numeric(sos_lm_mra1) > as.numeric(sos_lm_mra2) ~ -1,
    # ),
    # bbldiff_alt = case_when(
    #   as.numeric(sos_lm_bbl1) == as.numeric(sos_lm_bbl2) ~ 0,
    #   as.numeric(sos_lm_bbl1) < as.numeric(sos_lm_bbl2) ~ 1,
    #   as.numeric(sos_lm_bbl1) > as.numeric(sos_lm_bbl2) ~ -1,
    # ),
    # rasiarnidiff_alt = case_when(
    #   as.numeric(sos_lm_rasiarni1) == as.numeric(sos_lm_rasiarni2) ~ 0,
    #   as.numeric(sos_lm_rasiarni1) < as.numeric(sos_lm_rasiarni2) ~ 1,
    #   as.numeric(sos_lm_rasiarni1) > as.numeric(sos_lm_rasiarni2) ~ -1,
    # ),
    # gdmtdiff_alt = mradiff_alt + bbldiff_alt + rasiarnidiff_alt,
    # gdmtdiff_cat_alt = factor(
    #   case_when(
    #     gdmtdiff < 0 ~ -1,
    #     gdmtdiff == 0 ~ 0,
    #     gdmtdiff > 0 ~ 1
    #   ),
    #   levels = -1:1, labels = c("Decrease", "No change", "Increase")
    # ),
    # mradiff_alt = factor(mradiff_alt, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    # bbldiff_alt = factor(bbldiff_alt, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    # rasiarnidiff_alt = factor(rasiarnidiff_alt, levels = -1:1, labels = c("Decrease", "No change", "Increase")),
    # gdmtdiff_cat2_alt = fct_collapse(gdmtdiff_cat_alt, "Decrease/No change" = c("Decrease", "No change")),
    # mradiff2_alt = fct_collapse(mradiff_alt, "Decrease/No change" = c("Decrease", "No change")),
    # bbldiff2_alt = fct_collapse(bbldiff_alt, "Decrease/No change" = c("Decrease", "No change")),
    # rasiarnidiff2_alt = fct_collapse(rasiarnidiff_alt, "Decrease/No change" = c("Decrease", "No change"))
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
  mutate(across(where(is.character), as.factor))
