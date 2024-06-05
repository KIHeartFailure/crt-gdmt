arniimp <- rsdata %>%
  filter(!is.na(arni_rasiarni_1) & !is.na(arni_rasiarni_2) & rasiarni_1 < rasiarni_2) %>%
  mutate(rasiarnidiff = rasiarni_2 - rasiarni_1) %>%
  summarise(med = median(rasiarnidiff)) %>%
  pull(med)

medfunc <- function(x) {
  out <- factor(case_when(
    x == 0 ~ 0,
    x > 0 ~ 1,
    x < 0 ~ -1
  ), levels = -1:1, labels = c("Decrease", "Stable", "Increase"))
}

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
      indexyear <= 2013 ~ "2009-2013",
      indexyear <= 2018 ~ "2014-2018",
      indexyear <= 2023 ~ "2019-2023"
    ),
    sos_prevhfh1yr = factor(if_else(sos_timeprevhosphf <= 365 & !is.na(sos_timeprevhosphf), 1, 0), levels = 0:1, labels = c("No", "Yes")),
    sos_durationhf_cat = factor(
      case_when(
        sos_durationhf <= 9 * 30.5 ~ 1,
        sos_durationhf <= 18 * 30.5 ~ 2,
        sos_durationhf > 18 * 30.5 ~ 3
      ),
      levels = 1:3, labels = c("3-9", "10-18", ">=19")
    ),
    shf_bpsys_cat = factor(
      case_when(
        is.na(shf_bpsys) ~ NA_real_,
        shf_bpsys < 110 ~ 1,
        shf_bpsys >= 110 ~ 2
      ),
      levels = 1:2,
      labels = c("<110", ">=110")
    ),
    shf_heartrate_cat = case_when(
      shf_heartrate <= 60 ~ "<=60",
      shf_heartrate > 60 ~ ">60"
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
        "1",
        "2-3",
        "4-7",
        ">=8"
      )
    ),
    loopdiff = loop_2 - loop_1,
    loopdiff_cat = medfunc(loopdiff),
    bbldiff = bbl_2 - bbl_1,
    bbldiff_cat = medfunc(bbldiff),
    mradiff = mra_2 - mra_1,
    mradiff_cat = medfunc(mradiff),
    rasiarnidiff = rasiarni_2 - rasiarni_1,
    rasiarnidiff_cat = factor(case_when(
      is.na(arni_rasiarni_1) & !is.na(arni_rasiarni_2) ~ 1,
      !is.na(arni_rasiarni_1) & is.na(arni_rasiarni_2) ~ -1,
      rasiarni_1 == rasiarni_2 ~ 0,
      rasiarni_1 < rasiarni_2 ~ 1,
      rasiarni_1 > rasiarni_2 ~ -1
    ), levels = -1:1, labels = c("Decrease", "Stable", "Increase")),
    # ex arni
    rasiarnidiff_exarni_cat = factor(case_when(
      !is.na(arni_rasiarni_1) | !is.na(arni_rasiarni_2) ~ NA_real_,
      rasiarni_1 == rasiarni_2 ~ 0,
      rasiarni_1 < rasiarni_2 ~ 1,
      rasiarni_1 > rasiarni_2 ~ -1
    ), levels = -1:1, labels = c("Decrease", "Stable", "Increase")),
    # If switch from acei/arb to arni median change for patients increasing on ace/arb is imputed
    rasiarnidiff = if_else(is.na(arni_rasiarni_1) & !is.na(arni_rasiarni_2), arniimp, rasiarnidiff),
    rasiarnidiff_exarni = if_else(is.na(arni_rasiarni_1) & !is.na(arni_rasiarni_2), NA_real_, rasiarnidiff),
    mradiff_cat2 = fct_collapse(mradiff_cat, "Decrease/Stable" = c("Decrease", "Stable")),
    bbldiff_cat2 = fct_collapse(bbldiff_cat, "Decrease/Stable" = c("Decrease", "Stable")),
    rasiarnidiff_cat2 = fct_collapse(rasiarnidiff_cat, "Decrease/Stable" = c("Decrease", "Stable")),
    rasiarnidiff_exarni_cat2 = fct_collapse(rasiarnidiff_exarni_cat, "Decrease/Stable" = c("Decrease", "Stable")),
    mradiff_inccat2 = fct_collapse(mradiff_cat, "Increase/Stable" = c("Increase", "Stable")),
    bbldiff_inccat2 = fct_collapse(bbldiff_cat, "Increase/Stable" = c("Increase", "Stable")),
    loopdiff_inccat2 = fct_collapse(loopdiff_cat, "Increase/Stable" = c("Increase", "Stable")),
    rasiarnidiff_inccat2 = fct_collapse(rasiarnidiff_cat, "Increase/Stable" = c("Increase", "Stable")),
    rasiarnidiff_exarni_inccat2 = fct_collapse(rasiarnidiff_exarni_cat, "Increase/Stable" = c("Increase", "Stable"))
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


# ntprobnp

nt <- rsdata %>%
  reframe(ntmed = list(enframe(quantile(shf_ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntmed)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- rsdata %>%
  mutate(
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < nt$`33%` ~ 1,
        shf_ntprobnp < nt$`66%` ~ 2,
        shf_ntprobnp >= nt$`66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile", "2nd tertile", "3rd tertile")
    )
  )

rsdata <- rsdata %>%
  mutate(across(where(is.character), as.factor))
