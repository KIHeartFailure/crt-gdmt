rsdata <- rsdata %>%
  mutate(
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
    )
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
