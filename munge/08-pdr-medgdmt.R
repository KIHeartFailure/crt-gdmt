lmgdmt <- haven::read_sas(here::here("data/raw-data/lmgdmtdose_420.sas7bdat"))

lmgdmt <- lmgdmt %>%
  filter(
    # ANTAL > 0 &
    !is.na(forpddd)
  ) %>%
  select(-atc3, -atc4, -atc5) %>%
  arrange(lopnr, EDATUM, ATC) %>%
  select(-OTYP, -contains("SPKOD"), -VERKS, -ar)

lmgdmt <- left_join(
  rsdata %>%
    select(lopnr, indexdtm),
  lmgdmt,
  by = "lopnr",
  relationship = "many-to-many"
) %>%
  mutate(post = 1:n())

# remove neg incorrect prescriptions

## iterative since could not find another way...

# calcs the max number of neg disp per lopnr, ATC, ANTAL, forpddd, antnum (= number of runs needed)
lmgdmtremove <- lmgdmt %>%
  filter(ANTAL < 0) %>%
  rename(
    ANTALneg = ANTAL,
    EDATUMneg = EDATUM
  ) %>%
  mutate(ANTAL = abs(ANTALneg)) %>%
  select(lopnr, EDATUMneg, ATC, ANTAL, ANTALneg, forpddd, antnum) %>%
  group_by(lopnr, ATC, ANTAL, forpddd, antnum) %>%
  mutate(postremove = row_number()) %>%
  ungroup() %>%
  arrange(lopnr, ATC, EDATUMneg, ANTAL, forpddd, antnum, postremove)

lmgdmt2 <- lmgdmt %>%
  filter(ANTAL >= 0)

for (i in 1:max(lmgdmtremove$postremove)) {
  tmp <- inner_join(
    lmgdmt2,
    lmgdmtremove %>%
      filter(postremove == i),
    by = c("lopnr", "ATC", "ANTAL", "forpddd", "antnum")
  ) %>%
    mutate(diff = as.numeric(EDATUMneg - EDATUM)) %>%
    filter(diff >= 0) %>% # the negative disp must be same date or after the first (incorrect) disp
    group_by(lopnr, ATC, ANTAL, forpddd, antnum) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    select(post)

  tmp <- anti_join(
    lmgdmt2,
    tmp,
    by = "post"
  )

  # output to the global environment
  lmgdmt2 <<- tmp
}

lmgdmt3 <- lmgdmt2 %>%
  mutate(
    diff = as.numeric(EDATUM - indexdtm),
    time = case_when(
      diff >= -120 & diff <= -1 ~ 1,
      diff >= 306 & diff <= 425 ~ 2
    ),
    med = case_when(
      str_detect(ATC, "^(C09A|C09B|C09C|C09D)") ~ "rasiarni",
      # str_detect(ATC, "^(C09A|C09B)") ~ "acei",
      # str_detect(ATC, "^(C09C|C09D(?!X04))") ~ "arb",
      # str_detect(ATC, "^C09DX04") ~ "arni",
      str_detect(ATC, "^C07") ~ "bbl",
      str_detect(ATC, "^C03DA") ~ "mra"
    )
  ) %>%
  filter(!is.na(time) & !is.na(med)) # either at index or fu, med redundant, done in sas

# tab / day
lmgdmt4 <- lmgdmt3 %>%
  mutate(
    doserlower = tolower(DOSER),
    doserlower = str_squish(doserlower),
    doserlower = str_remove_all(doserlower, "filmdragerad|filmdrag|vattendrivande|vätskedrivande|konv"),
    doserlower = str_squish(doserlower),
    doserlower = str_replace_all(doserlower, "\\-(| )tab|depottabletter|depottablett|depottabl\\.|depotab|depottabl|depot-tab|depot- tabletter|depot tab|tabletter|tablett|tablet|tabl\\.|tabl|tab\\.|tabt|kapsel|tbl\\.|tbl|talett|kapslar|tb|talb|tqblett| ta |dosering| t ", "tab"),
    doserlower = str_remove_all(doserlower, "varje "),
    doserlower = str_replace_all(doserlower, "ggr|gånger", "gång"),
    doserlower = str_replace_all(doserlower, " \\+ |\\+", ","),
    doserlower = str_replace_all(doserlower, " - | -|- ", "-"),
    doserlower = str_replace_all(doserlower, "1 ½|1½|1 1/2|1,5| en och en halv|^(en och en halv)", "1.5"),
    doserlower = str_replace_all(doserlower, " st | stycken |^st", " "),
    doserlower = str_replace_all(doserlower, " varje ", " "),
    doserlower = str_replace_all(doserlower, "^en | en |1st", " 1 "),
    doserlower = str_replace_all(doserlower, "^två| två |2st| twice ", " 2 "),
    doserlower = str_replace_all(doserlower, "^tre | tre |3st", " 3 "),
    doserlower = str_replace_all(doserlower, "½|1/2|0,5|halv|half|1 halv|en halv", "0.5"),
    doserlower = str_replace_all(doserlower, "samt| o |0ch", "och"),
    doserlower = str_remove_all(doserlower, "\\/|per"),
    doserlower = str_replace_all(doserlower, "(?<=[:digit:])tab", " tab"),
    doserlower = str_replace_all(doserlower, "(?<=[:digit:]),(?=[:digit:])", "."),
    doserlower = str_replace_all(doserlower, "kl\\.|klockan", "kl"),
    doserlower = str_replace_all(doserlower, "kl 08:00|kl 8\\.00|kl 08|kl 8|kl08:00|kl08\\.00|kl08|kl8|kl 7:00|till frukost|på förmiddagen", "på morgonen"),
    doserlower = str_replace_all(doserlower, "på morgonen|på morgon", "morgon"),
    doserlower = str_replace_all(doserlower, "kl20:00|kl 20|kl 17| kl 16|kl 14|till middag|till natten|på kvällen|kl20|på lunch|till lunch", "till kvällen"),
    doserlower = str_replace_all(doserlower, "till kvällen", "kväll"),
    doserlower = str_replace_all(doserlower, "(?<=\\D),(?=\\D)", " och "),
    doserlower = str_remove_all(doserlower, "[:blank:]"),
    tabday = case_when(
      str_detect(doserlower, "4tabmorgonoch4|4tabmorgonochkväll") ~ 8,
      str_detect(doserlower, "4tabmorgonoch3|3tabmorgonoch4") ~ 7,
      str_detect(doserlower, "3tabmorgonoch3|3tab2gång|3tabmorgonochkväll|6tab|2tabmorgonoch4|2tab3gång") ~ 6,
      str_detect(doserlower, "3tabmorgonoch2|2tabmorgonoch3|2\\.0\\.3|2\\.5tabmorgonochkväll|2\\.1\\.2|^5tab|3\\.0\\.2|4tabmorgonoch1|2\\.5(tab|)2gång") ~ 5,
      str_detect(doserlower, "2tabmorgonoch2|2tab2gång|4tab|2\\.0\\.2|2tabmorgonochkväll|1\\.1\\.1\\.1|3\\.0\\.1|1\\.1\\.2|2x2") ~ 4,
      str_detect(doserlower, "2tabmorgonoch1\\.5|1\\.5tabmorgon(och|,|)2|3\\.5tab|2\\.5tabmorgonoch1tab") ~ 3.5,
      str_detect(doserlower, "3tab|2(tab|)morgonoch1|1tab3gång|1\\.5tab2gång|1\\.5tabmorgonoch1\\.5|1\\.5tabmorgonochkv|1tabmorgonoch1tabkvälloch1tab|1\\.1\\.1|2\\.0\\.1|1\\.0\\.2|1-2tab2gång|1-2tab1-3gång|2\\.1\\.0|2-0-1|1\\.5\\.0\\.1\\.5|1tabmorgonoch2|2tabmorgon,1tab") ~ 3,
      str_detect(doserlower, "2\\.5tab|1\\.5tabmorgon(och||,)1|1\\.0\\.1\\.5|2\\.0\\.0\\.5|1\\.5\\.0\\.1|1(tab|)tabmorgon(och||,)1\\.5|2tabmorgonoch0\\.5") ~ 2.5,
      str_detect(doserlower, "0\\.5tab2gång|0\\.5tabmorgonoch0\\.5|0\\.5\\.0\\.5tab|0\\.5tabmorgonochkväll|1tab1gång|1hjärtat|1\\.0\\\\.för hjärtat|0\\.5\\.0\\.5|0.5tabx2gång") ~ 1,
      str_detect(doserlower, "1\\.5(tab|dag)|0\\.5tabmorgon(och||,|\\.)1|1-2tab|1tabmorgonoch0\\.5|0\\.5(tab|)morgonoch1|1(tab|)morgon(och|,|)0\\.5|0\\.5\\.0\\.1|1\\.0\\.0\\.5|1\\.0\\.5tab|0\\.5-1tab2gång|1tab1-2gång|1-2tabdag|1\\.0\\.0\\.0\\.5|1\\.0\\.5\\.0|1\\.51gång|1\\.5x1") ~ 1.5,
      str_detect(doserlower, "1tabmorgonoch0\\.25|1\\.25tab|0\\.25tabmorgonoch1") ~ 1.25,
      str_detect(doserlower, "0\\.5-1tab|0\\.5tab1-2gång|1tabvarannandag,0\\.5tabvarannandag|0\\.5tabvarannandagoch1tabvarannandag") ~ 0.75,
      str_detect(doserlower, "1\\.0\\.0\\.5-1 tab") ~ 0.75,
      str_detect(doserlower, "0\\.5tab|1tabvarannan|05tab|0\\.5st|0\\.5dag|1tabmorgonvarannan|1tabkvällvarannan|1tab1gångvarannan|0\\.5tab|1varannandag|0\\.5x1|0\\.25tabmorgonochkväll") ~ 0.5,
      str_detect(doserlower, "0\\.25tab|0\\.5(tab|)varannandag") ~ 0.25,
      str_detect(doserlower, "2tab|2(1|)gång|1(tab|)morgonoch1|morgonochkväll|1x2|1\\.0\\.1|1tab2gång|1\\.0\\.1|1\\.1|2morgon|2kväll|2\\.0\\.0|2morgon|2dag|0\\.5tabmorgonoch0\\.5tabmittpådagen1tabkväll") ~ 2,
      str_detect(doserlower, "1tab|1x1|0\\.5morgonoch0\\.5|1(tab|)dag|0\\.5x2|1\\.0\\.0|0\\.1\\.0|1\\.0|0\\.5(tab|)2gång|0\\.5\\.0\\.5|0\\.5morgonochkväll|1(morgon|kväll)|0\\.5\\.0\\.0\\.5|1(var|om)(morgon|kväll|dag)|11gång") ~ 1
    )
  )

koll <- lmgdmt4 %>%
  count(is.na(tabday)) %>%
  mutate(p = n / sum(n) * 100)

koll <- lmgdmt4 %>%
  mutate(koll = str_detect(doserlower, "enligttidig") & is.na(tabday)) %>%
  count(is.na(tabday), koll) %>%
  mutate(p = n / sum(n) * 100)

# impute if missing tabsday

# koll <- lmgdmt2 %>%
#  filter(is.na(tabday)) %>%
#  count(doserlower)

impgdmt <- lmgdmt4 %>%
  group_by(ATC) %>%
  summarize(imptabday = round(mean(tabday, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(imptabday = case_when(
    is.na(imptabday) ~ 1,
    imptabday < 1 ~ imptabday,
    TRUE ~ round(imptabday, 0)
  ))

lmgdmt4 <- left_join(lmgdmt4, impgdmt, by = "ATC") %>%
  mutate(tabday = if_else(is.na(tabday), imptabday, tabday))

# strenghts
lmgdmt4 <- lmgdmt4 %>%
  mutate(
    ddddose = forpddd / antnum,
    ddd = case_when(
      ATC == "C03DA01" ~ 75,
      ATC == "C03DA04" ~ 50,
      ATC == "C07AA03" ~ 15,
      ATC == "C07AA05" ~ 160,
      ATC == "C07AA07" ~ 160,
      ATC == "C07AB02" ~ 150,
      ATC == "C07AB03" ~ 75,
      ATC == "C07AB07" ~ 10, # exists in 1.25, 2.5, 5, 10, so 2 digits
      ATC == "C07AG01" ~ 600,
      ATC == "C07AG02" ~ 37.5, # exists in 3.125, 6.25, 12.5, 25, so 3 digits
      ATC == "C07FB02" ~ NA_real_, # combination with metoprolol, exist in 5 and 10, assume 10
      ATC == "C09AA01" ~ 50,
      ATC == "C09AA02" ~ 10, # exists in 2.5, 5, 10, 20, so 1 digits
      ATC == "C09AA03" ~ 10,
      ATC == "C09AA05" ~ 2.5, # exists in 1.25, 2.5, 5, 10, 20, so 2 digits
      ATC == "C09AA06" ~ 15,
      ATC == "C09AA09" ~ 15,
      ATC == "C09BA02" ~ NA_real_, # combination with enalapril, only exists 20 mg
      ATC == "C09BA03" ~ NA_real_, # combination with lisinopril, only exists 10 mg
      ATC == "C09BA05" ~ NA_real_, # combination ramipril, most pats take 5, assume this
      ATC == "C09BA06" ~ NA_real_, # combination quinapril, does not exist in FASS
      ATC == "C09BA08" ~ NA_real_, # combination  cilazapril, does not exist in FASS
      ATC == "C09CA01" ~ 50, # exists in 12.5, 50, 100 so 1 digits
      ATC == "C09CA03" ~ 80,
      ATC == "C09CA04" ~ 150,
      ATC == "C09CA06" ~ 8,
      ATC == "C09CA07" ~ 40,
      ATC == "C09DA01" ~ NA_real_, # combination with losartan, most pats take 50, assume this
      ATC == "C09DA03" ~ NA_real_, # combination with valsartan, most pats take 80 or 160, assume 160
      ATC == "C09DA04" ~ NA_real_, # combination with irbesartan, most pats take 150, assume this
      ATC == "C09DA06" ~ NA_real_, # combination candesartan, most pats take 16, assume this
      ATC == "C09DA07" ~ NA_real_, # combination with telmisartan, only exists 80 mg
      ATC == "C09DB01" ~ NA_real_, # combination with valsartan, most pats take 80 or 160, assume 160
      ATC == "C09DX04" ~ NA_real_ # arni )
    ),
    strength = ddddose * ddd,
    # for the combinations
    strength = case_when(
      !is.na(strength) ~ strength,
      ATC == "C07FB02" ~ 100,
      ATC == "C09BA02" ~ 20,
      ATC == "C09BA03" ~ 10,
      ATC == "C09BA05" ~ 5,
      ATC == "C09BA06" ~ NA_real_,
      ATC == "C09BA08" ~ NA_real_,
      ATC == "C09DA01" ~ 50,
      ATC == "C09DA03" ~ 160,
      ATC == "C09DA04" ~ 150,
      ATC == "C09DA06" ~ 16,
      ATC == "C09DA07" ~ 80,
      ATC == "C09DB01" ~ 160,

      # arni assumption
      ATC == "C09DX04" & antnum %in% c(28, 56) ~ 51.5, # (real dose is 51 mg but to adjust to the so the target dose is correct)
      ATC == "C09DX04" & antnum %in% c(168) ~ 103
    ),
    strength = if_else(ATC == "C09DX04" & tabday > 2, 51.5, strength), # 103/2 (real dose is 51 mg but to adjust to the so the target dose is correct)
    strength = if_else(ATC == "C09DX04" & tabday > 4, 25.75, strength), # 103/5 (real dose is 26 mg but to adjust to the so the target dose is correct)
    # fix rounding errors
    tmpstrength = round(strength, 0),
    strength = case_when(
      tmpstrength %in% c(1, 6) ~ round(strength, 2),
      tmpstrength %in% c(2, 12) ~ round(strength, 1),
      tmpstrength %in% c(3) ~ round(strength, 3),
      TRUE ~ tmpstrength
    )
  )


koll <- lmgdmt4 %>%
  filter(ATC %in% c("C07AB02")) %>%
  mutate(strength = round(strength)) %>%
  count(strength) %>%
  mutate(p = n / sum(n) * 100)

# targetdoses
lmgdmt4 <- lmgdmt4 %>%
  mutate(
    dose = strength * tabday,
    targetdose =
      case_when(
        # mra
        ATC %in% c("C03DA01", "C03DA04") ~ 50,

        # bbl
        ATC == "C07AA03" ~ 15, #      	pindolol	15
        ATC == "C07AA05" ~ 160, #     	Propradolol	160
        ATC == "C07AA07" ~ 320, #     	sotalol	320
        ATC %in% c("C07AB02", "C07FB02") ~ 200, #   	Metoprolol	200
        ATC == "C07AB03" ~ 100, #   	Atenolol	100
        ATC == "C07AB07" ~ 10, #   	Bisoprolol	10
        ATC == "C07AG01" ~ 400, #   	labetalol	400
        ATC == "C07AG02" ~ 50, #   	karvedilol
        ATC == "C07AB12" ~ 10, # 	Nebivolol

        # acei
        ATC == "C09AA01" ~ 150, # 	Kaptopril	150
        ATC %in% c("C09AA02", "C09BA02") ~ 40, # 	Enalapril	40
        ATC %in% c("C09AA03", "C09BA03") ~ 35, #  	Lisinopril	35
        ATC %in% c("C09AA05", "C09BA05") ~ 10, #  	Ramipril	10
        ATC %in% c("C09AA06", "C09BA06") ~ 40, #  	Quinapril	40
        ATC == "C09AA09" ~ 40, #  	fosinopril	40

        # arb
        ATC %in% c("C09CA01", "C09DA01") ~ 150, #    	losartan 	150
        ATC %in% c("C09CA03", "C09DA03", "C09DB01") ~ 320, #    	Valsartan	320
        ATC %in% c("C09CA04", "C09DA04") ~ 300, #     	irbesartan	300
        ATC %in% c("C09CA06", "C09DA06") ~ 32, # 	kandesartan  	32
        ATC %in% c("C09CA07", "C09DA07") ~ 80, #    	telmisartan 	80,

        # arni
        ATC == "C09DX04" ~ 206
      ),
    targetdose = case_when(
      is.na(dose) ~ targetdose / 2,
      TRUE ~ dose / targetdose * 100
    )
  )

if (any(is.na(lmgdmt4$targetdose))) stop("Missing target dose")

# 1. mean targetdose if on same day and same ATC and same strength
# 2. summarize targetdose if on same day if same ATC and NOT same strength
# 3. mean targetdose if on same day and NOT same ATC

lmgdmt5 <- lmgdmt4 %>%
  group_by(lopnr, indexdtm, EDATUM, time, ATC, med, strength) %>%
  summarise(targetdose = mean(targetdose, na.rm = T)) %>%
  ungroup() %>%
  group_by(lopnr, indexdtm, EDATUM, time, ATC, med) %>%
  summarise(targetdose = sum(targetdose, na.rm = T)) %>%
  ungroup() %>%
  group_by(lopnr, indexdtm, EDATUM, time, med) %>%
  summarise(targetdose = mean(targetdose, na.rm = T)) %>%
  ungroup()

lmgdmt6 <- lmgdmt5 %>%
  mutate(
    diff = as.numeric(EDATUM - indexdtm),
    tmpdiff = abs(if_else(time == 1, diff, diff - 365))
  ) %>%
  group_by(lopnr, med, time) %>%
  arrange(tmpdiff) %>%
  slice(1) %>%
  ungroup()

# add info on arni

lmarni <- lmgdmt4 %>%
  filter(str_detect(ATC, "^C09DX04")) %>%
  select(lopnr, EDATUM) %>%
  mutate(arni = 1) %>%
  distinct()

lmgdmt7 <- left_join(
  lmgdmt6,
  lmarni,
  by = c("lopnr", "EDATUM")
) %>%
  select(lopnr, time, med, targetdose, arni)

lmgdmt8 <- lmgdmt7 %>%
  pivot_wider(names_from = c("med", "time"), values_from = c("targetdose", "arni")) %>%
  select(-arni_bbl_1, -arni_bbl_2, -arni_mra_2, -arni_mra_1)

colnames(lmgdmt8) <- str_remove_all(colnames(lmgdmt8), "targetdose_")

dosefunc <- function(med) {
  med <- factor(
    case_when(
      is.na(med) ~ 0,
      med < 25 ~ 1,
      med < 50 ~ 2,
      med < 75 ~ 3,
      med >= 75 ~ 4
    ),
    levels = 0:4,
    labels = c(
      "Not treated",
      "<25",
      "25-49",
      "50-74",
      ">=75"
    )
  )
}

dosefuncmra <- function(med) {
  med <- factor(
    case_when(
      is.na(med) ~ 0,
      med < 50 ~ 1,
      med < 75 ~ 2,
      med >= 75 ~ 3
    ),
    levels = 0:3,
    labels = c(
      "Not treated",
      "<50",
      "50-74",
      ">=75"
    )
  )
}
dosefunc2 <- function(med) {
  med <- factor(
    case_when(
      is.na(med) ~ 0,
      med < 50 ~ 1,
      med >= 50 ~ 2
    ),
    levels = 0:2,
    labels = c(
      "Not treated",
      "<50",
      ">=50"
    )
  )
}

rsdata <- left_join(rsdata, lmgdmt8, by = "lopnr")

rsdata <- rsdata %>%
  mutate(
    sos_lm_bbl1 = dosefunc(bbl_1),
    sos_lm_bbl2 = dosefunc(bbl_2),
    sos_lm_rasiarni1 = dosefunc(rasiarni_1),
    sos_lm_rasiarni2 = dosefunc(rasiarni_2),
    sos_lm_mra1 = dosefuncmra(mra_1),
    sos_lm_mra2 = dosefuncmra(mra_2),
    sos_lm_bbl1_3 = dosefunc2(bbl_1),
    sos_lm_rasiarni1_3 = dosefunc2(rasiarni_1),
    sos_lm_mra1_3 = dosefunc2(mra_1)
  )

checkcombdoses <- lmgdmt4 %>%
  mutate(
    diff = as.numeric(EDATUM - indexdtm),
    tmpdiff = abs(if_else(time == 1, diff, diff - 365)),
    arni = str_detect(ATC, "^C09DX04"),
    othercomb = ATC %in% c(
      "C07FB02", "C09BA02", "C09BA03", "C09BA05", "C09BA06", "C09BA08", "C09DA01",
      "C09DA03",
      "C09DA04",
      "C09DA06",
      "C09DA07",
      "C09DB01",
      "C07FB02",
      "C09BA02",
      "C09BA03",
      "C09BA05",
      "C09BA06",
      "C09BA08",
      "C09DA01",
      "C09DA03",
      "C09DA04",
      "C09DA06",
      "C09DA07",
      "C09DB01"
    )
  ) %>%
  group_by(lopnr, med, time) %>%
  arrange(tmpdiff) %>%
  slice(1) %>%
  ungroup()



# dubbelokoll
# load(here(shfdbpath, "data", datadate, "20221012/lmswedehf.RData"))
#
# lmcrt <- lmswedehf %>%
#   filter(
#     ANTAL >= 0 &
#       !is.na(forpddd)
#   )
#
# lmcrt <- left_join(
#   rsdata %>%
#     select(lopnr, indexdtm),
#   lmcrt,
#   by = "lopnr",
#   relationship = "many-to-many"
# )
#
# lmsel <- lmcrt %>%
#   mutate(diff = as.numeric(EDATUM - indexdtm)) %>%
#   filter(diff >= -120 & diff <= -1) %>%
#   select(lopnr, indexdtm, EDATUM, ATC)
#
# rm(lmswedehf)
# gc()
#
# rsdatakoll <- create_medvar(
#   atc = "^(C07)", medname = "bblkoll",
#   cohortdata = rsdata,
#   meddata = lmsel,
#   id = "lopnr",
#   valsclass = "fac",
#   metatime = "-120-0"
# )
#
