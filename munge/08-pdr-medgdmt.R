lmgdmt <- haven::read_sas(here::here("data/raw-data/lmgdmtdose.sas7bdat"))

lmgdmt <- lmgdmt %>%
  filter(
    ANTAL >= 0 &
      !is.na(forpddd)
  ) %>%
  select(-atc3, -atc4, -atc5)

lmgdmt <- left_join(
  rsdata %>%
    select(lopnr, indexdtm),
  lmgdmt,
  by = "lopnr",
  relationship = "many-to-many"
)

lmgdmt <- lmgdmt %>%
  mutate(
    diff = as.numeric(EDATUM - indexdtm),
    time = case_when(
      diff >= -120 & diff <= -1 ~ 1,
      diff >= 306 & diff <= 425 ~ 2
    ),
    tmpdiff = abs(if_else(time == 1, diff, diff - 365))
  ) %>%
  filter(!is.na(time)) # either at index or fu

lmgdmt <- lmgdmt %>%
  mutate(med = case_when(
    str_detect(ATC, "^(C09A|C09B|C09C|C09D)") ~ "rasiarni",
    # str_detect(ATC, "^(C09A|C09B)") ~ "acei",
    # str_detect(ATC, "^(C09C|C09D(?!X04))") ~ "arb",
    # str_detect(ATC, "^C09DX04") ~ "arni",
    str_detect(ATC, "^C07") ~ "bbl",
    str_detect(ATC, "^C03DA") ~ "mra"
  )) %>%
  filter(!is.na(med)) # redundant, done in sas

lmgdmt2 <- lmgdmt %>%
  group_by(lopnr, med, time) %>%
  arrange(tmpdiff) %>%
  slice(1) %>%
  ungroup()

lmgdmt2 <- lmgdmt2 %>%
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
      ATC == "C07FB02" ~ NA_real_, # combination with metoprolol, only exists 20 mg
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
    strenght = ddddose * ddd,
    # for the combinations
    strenght = case_when(
      !is.na(strenght) ~ strenght,
      ATC == "C07FB02" ~ 20,
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

      # arni
      ATC == "C09DX04" & antnum %in% c(28, 56) ~ 49,
      ATC == "C09DX04" & antnum %in% c(168) ~ 97 # assumption
    ),
    # fix rounding errors
    tmpstrenght = round(strenght, 0),
    strenght = case_when(
      tmpstrenght %in% c(1, 6) ~ round(strenght, 2),
      tmpstrenght %in% c(2, 12) ~ round(strenght, 1),
      tmpstrenght %in% c(3) ~ round(strenght, 3),
      TRUE ~ tmpstrenght
    )
  )

lmgdmt2 <- lmgdmt2 %>%
  mutate(
    doserlower = tolower(DOSER),
    doserlower = str_squish(doserlower),
    doserlower = str_replace_all(doserlower, "depottablett|depottabletter|depottabl.|depottabl|tabletter|tablett|tabl.|tabl|tab.|tabt|kapsel", "tab"),
    doserlower = str_replace_all(doserlower, "ggr|gånger", "gång"),
    doserlower = str_replace_all(doserlower, "1 ½|1½|1 1/2|1,5|en och en halv", "1.5"),
    doserlower = str_replace_all(doserlower, " st | stycken ", " "),
    doserlower = str_replace_all(doserlower, " varje ", " "),
    doserlower = str_replace_all(doserlower, "^en| en ", " 1 "),
    doserlower = str_replace_all(doserlower, "^två| två ", " 2 "),
    doserlower = str_replace_all(doserlower, "\\+| \\+ ", ", "),
    doserlower = str_replace_all(doserlower, " - ", "-"),
    doserlower = str_replace_all(doserlower, "½|1/2|0,5|halv", "0.5"),
    doserlower = str_replace_all(doserlower, "samt| o ", "och"),
    doserlower = str_replace_all(doserlower, "kl.|klockan|klckan", "kl"),
    doserlower = str_replace_all(doserlower, "kl 08:00|kl 8.00|kl 08|kl 8|kl08:00|kl08.00|kl08|kl8|kl 7:00|till frukost", "på morgonen"),
    doserlower = str_replace_all(doserlower, "kl20:00|kl 20|kl 17| kl 16|till middag|till natten", "till kvällen"),
    doserlower = str_squish(doserlower),
    tabday = case_when(
      str_detect(doserlower, "3 tab på morgonen, 3 tab|3 tab 2 gång") ~ 6,
      str_detect(doserlower, "3 tab morgon och 2 på kvällen|3 tab på morgonen, 3 tab till kvällen") ~ 5,
      str_detect(doserlower, "2 tab på morgonen, 1.5 tab|1.5 tab på morgonen, 2 tab") ~ 3.5,
      str_detect(doserlower, "3 tab|2 tab på morgonen, 1 tab|1 tab 3 gång|2 på morgonen och 1|1.5 tab 2 gång|1.5 tab på morgonen och 1.5 tab|1.5 tab på morgonen, 1.5 tab|1.5 tab på morgonen och till kvällen|1.5 tab2 gång|1.5 tabmorgon och kväll") ~ 3,
      str_detect(doserlower, "2.5 tab|1.5 tab på morgonen, 1 tab|1.5 tab på morgonen,1 tab") ~ 2.5,
      str_detect(doserlower, "0.5 tab 2 gång|0.5 tab på morgonen, 0.5 tab till kvällen|0.5, 0.5 tab|0.5 tab på morgonen och till kvällen|0.5 tab på morgonen och 0.5 tab|0.5 tab morgon och 0.5 tab|0.5 tab morgon och kväll|0.5 tab på morgonen 0.5|1 tab 1 gång dagl") ~ 1,
      str_detect(doserlower, "1.5 tab|0.5 tab på morgonen och 1 tab|1-2 tab|1 tab på morgonen, 0.5 tab|1 tab på morgonen, 0.5 tab|1 tab på morgonen och 0.5 tab|0.5 tab på morgonen, 1 tab|1 tab på morgonen 0.5 tab|1 tab morgon och 0.5 tab|1 tab på morgonen och 1 0.5 tab|1 tabmorgon, 0.5 tab|1 tabpå morgonen och 0.5|1 tab på morgonen. 0.5 tab|0.5 tab på morgonen och 1 0.5 tab|0.5 tab morgon och 1 tab|1 tab på morgonen och 0.5") ~ 1.5,
      str_detect(doserlower, "1 tab på morgonen, 0.25 tab") ~ 1.25,
      str_detect(doserlower, "0.5-1 tab") ~ 0.75,
      str_detect(doserlower, "0.5 tab|1 tab varannan dag|05 tab|0.5st") ~ 0.5,
      str_detect(doserlower, "0.25 tab|0.5 varannan dag") ~ 0.25,
      str_detect(doserlower, "2 tab på morgonen, 2 tab till kvällen|2 tab 2 gång|4 tab") ~ 4,
      str_detect(doserlower, "2 tab|2 gång|1 tab på morgonen, 1 tab|på morgonen och till kvällen|1 tab på morgonen och 1 tab|1 tab morgon och kväll|1 tab på morgonen och 1|1x2|1 tab morgon, 1 tab") ~ 2,
      TRUE ~ 1
    )
  ) %>%
  select(-OTYP, -starts_with("SPKOD"), -VERKS, -tmpdiff)

lmgdmt3 <- lmgdmt2 %>%
  mutate(
    ddd = if_else(ATC == "C09DX04" & tabday > 2, 49, ddd),
    dose = strenght * tabday,
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
        ATC == "C09DX04" ~ 194
      ),
    targetdose = case_when(
      is.na(dose) ~ targetdose / 2,
      TRUE ~ dose / targetdose * 100
    ),
    targetdose_cat = factor(
      case_when(
        targetdose <= 25 ~ 1,
        targetdose <= 50 ~ 2,
        targetdose <= 75 ~ 3,
        targetdose > 75 ~ 4
      ),
      levels = 1:4,
      labels = c(
        "0-25",
        "26-50",
        "51-75",
        ">75"
      )
    )
  ) %>%
  select(lopnr, time, med, targetdose, targetdose_cat)

if (any(is.na(lmgdmt3$targetdose_cat))) stop("Missing target dose")

# koll <- lmgdmt3 %>% filter(is.na(dose))
# koll <- lmgdmt3 %>% filter(ATC == "C09DX04")
# koll <- lmgdmt3 %>% filter(med == "mra" & targetdose_cat == "0-25")

lmgdmt4 <- lmgdmt3 %>%
  pivot_wider(names_from = c("med", "time"), values_from = c("targetdose_cat", "targetdose"))

rsdata <- left_join(rsdata, lmgdmt4, by = "lopnr")
