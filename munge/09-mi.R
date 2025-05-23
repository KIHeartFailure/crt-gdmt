# Impute missing values ---------------------------------------------------

modvarstmp <- c("sos_lm_rasiarni1", "sos_lm_bbl1", "sos_lm_mra1", "sos_lm_loop1", modvars)
noimpvars <- names(rsdata)[!names(rsdata) %in% modvarstmp]

ini <- mice(rsdata, maxit = 0, print = F, m = 1)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "indexyear_cat", "scb_dispincome_cat", "shf_ntprobnp_cat")] <- "polr"
meth[noimpvars] <- ""

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imprsdata <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "rsdata"),
    .packages = "mice"
  ) %dopar% {
    mice(rsdata,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# Check if all variables have been fully imputed --------------------------

datacheck <- mice::complete(imprsdata, 1)

for (i in seq_along(modvarstmp)) {
  if (any(is.na(datacheck[, modvarstmp[i]]))) stop("Missing for imp vars")
}
for (i in seq_along(modvarstmp)) {
  if (any(is.na(datacheck[, modvarstmp[i]]))) print(paste0("Missing for ", modvarstmp[i]))
}

imprsdatahypo <- mice::filter(imprsdata, rsdata$shf_bpsys_cat == "<110" & !is.na(rsdata$shf_bpsys_cat))

imprsdatadurhf1 <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == "3-9")
imprsdatadurhf2 <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == "10-18")
imprsdatadurhf3 <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == ">=19")

imprsdatasenspop <- mice::filter(imprsdata, rsdata$senspop)
imprsdatasenscrtfu <- mice::filter(imprsdata, rsdata$senscrtfu)

imprsdatahypopp <- mice::filter(imprsdata, rsdata$shf_bpsys_cat == "<110" & !is.na(rsdata$shf_bpsys_cat) & rsdata$senscrtfu)

imprsdatadurhf1pp <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == "3-9" & rsdata$senscrtfu)
imprsdatadurhf2pp <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == "10-18" & rsdata$senscrtfu)
imprsdatadurhf3pp <- mice::filter(imprsdata, rsdata$sos_durationhf_cat == ">=19" & rsdata$senscrtfu)

imprsdatasenspoppp <- mice::filter(imprsdata, rsdata$senspop & rsdata$senscrtfu)
