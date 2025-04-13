## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----setup--------------------------------------------------------------------

## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
library(lmerTest)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)
library(data.table)

## load some sample data for examples
data(aces_daily, package = "JWileymisc")


## -----------------------------------------------------------------------------

## overall structure
str(aces_daily, nchar.max = 30)


## -----------------------------------------------------------------------------

## how many unique IDs (people) are there?
length(unique(aces_daily$UserID))

## how many not missing observations of negative affect are there?
sum(!is.na(aces_daily$NegAff))

## how many not missing observations of stress are there?
sum(!is.na(aces_daily$STRESS))


## -----------------------------------------------------------------------------

summary(aces_daily$NegAff)

summary(aces_daily$STRESS)


## -----------------------------------------------------------------------------

iccMixed(
  dv = "NegAff",
  id = "UserID",
  data = aces_daily)

iccMixed("STRESS", "UserID", aces_daily)


## -----------------------------------------------------------------------------

tmp <- meanDecompose(NegAff ~ UserID, data = aces_daily)
str(tmp, nchar.max = 30)

plot(testDistribution(tmp[["NegAff by UserID"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Negative Affect")

plot(testDistribution(tmp[["NegAff by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Negative Affect")


## -----------------------------------------------------------------------------

tmp <- meanDecompose(STRESS ~ UserID, data = aces_daily)

plot(testDistribution(tmp[["STRESS by UserID"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person STRESS")

plot(testDistribution(tmp[["STRESS by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person STRESS")


## -----------------------------------------------------------------------------

strictControl <- lmerControl(optCtrl = list(
   algorithm = "NLOPT_LN_NELDERMEAD",
   xtol_abs = 1e-12,
   ftol_abs = 1e-12))

m <- lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
          data = aces_daily, control = strictControl)


## ----fig.width = 7, fig.height = 10-------------------------------------------

md <- modelDiagnostics(m, ev.perc = .001)

plot(md, ask = FALSE, ncol = 2, nrow = 3)


## -----------------------------------------------------------------------------

mvextreme <- subset(md$extremeValues,
  EffectType == "Multivariate Random Effect UserID")

head(mvextreme)

unique(mvextreme$UserID)


## ----fig.width = 7, fig.height = 10-------------------------------------------

m2 <- update(m, data = subset(aces_daily,
  UserID %!in% unique(mvextreme$UserID)))

md2 <- modelDiagnostics(m2, ev.perc = .001)

plot(md2, ask = FALSE, ncol = 2, nrow = 3)

mvextreme2 <- subset(md2$extremeValues,
  EffectType == "Multivariate Random Effect UserID")

unique(mvextreme2$UserID)


## ----fig.width = 7, fig.height = 10-------------------------------------------

m3 <- update(m, data = subset(aces_daily,
  UserID %!in% c(unique(mvextreme$UserID), unique(mvextreme2$UserID))))

md3 <- modelDiagnostics(m3, ev.perc = .001)

plot(md3, ask = FALSE, ncol = 2, nrow = 3)


## -----------------------------------------------------------------------------

modelPerformance(m3)


## -----------------------------------------------------------------------------

summary(m3)


## -----------------------------------------------------------------------------

mt3 <- modelTest(m3)

names(mt3) ## list of all tables available

APAStyler(mt3)


## -----------------------------------------------------------------------------

APAStyler(mt3,
          format = list(
            FixedEffects = "%s, %s (%s; %s)",
            RandomEffects = c("%s", "%s (%s, %s)"),
            EffectSizes = "%s, %s; %s"),
          digits = 3,
          pcontrol = list(digits = 3, stars = FALSE,
                          includeP = TRUE, includeSign = TRUE,
                          dropLeadingZero = TRUE))


## -----------------------------------------------------------------------------
## run modelTest() on the original model, m
mt <- modelTest(m)

APAStyler(list(Original = mt, `Outliers Removed` = mt3))

## -----------------------------------------------------------------------------
d <- as.data.table(aces_daily)[!is.na(SES_1) & !is.na(BornAUS)]
d[, SEScat := factor(SES_1)]
d[, BornAUScat := factor(BornAUS)]

m.noint <- lmer(PosAff ~ BornAUScat + SEScat + (1 | UserID), data = d)

## ----echo = FALSE, results = "asis"-------------------------------------------
knitr::kable(fixef(m.noint), caption = "no interaction model fixed effects")

## -----------------------------------------------------------------------------
m.nointdrop <- update(m.noint, . ~ . - SEScat)

## ----echo = FALSE, results = "asis"-------------------------------------------
knitr::kable(
  fixef(m.nointdrop),
  caption = "no interaction model fixed effects after dropping a predictor")

## ----results = "asis"---------------------------------------------------------
knitr::kable(
  t(modelCompare(m.nointdrop, m.noint)$Comparison),
  caption = "model comparison")

## ----results = "asis"---------------------------------------------------------
knitr::kable(APAStyler(modelTest(m.noint)))

## -----------------------------------------------------------------------------
m.int <- lmer(PosAff ~ BornAUScat * SEScat + (1 | UserID), data = d)

## ----echo = FALSE, results = "asis"-------------------------------------------
knitr::kable(fixef(m.int), caption = "interaction model fixed effects")

## -----------------------------------------------------------------------------
m.intdrop <- update(m.int, . ~ . - SEScat)

## ----echo = FALSE, results = "asis"-------------------------------------------
knitr::kable(
  fixef(m.intdrop),
  caption = "interaction model fixed effects after dropping a predictor")

## ----eval = FALSE-------------------------------------------------------------
# modelCompare(m.intdrop, m.int)

## -----------------------------------------------------------------------------
logLik(m.int)
logLik(m.intdrop)

## ----results = "asis"---------------------------------------------------------
knitr::kable(APAStyler(modelTest(m.int)))

## -----------------------------------------------------------------------------
## manually dummy code
d[, SEScat5 := fifelse(SES_1 == 5, 1, 0)]
d[, SEScat6 := fifelse(SES_1 == 6, 1, 0)]
d[, SEScat7 := fifelse(SES_1 == 7, 1, 0)]
d[, SEScat8 := fifelse(SES_1 == 8, 1, 0)]

## interaction model
m.intman <- lmer(PosAff ~ BornAUS * (SEScat5 + SEScat6 + SEScat7 + SEScat8) +
                   (1 | UserID), data = d)

## drop just the simple main effects of SEScat
m.intmandrop <- update(m.intman, . ~ . - SEScat5 - SEScat6 - SEScat7 - SEScat8)


## ----results = "asis"---------------------------------------------------------
knitr::kable(
  t(modelCompare(m.intmandrop, m.intman)$Comparison),
  caption = "manual model comparison")

## ----results = "asis"---------------------------------------------------------
knitr::kable(APAStyler(modelTest(m.intman)))

## ----results = "asis"---------------------------------------------------------
m.cint <- lmer(PosAff ~ STRESS * NegAff + (1 | UserID), data = aces_daily)

knitr::kable(APAStyler(modelTest(m.cint)))

