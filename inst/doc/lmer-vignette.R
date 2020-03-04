## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
library(lmerTest)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)

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


## ---- fig.width = 7, fig.height = 10------------------------------------------

md <- modelDiagnostics(m, ev.perc = .001)

plot(md, ask = FALSE, ncol = 2, nrow = 3)


## -----------------------------------------------------------------------------

mvextreme <- subset(md$extremeValues,
  EffectType == "Multivariate Random Effect UserID")

head(mvextreme)

unique(mvextreme$UserID)


## ---- fig.width = 7, fig.height = 10------------------------------------------

m2 <- update(m, data = subset(aces_daily,
  UserID %!in% unique(mvextreme$UserID)))

md2 <- modelDiagnostics(m2, ev.perc = .001)

plot(md2, ask = FALSE, ncol = 2, nrow = 3)

mvextreme2 <- subset(md2$extremeValues,
  EffectType == "Multivariate Random Effect UserID")

unique(mvextreme2$UserID)


## ---- fig.width = 7, fig.height = 10------------------------------------------

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


