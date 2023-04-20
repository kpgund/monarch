## generalized linear models to consider for backyard garden sampling - 

## load in needed libraries
library(tidyverse)
library(glmmTMB)
library(sjPlot)

# bringing in data
backyard<-read.csv("https://raw.githubusercontent.com/lmgermeroth/Garden-sampling-SU21/main/backyard_3.9.23.csv")

# data cleaning: "Phenology" versus "Phenology " 
## see if there are Phenology with space after flowering
backyard %>% 
  dplyr::filter(
    PHENOLOGY == "FLOWERING "
  ) ## there are 8
## subset which are "FLOWERING " and put in "FLOWERING"
backyard[which(backyard$PHENOLOGY == "FLOWERING "), "PHENOLOGY"] <- "FLOWERING"

# check to see if it is fixed
backyard  %>% 
  dplyr::filter(
    PHENOLOGY == "FLOWERING "
  )
unique(backyard$PHENOLOGY) ## YAY

backyard <- backyard  %>% 
  dplyr::mutate(
    PHENOLOGY = as.factor(PHENOLOGY),
    LOCATION = as.factor(LOCATION),
    FLOW_PLANT_CAT = as.factor(FLOW_PLANT_CAT),
    FOREST = as.factor(FOREST),
    SPECIES = as.factor(SPECIES),
    WEEK = as.factor(WEEK),
    HEIGHT.std = as.numeric(scale(HEIGHT)),
    PRED_WOUT_ANT_TOT.std = as.numeric(scale(PRED_WOUT_ANT_TOT)),
    ANT_TOT.std = as.numeric(scale(ANT_TOT)),
    APH_NER.std = as.numeric(scale(APH_NER)),
    MYZ_ASC.std = as.numeric(scale(MYZ_ASC))
  )

# model process:
# 1) global model - including all terms I think would be important
# 2) community model - preds, ants, aphids, etc
# 3) host plant model - location, species (in case where there are not just one sp)

# putting location as a random effect, making these mixed models now

## Reconsidering interactive effects ##
## lg global model (1|LOCATION) <- random intercept for location
## if you want to see an interaction between LOCATION and MYZ_ASC maybe add a random effect (MYZ_ASC | LOCATION) # this would change the slopes for MYZ based on Location

## GENERAL FLOW OF MODELS:
# first is GLOBAL: all terms I think could be important
# second is COMMUNITY: community interactions I think could be important, smaller
# third is ENVIRONMENT: environmental factors like location, height, week, species when applicable


# what effects EGG ABUNDANCE across ALL THREE SPECIES OF MILKWEED?
# EGGS - ALL SPECIES ----
## global model 
egg.all.global <- EGG_TOT ~ SPECIES + (HEIGHT.std + PRED_WOUT_ANT_TOT.std + ANT_TOT.std + APH_NER.std + MYZ_ASC.std)*SPECIES + (1|LOCATION) + (1|WEEK)
egg.all.global.glmm <- glmmTMB(
  egg.all.global,
  data = backyard,
  family = poisson
)  
summary(egg.all.global.glmm) 
# takes a minute to run don't worry
# with SPECIESINCARNATA at the intercept


egg.all.comm <- EGG_TOT ~ (PRED_WOUT_ANT_TOT.std + ANT_TOT.std + APH_NER.std + MYZ_ASC.std)*SPECIES + (1|LOCATION) + (1|WEEK)
egg.all.comm.glmm<-glmmTMB(
  egg.all.comm,
  data=backyard,
  family=poisson
)
summary(egg.all.comm.glmm) 


egg.all.env <- EGG_TOT ~ SPECIES*HEIGHT + (1|LOCATION) + (1|WEEK)
egg.all.env.glmm <- glmmTMB(
  egg.all.env,
  data=backyard,
  family = poisson
)
summary(egg.all.env.glmm) 

## compare the AIC values
AIC(egg.all.env.glmm, egg.all.comm.glmm, egg.all.global.glmm)

## check the residuals
res.global <- residuals(egg.all.global.glmm)
plot(res.global, backyard$EGG_TOT)


# eggs
## make a confidence interval
backyard.egg.confint.df<-data.frame(confint(egg.all.global.glmm))
## make an output
tab_model(egg.all.global.glmm,
          transform = NULL,
          show.se = TRUE,
          show.zeroinf = F,
          show.r2 = F,
          show.icc = F,
          show.re.var = F,
          show.ngroups = FALSE,
          show.obs = F,
          pred.labels = c(
            "(Intercept)",
            "A. syriaca",
            "A. tuberosa",
            "Height",
            "Predator total - without ants",
            "Ant total",
            "Aphis nerii",
            "Myzocallis asclepiadis",
            "A. syriaca x Height",
            "A. tuberosa x Height",
            "A. syriaca x Predator total - without ants",
            "A. tuberosa x Predator total - without ants",
            "A. syriaca x Ant total",
            "A. tuberosa x Ant total",
            "A. syriaca x Aphis nerii",
            "A. tuberosa x Aphis nerii",
            "A. syriaca x Myzocallis asclepiadis",
            "A. tuberosa x Myzocallis asclepiadis"
          ),
          dv.labels = " ",
          string.est = "Estimate",
          string.se = "SE",
          string.p = "p-value",
          string.ci = "95% CI",
          file = "datavis/egg-results.html"
        )


# LARVAE - ALL SPECIES ----
## global model 
larv.all.global <- LARV_TOT ~ SPECIES + (HEIGHT.std + PRED_WOUT_ANT_TOT.std + ANT_TOT.std + APH_NER.std + MYZ_ASC.std)*SPECIES + (1|LOCATION) + (1|WEEK)
larv.all.global.glmm <- glmmTMB(
  larv.all.global,
  data = backyard,
  family = poisson
)  
summary(larv.all.global.glmm) 
# takes a minute to run don't worry
# with SPECIESINCARNATA at the intercept


larv.all.comm <- LARV_TOT ~ (PRED_WOUT_ANT_TOT.std + ANT_TOT.std + APH_NER.std + MYZ_ASC.std)*SPECIES + (1|LOCATION) + (1|WEEK)
larv.all.comm.glmm<-glmmTMB(
  larv.all.comm,
  data=backyard,
  family=poisson
)
summary(larv.all.comm.glmm) 


larv.all.env <- LARV_TOT ~ SPECIES*HEIGHT + (1|LOCATION) + (1|WEEK)
larv.all.env.glmm <- glmmTMB(
  larv.all.env,
  data=backyard,
  family = poisson
)
summary(larv.all.env.glmm) 

## compare the AIC values
AIC(larv.all.env.glmm, larv.all.comm.glmm, larv.all.global.glmm)

## check the residuals
res.global <- residuals(larv.all.global.glmm)
plot(res.global, backyard$EGG_TOT)


# eggs
## make a confidence interval
backyard.larv.confint.df<-data.frame(confint(larv.all.comm.glmm))
## make an output
tab_model(larv.all.comm.glmm,
          transform = NULL,
          show.se = TRUE,
          show.zeroinf = F,
          show.r2 = F,
          show.icc = F,
          show.re.var = F,
          show.ngroups = FALSE,
          show.obs = F,
          pred.labels = c("(Intercept)",
                          "Predator total - without ants",
                          "Ant total",
                          "Aphis nerii",
                          "Myzocallis asclepiadis",
                          "A. Syriaca",
                          "A. Tuberosa",
                          "A. syriaca x Predator total - without ants",
                          "A. tuberosa x Predator total - without ants",
                          "A. syriaca x Ant total",
                          "A. tuberosa x Ant total",
                          "A. syriaca x Aphis nerii",
                          "A. tuberosa x Aphis nerii",
                          "A. syriaca x Myzocallis asclepiadis",
                          "A. tuberosa x Myzocallis asclepiadis"),
          dv.labels = " ",
          string.est = "Estimate",
          string.se = "SE",
          string.p = "p-value",
          string.ci = "95% CI",
          file = "datavis/larvae-results.html"
)
