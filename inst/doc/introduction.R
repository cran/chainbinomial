## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)



## ----loadpackage--------------------------------------------------------------
library(chainbinomial)
require(dplyr)
require(tidyr)

## ----ex1----------------------------------------------------------------------
dchainbinom(x = 2, s0 = 3, i0 = 1, sar = 0.23)

## ----ex2----------------------------------------------------------------------
dchainbinom(x = 0:3, s0 = 3, i0 = 1, sar = 0.23)

## ----ex3----------------------------------------------------------------------
dchainbinom(x = 0:2, s0 = 2, i0 = 2, sar = 0.23)

## ----ex4----------------------------------------------------------------------
dchainbinom(x = 0:3, s0 = 3, i0 = 1, sar = 0.23, generations = 2)

## ----data1--------------------------------------------------------------------
heasman_reid_1961_intro_case_status

## ----data2, message=FALSE-----------------------------------------------------
library(dplyr)
library(tidyr)

heasman_reid_1961_intro_case_status %>% 
  pivot_longer(cols = -1, 
               names_to = 'intro_case', 
               values_to = 'N') %>% 
  uncount(weights = N) -> intro_case_status_long

head(intro_case_status_long)

## ----sar1---------------------------------------------------------------------
intro_case_status_long %>% 
  filter(intro_case == 'father') -> intro_case_status_long_fathers


sar_est <- estimate_sar(infected = intro_case_status_long_fathers$furter_cases, s0 = 4)

sar_est$sar_hat

## ----sar2---------------------------------------------------------------------
sar_est$sar_hat

## ----sar3---------------------------------------------------------------------
confint(sar_est)

## ----cbmod1-------------------------------------------------------------------
xmat <- model.matrix(~ intro_case, data = intro_case_status_long)

cbmod_res <- cbmod(y = intro_case_status_long$furter_cases,
                   s0 = rep(4, nrow(intro_case_status_long)), 
                   x = xmat, 
                   i0 = 1, 
                   link = 'identity')


summary(cbmod_res)

## ----cbmod2-------------------------------------------------------------------
confint(cbmod_res)

