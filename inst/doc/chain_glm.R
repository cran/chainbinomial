## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize, include=TRUE, message=FALSE----------------------------------
# Load packages
library(chainbinomial)
library(dplyr)
library(tidyr)

# Take a look at the chain data
head(heasman_reid_1961_chains)

## ----fix_data-----------------------------------------------------------------
# Repeat each chain by the number of times it was observed.
chains_expanded <- rep(heasman_reid_1961_chains$chain, times = heasman_reid_1961_chains$n)

# Here is a function that converts chains into a long data frame.
chain_to_data_frame <- function(x, split='-'){
  
  # Split the strings into vectors.
  chain_list  <- strsplit(x, split = split)
  
  # Get the lengths (number of generations) of each chain.
  chain_lenghts <- sapply(chain_list, FUN = length)
  
  # Initialize data.frame to store the chain data.
  # chain_number is an index variable to keep track of which chain/household the
  # data represents.
  chain_df <- data.frame(chain_number = rep(1:length(chain_list), chain_lenghts))
  
  # Make a variable that indicates the generation.
  generation_list <- lapply(chain_list, FUN = function(x){0:(length(x)-1)})
  chain_df$generation <- unlist(generation_list)
  
  # Add the number of infected in each generation.
  chain_df$I <- as.numeric(unlist(chain_list))
  
  # split the data.frame by chain_number (index variable)
  df_list <- split(chain_df, f = chain_df$chain_number)
  
  # Get the number of infected in the next generaton.
  I_next_list <- lapply(df_list, FUN = function(df){c(df$I[-1], NA)})
  chain_df$I_next <- unlist(I_next_list)
  
  return(chain_df)
}

longdata <- chain_to_data_frame(chains_expanded)

# Next we need to add the number of remaining susceptible and the number 
# of individuals who are NOT infected in the current generation (escaped infection).

longdata %>% 
  group_by(chain_number) %>% 
  mutate(S = 5 - cumsum(I), # All households are of size 5.
         ESCAPED = S - I_next) %>% 
  ungroup() -> longdata

head(longdata)

## ----fix_data2----------------------------------------------------------------
longdata %>% 
  filter(!is.na(I_next)) %>% 
  group_by(chain_number) %>% 
  mutate(TO_REMOVE = any(generation == 0 & I > 1)) %>% ungroup() %>% 
  filter(!TO_REMOVE) -> longdata_filtered


## ----glm1---------------------------------------------------------------------
glm_res <- glm(cbind(ESCAPED, I_next) ~ I - 1, 
               data = longdata_filtered, 
               family = binomial('log'))

summary(glm_res)

## ----glm1_2-------------------------------------------------------------------
1 - exp(coef(glm_res))

## ----glm1_3, message=FALSE----------------------------------------------------
1 - exp(confint(glm_res))

## ----final_outbreak_size1-----------------------------------------------------
# Get the number of primary cases (infected in generation 0).
longdata %>% 
  filter(generation == 0) %>% 
  group_by(chain_number) %>% 
  summarise(I0 = sum(I), .groups = 'drop') -> dta_i0

# Get the sizes of the outbreaks, not counting the primary cases.
longdata %>% 
  group_by(chain_number) %>% 
  summarise(I = sum(I), .groups = 'drop') %>% 
  left_join(dta_i0, by = 'chain_number') %>% 
  # Do not count the primary cases.
  mutate(I = I - I0) %>%
  # Get the number of initial susceptibles. All households have 5 members. 
  mutate(S0 = 5 - I0) -> dta_outbreak_sizes

head(dta_outbreak_sizes)

## ----final_outbreak_size2-----------------------------------------------------
sar_res <- estimate_sar(infected = dta_outbreak_sizes$I, 
                        s0 = dta_outbreak_sizes$S0, 
                        i0 = dta_outbreak_sizes$I0)

sar_res$sar_hat

confint(sar_res)

## ----glm2---------------------------------------------------------------------
# Greenwood model
glm_res2 <- glm(cbind(ESCAPED, I_next) ~ 1, 
               data = longdata_filtered, 
               family = binomial('log'))


summary(glm_res2)

## ----glm2_2-------------------------------------------------------------------
1 - exp(coef(glm_res2))
1 - exp(confint(glm_res2))

## ----glm2_alt-----------------------------------------------------------------
glm_res2_alt <- glm(cbind(I_next, ESCAPED) ~ 1, 
               data = longdata_filtered, 
               family = binomial('log'))

exp(coef(glm_res2_alt))

## ----glm3---------------------------------------------------------------------
# Becker's generalized model
glm_res3 <- glm(cbind(ESCAPED, I_next) ~ as.character(generation) - 1, 
               data = longdata_filtered, 
               family = binomial('log'))


# Take alook at the attack rates.
1 - exp(coef(glm_res3))

