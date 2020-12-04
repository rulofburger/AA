library(caret)
library(tidyverse)


# weights_plus_splines <- haven::read_dta("data/weights_plus_splines.dta") %>% 
#   tibble %>% 
#   select(
#     l_imputed_real, race, age_20, age_25, age_30, age_35, age_40, age_45, age_50, age_55,
#     year_1997, year_1998, year_1999, year_2000, year_2001, year_2002, year_2003, year_2004,
#     year_2005, year_2006, year_2007, year_2008, year_2010, year_2011, year_2012, year_2013, 
#     year_2014, year_2015, year_2016, year_2017
#   ) %>% 
#   mutate(white = race == 4) %>% 
#   filter(!is.na(l_imputed_real)) %>% 
#   select(-race)
# 
# saveRDS(weights_plus_splines, "data/weights_plus_splines.rds")

weights_plus_splines <- readRDS("data/weights_plus_splines.rds")


lm(
  data = weights_plus_splines, l_imputed_real ~ .
)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

lambda <- 10^seq(-4, 0, length = 10)

set.seed(825)
lasso_fit <- train(
  l_imputed_real ~ ., 
  data = weights_plus_splines, 
  method = "glmnet", 
  trControl = fitControl,
  ## This last option is actually one
  ## for gbm() that passes through
  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
  verbose = TRUE
)

blahblah
model_lasso <- train(
  log_income_pc ~ .,
  data = df_train_pre_proc,
  method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

lasso_fit