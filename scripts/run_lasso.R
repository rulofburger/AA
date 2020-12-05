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

weights_plus_splines <- readRDS("data/weights_plus_splines.rds") %>% 
  mutate(educ = factor(educ))


lm(
  data = weights_plus_splines, 
  l_imputed_real ~ white + educ +
    educ:white +
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white 
)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

lambda <- 10^seq(-6, -0.45, length = 100)

set.seed(825)
lasso_fit <- train(
  l_imputed_real ~ white + educ +
    educ:white +
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white 
  ,
  data = weights_plus_splines, 
  method = "glmnet", 
  trControl = fitControl,
  ## This last option is actually one
  ## for gbm() that passes through
  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
  verbose = TRUE
)

lasso_fit

X <- model.matrix(
  ~ white + educ +
    educ:white +
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white 
  ,
  data = weights_plus_splines
  )

lasso_fit_cv <- glmnet::cv.glmnet(
  x = X,
  y = weights_plus_splines$l_imputed_real,
  data = weights_plus_splines
)

lasso_fit_cv
sum(coef(lasso_fit_cv) == 0)
sum(coef(lasso_fit_cv) != 0)
coef(lasso_fit_cv)

# model_lasso <- train(
#   log_income_pc ~ .,
#   data = df_train_pre_proc,
#   method = "glmnet",
#   trControl = fitControl,
#   tuneGrid = expand.grid(alpha = 1, lambda = lambda)
# )

