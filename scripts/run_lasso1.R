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
  data = weights_plus_splines, 
  l_imputed_real ~ white + educ_10 + educ_11 + educ_12 + educ_13 + educ_15 +  
    educ_10:white + educ_11:white + educ_12:white + educ_13:white + educ_15:white + 
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013:white + year_2014:white + year_2015:white + year_2016:white + year_2017:white
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white,
  weight = phi
  )

lm_fit <- lm.fit(
  x = X,
  y = weights_plus_splines_dummies$l_imputed_real
)


set.seed(3721)
cv.10.folds <- createFolds(weights_plus_splines$l_imputed_real, k = 3)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 3,
  ## repeated ten times
  index = cv.10.folds)

lambda <- 10^seq(-6, -0.45, length = 10)
lasso_grid <- expand.grid(fraction = c(0.1, 0.01))

my_grid <- expand.grid(
  # alpha = seq(0, 1, length = 10),
  alpha = 1,
  lambda = 10^seq(-6, -0.45, length = 10)
  )

set.seed(825)
lasso_fit <- train(
  l_imputed_real ~ white + educ_10 + educ_11 + educ_12 + educ_13 + educ_15 +  
    educ_10:white + educ_11:white + educ_12:white + educ_13:white + educ_15:white + 
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013:white + year_2014:white + year_2015:white + year_2016:white + year_2017:white + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white,
  weights = phi,
  data = weights_plus_splines,
  # x = X,
  # y = weights_plus_splines$l_imputed_real,
  # preProcess = c("center", "scale"),
  method = "glmnet", 
  tuneGrid = my_grid,
  trControl = fitControl
)

lasso_fit
lasso_fit$finalModel$beta
predict(lasso_fit$finalModel, type = "coeff")
coef(lasso_fit$finalModel, lasso_fit$bestTune$lambda)


X <- model.matrix(
  ~ white + educ_11 + educ_12 + educ_13 + educ_15 + educ_16 + 
    educ_10:white + educ_11:white + educ_12:white + educ_13:white + educ_15:white + educ_16:white + 
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013:white + year_2014:white + year_2015:white + year_2016:white + year_2017:white + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white 
  ,
  data = weights_plus_splines_dummies
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

