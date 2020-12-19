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


my_grid <- expand.grid(
  alpha = seq(0, 1, length = 10),
  # alpha = 1,
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
  method = "glmnet", 
  tuneGrid = my_grid,
  trControl = fitControl
)

lasso_fit <- train(
  l_imputed_real ~ 
    white + 
    educ_10 + educ_11 + educ_12 + educ_13 + educ_15 + 
    age_20 + age_25 + age_30 + age_35 + age_40 + age_45 + age_50 + age_55 + 
    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + 
    year_2002 + year_2003 + year_2004 + year_2005 + year_2006 +      
    year_2007 + year_2008 + year_2010 + year_2011 + year_2012 + 
    year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + 
    
    educ_10:white + educ_11:white + educ_12:white + educ_13:white + educ_15:white + 
    age_20:white + age_25:white + age_30:white + age_35:white + age_40:white + age_45:white + age_50:white + age_55:white +
    year_1997:white + year_1998:white + year_1999:white + year_2000:white + year_2001:white + 
    year_2002:white + year_2003:white + year_2004:white + year_2005:white + year_2006:white +      
    year_2007:white + year_2008:white + year_2010:white + year_2011:white + year_2012:white + 
    year_2013:white + year_2014:white + year_2015:white + year_2016:white + year_2017:white + 

    year_1997:educ_10 + year_1998:educ_10 + year_1999:educ_10 + year_2000:educ_10 + year_2001:educ_10 + 
    year_2002:educ_10 + year_2003:educ_10 + year_2004:educ_10 + year_2005:educ_10 + year_2006:educ_10 +      
    year_2007:educ_10 + year_2008:educ_10 + year_2010:educ_10 + year_2011:educ_10 + year_2012:educ_10 + 
    year_2013:educ_10 + year_2014:educ_10 + year_2015:educ_10 + year_2016:educ_10 + year_2017:educ_10 +
    
    year_1997:educ_11 + year_1998:educ_11 + year_1999:educ_11 + year_2000:educ_11 + year_2001:educ_11 + 
    year_2002:educ_11 + year_2003:educ_11 + year_2004:educ_11 + year_2005:educ_11 + year_2006:educ_11 +      
    year_2007:educ_11 + year_2008:educ_11 + year_2010:educ_11 + year_2011:educ_11 + year_2012:educ_11 + 
    year_2013:educ_11 + year_2014:educ_11 + year_2015:educ_11 + year_2016:educ_11 + year_2017:educ_11 +
    
    year_1997:educ_12 + year_1998:educ_12 + year_1999:educ_12 + year_2000:educ_12 + year_2001:educ_12 + 
    year_2002:educ_12 + year_2003:educ_12 + year_2004:educ_12 + year_2005:educ_12 + year_2006:educ_12 +      
    year_2007:educ_12 + year_2008:educ_12 + year_2010:educ_12 + year_2011:educ_12 + year_2012:educ_12 + 
    year_2013:educ_12 + year_2014:educ_12 + year_2015:educ_12 + year_2016:educ_12 + year_2017:educ_12 +
    
    year_1997:educ_13 + year_1998:educ_13 + year_1999:educ_13 + year_2000:educ_13 + year_2001:educ_13 + 
    year_2002:educ_13 + year_2003:educ_13 + year_2004:educ_13 + year_2005:educ_13 + year_2006:educ_13 +      
    year_2007:educ_13 + year_2008:educ_13 + year_2010:educ_13 + year_2011:educ_13 + year_2012:educ_13 + 
    year_2013:educ_13 + year_2014:educ_13 + year_2015:educ_13 + year_2016:educ_13 + year_2017:educ_13 +
    
    year_1997:educ_15 + year_1998:educ_15 + year_1999:educ_15 + year_2000:educ_15 + year_2001:educ_15 + 
    year_2002:educ_15 + year_2003:educ_15 + year_2004:educ_15 + year_2005:educ_15 + year_2006:educ_15 +      
    year_2007:educ_15 + year_2008:educ_15 + year_2010:educ_15 + year_2011:educ_15 + year_2012:educ_15 + 
    year_2013:educ_15 + year_2014:educ_15 + year_2015:educ_15 + year_2016:educ_15 + year_2017:educ_15 +
    
    year_1997:age_20 + year_1998:age_20 + year_1999:age_20 + year_2000:age_20 + year_2001:age_20 + 
    year_2002:age_20 + year_2003:age_20 + year_2004:age_20 + year_2005:age_20 + year_2006:age_20 +      
    year_2007:age_20 + year_2008:age_20 + year_2010:age_20 + year_2011:age_20 + year_2012:age_20 + 
    year_2013:age_20 + year_2014:age_20 + year_2015:age_20 + year_2016:age_20 + year_2017:age_20 +

    year_1997:age_25 + year_1998:age_25 + year_1999:age_25 + year_2000:age_25 + year_2001:age_25 + 
    year_2002:age_25 + year_2003:age_25 + year_2004:age_25 + year_2005:age_25 + year_2006:age_25 +      
    year_2007:age_25 + year_2008:age_25 + year_2010:age_25 + year_2011:age_25 + year_2012:age_25 + 
    year_2013:age_25 + year_2014:age_25 + year_2015:age_25 + year_2016:age_25 + year_2017:age_25 +
  
    year_1997:age_30 + year_1998:age_30 + year_1999:age_30 + year_2000:age_30 + year_2001:age_30 + 
    year_2002:age_30 + year_2003:age_30 + year_2004:age_30 + year_2005:age_30 + year_2006:age_30 +      
    year_2007:age_30 + year_2008:age_30 + year_2010:age_30 + year_2011:age_30 + year_2012:age_30 + 
    year_2013:age_30 + year_2014:age_30 + year_2015:age_30 + year_2016:age_30 + year_2017:age_30 +
    
    year_1997:age_35 + year_1998:age_35 + year_1999:age_35 + year_2000:age_35 + year_2001:age_35 + 
    year_2002:age_35 + year_2003:age_35 + year_2004:age_35 + year_2005:age_35 + year_2006:age_35 +      
    year_2007:age_35 + year_2008:age_35 + year_2010:age_35 + year_2011:age_35 + year_2012:age_35 + 
    year_2013:age_35 + year_2014:age_35 + year_2015:age_35 + year_2016:age_35 + year_2017:age_35 +
    
    year_1997:age_40 + year_1998:age_40 + year_1999:age_40 + year_2000:age_40 + year_2001:age_40 + 
    year_2002:age_40 + year_2003:age_40 + year_2004:age_40 + year_2005:age_40 + year_2006:age_40 +      
    year_2007:age_40 + year_2008:age_40 + year_2010:age_40 + year_2011:age_40 + year_2012:age_40 + 
    year_2013:age_40 + year_2014:age_40 + year_2015:age_40 + year_2016:age_40 + year_2017:age_40 +
    
    year_1997:age_45 + year_1998:age_45 + year_1999:age_45 + year_2000:age_45 + year_2001:age_45 + 
    year_2002:age_45 + year_2003:age_45 + year_2004:age_45 + year_2005:age_45 + year_2006:age_45 +      
    year_2007:age_45 + year_2008:age_45 + year_2010:age_45 + year_2011:age_45 + year_2012:age_45 + 
    year_2013:age_45 + year_2014:age_45 + year_2015:age_45 + year_2016:age_45 + year_2017:age_45 +

    year_1997:age_50 + year_1998:age_50 + year_1999:age_50 + year_2000:age_50 + year_2001:age_50 + 
    year_2002:age_50 + year_2003:age_50 + year_2004:age_50 + year_2005:age_50 + year_2006:age_50 +      
    year_2007:age_50 + year_2008:age_50 + year_2010:age_50 + year_2011:age_50 + year_2012:age_50 + 
    year_2013:age_50 + year_2014:age_50 + year_2015:age_50 + year_2016:age_50 + year_2017:age_50 +
    
    year_1997:age_55 + year_1998:age_55 + year_1999:age_55 + year_2000:age_55 + year_2001:age_55 + 
    year_2002:age_55 + year_2003:age_55 + year_2004:age_55 + year_2005:age_55 + year_2006:age_55 +      
    year_2007:age_55 + year_2008:age_55 + year_2010:age_55 + year_2011:age_55 + year_2012:age_55 + 
    year_2013:age_55 + year_2014:age_55 + year_2015:age_55 + year_2016:age_55 + year_2017:age_55
  ,
  weights = phi,
  data = weights_plus_splines,
  method = "glmnet", 
  tuneGrid = my_grid,
  trControl = fitControl
)


lasso_fit
lasso_fit$finalModel$beta
predict(lasso_fit$finalModel, type = "coeff")
coef(lasso_fit$finalModel, lasso_fit$bestTune$lambda)

sum(coef(lasso_fit) == 0)
sum(coef(lasso_fit) != 0)
coef(lasso_fit)
