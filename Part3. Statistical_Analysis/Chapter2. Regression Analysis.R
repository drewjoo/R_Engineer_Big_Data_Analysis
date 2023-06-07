# part3
# Chapter2. Regression Analysis

# Load Data
library(MASS)
data('Cars93')
str(Cars93)

# Theme1. Simple Regression Analysis
lm(Price ~ EngineSize, data = Cars93)

# Summarize the model
Cars93_lm = lm(Price ~ EngineSize, data = Cars93)
summary(Cars93_lm)

# set.seed
set.seed(1234)

# randomly pick 5 indexes
idx = sample(1:nrow(Cars93), size = 5)
idx

# X_test data
X_test = Cars93[idx, ]

# Predict y_test value
predict(Cars93_lm, X_test, interval = 'none')

# Predict y_test value with confidence interval
predict(Cars93_lm, X_test, interval = 'confidence')

# Predict y_test value with prediction interval
predict(Cars93_lm, X_test, interval = 'prediction')

#---------------------------------------------------

# Theme2. Multi Regression Analysis
Price_lm = lm(Price ~ EngineSize + RPM + Weight, data = Cars93)
summary(Price_lm)

# Variable Selection
lm_result = lm(Price ~ EngineSize + Horsepower + RPM + Width + Length + Weight, data = Cars93)
step(lm_result, direction = 'backward') # forward, stepwise
