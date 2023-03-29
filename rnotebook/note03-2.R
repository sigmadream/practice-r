library(readr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(caret)
library(leaps)
library(car)
library(mice)
library(scales)
library(RColorBrewer)
library(plotly)
library(nortest)
library(lmtest)

housing_data = read_csv("./data/housing.csv")
summary(housing_data)

plot_map = ggplot(housing_data, 
                  aes(x = longitude, y = latitude, color = median_house_value, 
                      hma = housing_median_age, tr = total_rooms, tb = total_bedrooms,
                      hh = households, mi = median_income)) +
  geom_point(aes(size = population), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map - Longtitude vs Latitude and Associated Variables") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(color = "Median House Value (in $USD)", size = "Population")
plot_map

housing_data$ocean_proximity = as.factor(housing_data$ocean_proximity)
levels(housing_data$ocean_proximity)

ggplot(housing_data, aes(x = factor(ocean_proximity))) +
  geom_bar(stat = "count", color = "black", fill = "blue")

summary(housing_data$ocean_proximity)
housing_data = housing_data[housing_data$ocean_proximity != "ISLAND", ]

sum(is.na(housing_data))

total_bedrooms = housing_data$total_bedrooms
sum(is.na(total_bedrooms))

bedroom_mean = mean(housing_data$total_bedrooms, na.rm=TRUE)
bedroom_median = median(housing_data$total_bedrooms, na.rm=TRUE)
ggplot(housing_data, aes(x = total_bedrooms)) +
  geom_histogram(bins = 40, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "Mean"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "Median"), lwd = 1.5) +
  xlab("Total Bedrooms") +
  ylab("Frequency") +
  ggtitle("Histogram of Total Bedrooms (noncontinuous variable)") +
  scale_color_manual(name = "Summary Stats", labels = c("Mean", "Median"), values = c("red", "green"))

housing_data$total_bedrooms[is.na(housing_data$total_bedrooms)] = bedroom_median

str(housing_data)

par(mfrow = c(3, 3))
hist(housing_data$longitude, breaks = 20, main = "longitude", border="darkorange", col="dodgerblue")
hist(housing_data$latitude, breaks = 20, main = "latitude", border="darkorange", col="dodgerblue")
hist(housing_data$housing_median_age, breaks = 20, main = "housing_median_age", border="darkorange", col="dodgerblue")
hist(housing_data$total_rooms, breaks = 20, main = "total_rooms", border="darkorange", col="dodgerblue")
hist(housing_data$total_bedrooms, breaks = 20, main = "total_bedrooms", border="darkorange", col="dodgerblue")
hist(housing_data$population, breaks = 20, main = "population", border="darkorange", col="dodgerblue")
hist(housing_data$households, breaks = 20, main = "households", border="darkorange", col="dodgerblue")
hist(housing_data$median_income, breaks = 20, main = "median_income", border="darkorange", col="dodgerblue")
hist(housing_data$median_house_value, breaks = 20, main = "median_house_value", border="darkorange", col="dodgerblue")

pairs(housing_data, col = "dodgerblue")

housing_data_nc = housing_data[, -10]
corrmatrix = cor(housing_data_nc)
kable(t(corrmatrix))

set.seed(42)
housing_trn_idx = createDataPartition(housing_data$ocean_proximity, p = .70, list = FALSE)

housing_trn_data = housing_data[housing_trn_idx, ]
housing_tst_data = housing_data[-housing_trn_idx, ]

full_additive_model = lm(median_house_value ~ ., data = housing_trn_data)
full_additive_adjr2 = summary(full_additive_model)$adj.r.squared

full_twoway_model = lm(median_house_value ~ (.)^2, data = housing_trn_data)
full_twoway_adjr2 = summary(full_twoway_model)$adj.r.squared

full_threeway_model = lm(median_house_value ~ (.)^3, data = housing_trn_data)
full_threeway_adjr2 = summary(full_threeway_model)$adj.r.squared

beginning_mods_results = data.frame(
  "Total Predictors" =
    c("Additive Model" = extractAIC(full_additive_model)[1],
      "Two-Way Int. Model" = extractAIC(full_twoway_model)[1],
      "Three-Way Int. Model" = extractAIC(full_threeway_model)[1]),
  "AIC" =
    c("Additive Model" = extractAIC(full_additive_model)[2],
      "Two-Way Int. Model" = extractAIC(full_twoway_model)[2],
      "Three-Way Int. Model" = extractAIC(full_threeway_model)[2]),
  "Adj R-Squared" =
    c("Additive Model" = full_additive_adjr2,
      "Two-Way Int. Model" = full_twoway_adjr2,
      "Three-Way Int. Model" = full_threeway_adjr2))

kable(beginning_mods_results, align = c("c", "r"))

back_additive_mod_finish_aic = step(full_additive_model, direction = "backward", trace = 0)
both_additive_mod_finish_aic = step(full_additive_model, direction = "both", trace = 0)

n = length(resid(full_additive_model))
back_additive_mod_finish_bic = step(full_additive_model, direction = "backward", k = log(n), trace = 0)
both_additive_mod_finish_bic = step(full_additive_model, direction = "both", k = log(n), trace = 0)

back_twoway_mod_finish_aic = step(full_twoway_model, direction = "backward", trace = 0)
both_twoway_mod_finish_aic = step(full_twoway_model, direction = "both", trace = 0)

n = length(resid(full_twoway_model))
back_twoway_mod_finish_bic = step(full_twoway_model, direction = "backward", k = log(n), trace = 0)
both_twoway_mod_finish_bic = step(full_twoway_model, direction = "both", k = log(n), trace = 0)

aic_and_bic_results = data.frame(
  "AIC" =
    c("Backward" =
        c("Additive" = extractAIC(back_additive_mod_finish_aic)[2],
          "Two-Way" = extractAIC(back_twoway_mod_finish_aic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2]),
      "Both" =
        c("Additive" = extractAIC(both_additive_mod_finish_aic)[2],
          "Two-Way" = extractAIC(both_twoway_mod_finish_aic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2])),
  "BIC" =
    c("Backward" =
        c("Additive" = extractAIC(back_additive_mod_finish_bic)[2],
          "Two-Way" = extractAIC(back_twoway_mod_finish_bic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2]),
      "Both" =
        c("Additive" = extractAIC(both_additive_mod_finish_bic)[2],
          "Two-Way" = extractAIC(both_twoway_mod_finish_bic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2])))

kable(aic_and_bic_results)

diagnostics = function(model, alpha = .05, pointcol = "orange", linecol = "blue", plots = TRUE, tests = TRUE, pointtype = 16) {
  if (plots == TRUE) {
    par(mfrow = c(1, 3))
    plot(
      fitted(model),
      resid(model),
      pch = pointtype,
      xlab = "Fitted Values",
      ylab = "Residuals",
      main = "Fitted vs Residuals",
      col = pointcol
    )
    abline(h = 0, lwd = 2, col = linecol)
    
    qqnorm(
      resid(model),
      pch = pointtype,
      main = "QQNorm Plot",
      col = pointcol
    )
    qqline(
      resid(model),
      lwd = 2,
      col = linecol
    )
    hist(
      resid(model),
      main = "Histogram of Residuals",
      col = pointcol,
      xlab = "Residuals",
      ylab = "Frequency"
    )
  }
  if (tests == TRUE) {
    ad_test = ad.test(resid(model))
    bp_test = bptest(model)
    test_results = data.frame(
      "Anderson-Darling Normality Test" =
        c("Test Statistic" = round(ad_test$statistic, 5),
          "P-Value" = ad_test$p.value,
          "Result" = ifelse(ad_test$p.value < alpha, "Reject", "Fail To Reject")),
      "Breusch-Pagan Test" =
        c("Test Statistic" = round(bp_test$statistic, 5),
          "P-Value" = bp_test$p.value,
          "Result" = ifelse(bp_test$p.value < alpha, "Reject", "Fail To Reject")))
    
    kable(t(test_results), col.names = c("Test Statistic", "P-Value", "Decision"))
  }
}

diagnostics(back_additive_mod_finish_aic)

diagnostics(back_twoway_mod_finish_aic)

diagnostics(full_threeway_model)

back_twoway_mod_finish_aic_log = lm(formula = log(median_house_value) ~ longitude + latitude + housing_median_age + 
                                      total_rooms + total_bedrooms + population + households + 
                                      median_income + ocean_proximity + longitude:latitude + longitude:housing_median_age + 
                                      longitude:total_rooms + longitude:total_bedrooms + longitude:households + 
                                      longitude:median_income + longitude:ocean_proximity + latitude:housing_median_age + 
                                      latitude:total_rooms + latitude:total_bedrooms + latitude:median_income + 
                                      latitude:ocean_proximity + housing_median_age:total_rooms + 
                                      housing_median_age:population + housing_median_age:households + 
                                      housing_median_age:median_income + housing_median_age:ocean_proximity + 
                                      total_rooms:population + total_rooms:households + total_rooms:median_income + 
                                      total_rooms:ocean_proximity + total_bedrooms:households + 
                                      total_bedrooms:median_income + total_bedrooms:ocean_proximity + 
                                      population:households + population:median_income + population:ocean_proximity + 
                                      households:median_income + households:ocean_proximity + median_income:ocean_proximity, 
                                    data = housing_trn_data)

diagnostics(back_twoway_mod_finish_aic_log)

# From the text: http://daviddalpiaz.github.io/appliedstats/variable-selection-and-model-building.html
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_rmse = function(actual, predicted) {
  sqrt(sum((actual - predicted)^2) / length(actual)) 
}
calc_avg_per_error = function(actual, predicted) {
  inter_abs = abs(predicted - actual)
  100 * (sum(inter_abs / actual)) / length(actual)
}

additive_loocv_rmse = calc_loocv_rmse(back_additive_mod_finish_aic)
twoway_loocv_rmse = calc_loocv_rmse(back_twoway_mod_finish_aic)
threeway_loocv_rmse = calc_loocv_rmse(full_threeway_model)

loocv_rmse_results = data.frame(
  "LOOCV-RMSE" =
    c("Backwards Additive" = additive_loocv_rmse,
      "Backwards Two-Way" = twoway_loocv_rmse,
      "Initial Three-way" = threeway_loocv_rmse))

kable(loocv_rmse_results)

# the actual median house values from the test set
test_actual = housing_tst_data$median_house_value
# the predicted house values for the test set
test_predictions = predict(back_twoway_mod_finish_aic, housing_tst_data)
# the RMSE
test_rmse = calc_rmse(test_actual, test_predictions)
# the percentage error
test_perc_error = calc_avg_per_error(test_actual, test_predictions)

test_rmse

test_perc_error

op0 = par()
op1 = op0$mar
op1[2] = 7
par(mar = op1)
plot(test_predictions - test_actual, housing_tst_data$ocean_proximity,
     xlab = "Predicted Price Over / Under Actual Price",
     ylab = "",
     main = "Predicted Price Over / Under Actual Price vs Ocean Proximity",
     col = "dodgerblue", yaxt = "none")
axis(2, at = 1:5, labels = levels(housing_tst_data$ocean_proximity), las = 1)

plot_map = ggplot(housing_tst_data, 
                  aes(x = longitude, y = latitude, 
                      color = test_predictions - test_actual, 
                      hma = housing_median_age, tr = total_rooms, tb = total_bedrooms,
                      hh = households, mi = median_income)) +
  geom_point(aes(size = abs(test_predictions - test_actual)), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Predicted Price Over / Under Actual Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(color = "Predicted Price Over / Under (in $USD)", 
       size = "Magnitude of Price Difference")
plot_map
