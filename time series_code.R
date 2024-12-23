# Load necessary libraries
library(readr)
library(stats)
library(dygraphs)
library(forecast)
library(tseries)

# Load the dataset
AUS_beer <- read.csv("c:/Users/kiran/Downloads/monthly_beer_data.csv")

# View the first few rows of the dataset
head(AUS_beer)

# Check the structure of the dataset
str(AUS_beer)

# Specify Monthly.beer.production in the data frame and assign it to a variable
AUS_beer <- AUS_beer$Monthly.beer.production

# Convert the series from a data frame to ts format
AUS_beer_ts <- ts(data = AUS_beer,  # The values of the series
                  start = c(1956, 1),  # The time of the first observation
                  frequency = 12)  # The frequency of the series

# Plot the monthly beer production in Australia using dygraph
dygraph(AUS_beer_ts,
        main = "Monthly Beer Production in Australia",
        xlab = "Month",  # Add missing comma
        ylab = "Production (in millions of liters)") %>%
  dyRangeSelector()  # Add a time slider

# Plot the monthly beer production in Australia using base R
plot(AUS_beer_ts, 
     main = "Monthly Beer Production in Australia", 
     xlab = "Time", 
     ylab = "Production (in millions of liters)", 
     col = "blue", 
     lwd = 2)  # Adding color and line width for better visualization

# Perform decomposition (using additive decomposition in this case)
decomposed_ts <- decompose(AUS_beer_ts, type = "additive")

# Plot the decomposed components
plot(decomposed_ts)

# Apply Box-Cox transformation
lambda <- BoxCox.lambda(AUS_beer_ts)
lambda
beer_transformed <- BoxCox(AUS_beer_ts, lambda)
plot(beer_transformed, main = "Transformed Beer Production (Box-Cox)")

# Exploratory Data Analysis (EDA)
# Plot ACF and PACF
acf(AUS_beer_ts, main = "ACF of Monthly Beer Production")

# Plot the PACF (Partial Autocorrelation Function)
pacf(AUS_beer_ts, main = "PACF of Monthly Beer Production")

# Apply differencing if necessary
beer_diff <- diff(AUS_beer_ts, differences = 1)
plot(beer_diff, main = "Differenced Series (1st Order)")

# Apply seasonal differencing if required (lag = 12 for monthly data)
beer_seasonal_diff <- diff(beer_diff, lag = 12)
plot(beer_seasonal_diff, main = "Seasonally Differenced Series")

# Recheck stationarity of the seasonally differenced series
adf_test_diff <- adf.test(beer_seasonal_diff)
print(adf_test_diff)

# Plot ACF and PACF of the seasonally differenced series
par(mfrow = c(1,2))
acf(beer_seasonal_diff, main = "ACF of Monthly Beer Production")
pacf(beer_seasonal_diff, main = "PACF of Monthly Beer Production")

# Data Splitting
# Split into training (80%) and test (20%) sets
train_size <- floor(0.8 * length(AUS_beer_ts))
train_set <- window(AUS_beer_ts, end = c(1956 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_set <- window(AUS_beer_ts, start = c(1956 + train_size %/% 12, train_size %% 12 + 1))

# Visualize the training and test sets
plot(train_set, main = "Training and Test Sets", col = "blue", ylab = "Beer Production", xlab = "Time")
lines(test_set, col = "red")
legend("topright", legend = c("Training Set", "Test Set"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Fit the SARIMA model
sarima_model <- auto.arima(train_set, seasonal = TRUE, lambda = lambda)
print(sarima_model)

# Fit the SARIMA model with updated parameters (1, 1, 1)(1, 1, 1)[12]
model_111 <- arima(train_set, 
                   order=c(1,1,1),               # Non-seasonal ARIMA (AR=1, differencing=1, MA=1)
                   seasonal=list(order=c(1,1,1), period=12))  # Seasonal ARIMA (SAR=1, SD=1, SMA=1, s=12)

# View the model summary
summary(model_111)

# Fit Exponential Smoothing Model
ets_model <- ets(train_set, lambda = lambda)
summary(ets_model)

# Evaluate Models
sarima_forecast <- forecast(sarima_model, h = length(test_set))
ets_forecast <- forecast(ets_model, h = length(test_set))

# Plot forecasts
plot(sarima_forecast, main = "SARIMA Forecast")
lines(test_set, col = "red")

plot(ets_forecast, main = "ETS Forecast")
lines(test_set, col = "red")

# Calculate RMSE for each model
sarima_rmse <- sqrt(mean((test_set - sarima_forecast$mean)^2))
ets_rmse <- sqrt(mean((test_set - ets_forecast$mean)^2))

cat("SARIMA RMSE:", sarima_rmse, "\n")
cat("ETS RMSE:", ets_rmse, "\n")

# Forecast for 1996 (choose the best model)
best_model <- sarima_model  # Replace with the best model based on RMSE
forecast_1996 <- forecast(best_model, h = 12)
plot(forecast_1996, main = "Forecast for 1996")
