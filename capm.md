
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
#Initialise the first daily return
df$Daily_Return_AMD <- numeric(nrow(df))
df$Daily_Return_GSPC <- numeric(nrow(df))
df$Daily_Return_AMD[1] <- 0
df$Daily_Return_GSPC[1] <- 0

#Loop through the data set and calculate the daily returns for AMD and GSPC.
for (i in 2:nrow(df)) {
  if (!is.na(df$AMD[i]) && !is.na(df$AMD[i - 1]) && df$AMD[i - 1] != 0) {
    df$Daily_Return_AMD[i] <- (df$AMD[i]- df$AMD[i - 1])/df$AMD[i - 1]
  }  else {
    df$Daily_Return_AMD[i] <- NA
  }
   if ( df$GSPC[i - 1] != 0 && !is.na(df$GSPC[i]) && !is.na(df$GSPC[i - 1])) {
    df$Daily_Return_GSPC[i] <- (df$GSPC[i]- df$GSPC[i - 1])/df$GSPC[i - 1]
    
   } else {
    df$Daily_Return_GSPC[i] <- NA
  }
  
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#Initializes the Daily Risk-Free Rate to 0 
df$Daily_RF <- 0

#The loop goes through each dates imported adjusted Annual Risk Free rate and using the formula given, calculates the Daily Risk-Free Rate.
for (i in 1:nrow(df)){
  df$Daily_RF[i] = (1 + (df$RF[i])/100)^(1/360) - 1 
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#Intialises the excess returns to 0
df$Excess_Returns_AMD <- 0
df$Excess_Returns_GSPC <- 0

#Loops through and calculates the daily excess return for AMD and S&P500  
for (i in 1:nrow(df)){
  df$Excess_Returns_AMD[i] <- df$Daily_Return_AMD[i] - df$Daily_RF[i]
  df$Excess_Returns_GSPC[i] <- df$Daily_Return_GSPC[i] - df$Daily_RF[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
regression_model <- lm(df$Excess_Returns_AMD ~ df$Excess_Returns_GSPC, data = df)
summary(regression_model)
summary_model <- summary(regression_model)
beta_value <- summary_model$coefficients[2,1]
#Beta coefficient of approximately 1.570
print(sprintf("Beta coefficient is approximately %.3f", beta_value))
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:** The beta coefficient for AMD’s excess returns is approximately 1.570, indicating higher volatility compared to the S&P500. Specifically, AMD’s beta values suggests that for every 1% change in the S&P 500 excess returns, AMD’s excess returns is expected to change 1.57%, reflecting higher risk and variability. Given the S&P500 serves as a broad market indicator, AMD is more volatile than the market.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
library(ggplot2)

ggplot(df, aes(x = Excess_Returns_GSPC, y = Excess_Returns_AMD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, col = "red") +
  labs(
    title = "CAPM Regression: AMD Vs S&P500 Excess Returns",
      x =  "S&P500 Excess Returns",
      y = "AMD Excess Returns"
  ) +
  theme_classic()
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#Using the regression model, extract the daily standard error (sigma value).
extract_summ <- summary(regression_model)
s_f <- extract_summ$sigma

#Convert the daily standard error to the annual standard error using the provided formula.
#252 represents the number of days the markets open in a year.
annual_s_f <- s_f*(sqrt(252))

#Initialise values for the Expected Annual Return of AMD Formula.

#Given Values.
risk_free_rate <- 0.05
exp_GSPC_return <- 0.133

#Extract the Beta Value.
amd_beta <- coef(regression_model)["df$Excess_Returns_GSPC"]

#Calculation for expected AMD return (Capital Assest Pricing Model).
exp_amd_return <- risk_free_rate + amd_beta*(exp_GSPC_return-risk_free_rate)

#Using the z-score to calculate the prediction interval for the larger sample size (n >30) as the mean approaches normality, due to Central Limit Theorem.

#Z-score for 90% confidence interval.
z_score <- 1.645

#Margin of Error
error_margin <- z_score*annual_s_f

#Prediction interval.
upper <- (exp_amd_return + error_margin)*100
lower <- (exp_amd_return - error_margin)*100

print(sprintf("AMD's Annual Expected Return with a 90%% prediction interval is %.2f%% to %.2f%%", lower, upper))
```
