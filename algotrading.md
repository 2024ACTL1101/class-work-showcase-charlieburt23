
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
#Initialize the first buy
amd_df$trade_type[1] <- "buy"
amd_df$accumulated_shares[1] <- share_size
amd_df$costs_proceeds[1] <- (-share_size)*amd_df$close[1]
for (i in 2:nrow(amd_df)) {
# Fill your code here
if (amd_df$close[i] < amd_df$close[i-1] ) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- (-share_size)*amd_df$close[i]
amd_df$accumulated_shares[i] <- share_size + amd_df$accumulated_shares[i-1]
} else {
amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
amd_df$costs_proceeds[i] <- 0
}
if (i == nrow(amd_df)) {
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- (amd_df$accumulated_shares[i])*(amd_df$close[i])
}
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
##Repeats step 2, to ensure the correct strategy is implemented for this step.
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
#initialise the first buy
amd_df$trade_type[1] <- "buy"
amd_df$accumulated_shares[1] <- 100
amd_df$costs_proceeds[1] <- (-100)*amd_df$close[1]
for (i in 2:nrow(amd_df)) {
if (amd_df$close[i] < amd_df$close[i-1] ) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- (-share_size)*amd_df$close[i]
amd_df$accumulated_shares[i] <- share_size + amd_df$accumulated_shares[i-1]
} else {
amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
amd_df$costs_proceeds[i] <- 0
}
if (i == nrow(amd_df)) {
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- (amd_df$accumulated_shares[i])*(amd_df$close[i])
}
}
#The code for finding the optimal dates using step 2's buying strategy.
amd_df$buy_date <- amd_df$date[1]
amd_df$sell_date <- amd_df$date[1]
max_profit <- 0
#The bigger for loop runs through all possible starting buying dates
for (i in 1:nrow(amd_df)) {
##The smaller for loop runs through all possible corresponding selling dates, starting from the buying date bei
ng tested.
for (j in i:nrow(amd_df)) {
##Sums all the capital invested from the tested buy to tested sell date.
for (k in i:j) {
sum <- 0
sum <- sum + amd_df$costs_proceeds[k]
}
##Now finds the profit if all shares were sold at the sell dates share price
if (((amd_df$close[j])*(amd_df$accumulated_shares[j]) - sum) > max_profit) {
##if this is greater than any previous dates profit, these dates will be assigned the new optimum buying
and selling dates.
max_profit <- amd_df$close[j]*amd_df$accumulated_shares[j] - sum
amd_df$sell_date <- amd_df$date[j]
amd_df$buy_date <-amd_df$date[i]
}
}
}
##Buy date
buy_date <- amd_df$buy_date[1]
print(buy_date)
##Sell date
sell_date <- amd_df$sell_date[1]
print(sell_date)
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
## profit or loss calculation (sums all the buying of shares with the selling of all shares on final day)
total_profit <- sum(amd_df$costs_proceeds)
print(format(total_profit, nsmall = 3))
##Invested Capital
total_invested_cap <- sum(amd_df$costs_proceeds) - amd_df$costs_proceeds[nrow(amd_df)]
##ROI
ROI <- (total_profit/total_invested_cap)*100
print(ROI)
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Fill your code here
buy_price_tot <- 0
buy_tot <-
average_buy <- 0
percent_dif <- 0
sell_revenue <- 0
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
#initialize the first buy, as stated in step 2's strategy.
amd_df$trade_type[1] <- "buy"
amd_df$accumulated_shares[1] <- share_size
amd_df$costs_proceeds[1] <- (-share_size)*amd_df$close[1]
buy_price_tot <- amd_df$close[1]
buy_tot <- 1
average_buy <- (buy_price_tot/buy_tot)
##This code repeats the buying strategy of Step 2, whilst additionally following the strategy of Option 1 at a ma
ximum 10% increase in share price from the average buy price before selling 50% of all shares owned.
##Each step is attempted to be independent of running other steps before, hence repeating some previous code.
for (i in 2:nrow(amd_df)) {
##The flag for the 'Sell then Buy' becomes reset at each new date.
flag <- 0
##This determines at each share price whether to sell 50% or not.
if (amd_df$close[i] >= 1.1*average_buy && average_buy != 0) {
amd_df$trade_type[i] <- "Sell"
##Adjusts the flag to 1, which in the case of buying on the same date as selling will result in a differe
nt costs_proceeds calculation
flag <- 1
##
sell_revenue <-amd_df$accumulated_shares[i]*0.5*amd_df$close[i]
amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]*0.5
amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
} else {
##Ensures the amount of shares is carried consistent through each date despite selling or not.
amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
amd_df$costs_proceeds[i] <- 0
}
if (amd_df$close[i] < amd_df$close[i-1]) {
buy_price_tot <- buy_price_tot + amd_df$close[i]
buy_tot <- buy_tot + 1
average_buy <- (buy_price_tot/buy_tot)
if (flag == 1) {
amd_df$trade_type[i] <- "Sell then buy"
amd_df$costs_proceeds[i] <- amd_df$costs_proceeds[i] + (-100)*amd_df$close[i]
} else {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- (-share_size)*amd_df$close[i]
}
amd_df$accumulated_shares[i] <- share_size + amd_df$accumulated_shares[i]
}
if (i == nrow(amd_df)) {
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- (amd_df$accumulated_shares[i])*(amd_df$close[i])
}
}
##STEP 5 (Option 1) Profit
print(sum(amd_df$costs_proceeds))

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
##Step 3 finds a optimum period to start buying. Then by following the step 2 trading strategy finding a date to
sell all accumulative shares.
##Despite attempts to make each step independent, step 3 must be run directly before this step to calculate the m
ax profit value.
##Buy date
buy_date <- amd_df$buy_date[1]
print(buy_date) ##2019-05-20
##Sell date
sell_date <- amd_df$sell_date[1]
print(sell_date) ##2024-03-07
##ROI of step 3 ( adjusted buying and selling dates)
new_capital <- 0
i <- 2
while (!is.na(amd_df$date[i]) && (amd_df$date[i] != sell_date) ){
##To find the capital invested of step 3 after the modified step 5 capital.
if(amd_df$close[i] < amd_df$close[i-1]) {
amd_df$costs_proceeds[i] <- (-100)*amd_df$close[i]
}
new_capital = new_capital + amd_df$costs_proceeds[i]
i = i + 1
}
ROI_NEW <- (max_profit/-new_capital)*100
##ROI new method
print(ROI_NEW) ##247.14%
##Profit
print(max_profit) ##$12513696
##Original values (STEP 2)
print(ROI) ##84.15037%
print(total_profit) ##$4644710


##Discussion
"On March 7, 2024, the share price of AMD peaked at $211.38 following some previous advancements within the compa
ny's technology and products. AMD are a company within the technology industry primarily producing GPU's, thus ca
n account for the steadily increasing share price since 2019 as the industry becomes more and more predominate. T
he AMD CEO Lisa Su launched a new graphics processor unit designed for the evolving AI industry in early December
2023, with commitments from renowned companies such as Meta and Microsoft. As a result investors began to believe
in AMD's innovation and hence driving the share price to the peak in March 2024. My first strategy (step 2) earne
d a profit of $4644710 and ROI of 84.15% from 2019-05-20 to 2024-05-17. Comparatively my second strategy earned a
profit of $12513696 and ROI of 247.14%, resulting in a 2.94 times better ROI and $7868986 more profit with a comp
letion date of 2024-03-07."
```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




