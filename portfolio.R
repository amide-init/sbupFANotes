#function to check and install a single package
check_and_install <- function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#Load all required packages 
check_and_install("PerformanceAnalytics") # Performance metrics and visualization 
check_and_install("PortfolioAnalytics")  # For portfolio optimization 
check_and_install("ROI") # Optimization infrastructure
check_and_install("ROI.plugin.glpk")  # Linear programming plugin for ROI
check_and_install("ROI.plugin.quadprog")  # Quadratic programming plugin for ROI
check_and_install("quantmod") #fetching stock data

################################################################################
# Fetch Stock Data
################################################################################

# Load historical stock prices
symbols <- c("AAPL", "GOOG", "MSFT")  # Stock symbols
start_date <- "2020-01-01"            # Start date for historical data
end_date <- Sys.Date()                # End date as today's date

# Fetch adjusted closing prices
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)
tail(AAPL)
# Combine closing prices into one dataset
stock_data <- na.omit(merge(Cl(AAPL), Cl(GOOG), Cl(MSFT)))
head(stock_data)
colnames(stock_data) <- symbols  # Rename columns to stock symbols
head(stock_data)  # Preview data


################################################################################
# Calculate Daily Return
################################################################################

# Calculate daily returns
stock_returns <- na.omit(Return.calculate(stock_data))
head(stock_returns)
# View summary statistics of returns
summary(stock_returns)


################################################################################
# Visualize Risk and Return
################################################################################

# Calculate mean returns and risk (standard deviation)
mean_returns <- colMeans(stock_returns)
risk <- apply(stock_returns, 2, sd)
print(risk)
# Plot risk vs. return
plot(risk, mean_returns, 
     xlab = "Risk (Standard Deviation)", 
     ylab = "Return (Mean)", 
     main = "Risk vs. Return of Stocks",
     pch = 19, col = "blue")
text(risk, mean_returns, labels = colnames(stock_returns), pos = 4, cex = 0.8)


################################################################################
# Define a Portfolio Optimization Problem
################################################################################


# Initialize portfolio object
portfolio <- portfolio.spec(assets = colnames(stock_returns))

# Add constraints
portfolio <- add.constraint(portfolio, type = "full_investment")  # Fully invest capital
portfolio <- add.constraint(portfolio, type = "long_only")        # No short selling

# Add objectives
portfolio <- add.objective(portfolio, type = "return", name = "mean")  # Maximize return
portfolio <- add.objective(portfolio, type = "risk", name = "StdDev")  # Minimize risk


################################################################################
# Optimize the Portfolio
################################################################################

# Optimize the portfolio
optimized_portfolio <- optimize.portfolio(R = stock_returns, 
                                          portfolio = portfolio, 
                                          optimize_method = "ROI")
print(optimized_portfolio)
# Extract optimal weights
optimal_weights <- extractWeights(optimized_portfolio)

# Print results
print(optimized_portfolio)
print("Optimal Weights:")
print(optimal_weights)

################################################################################
# Analyze the Efficient Frontier 
################################################################################

# Generate the efficient frontier
efficient_frontier <- create.EfficientFrontier(R = stock_returns, portfolio = portfolio, 
                                               type = "mean-StdDev")

# Plot the efficient frontier
chart.EfficientFrontier(efficient_frontier, 
                        match.col = "StdDev", 
                        type = "l", 
                        main = "Efficient Frontier")


################################################################################
#  Backtest the Optimized Portfolio
################################################################################

# Calculate portfolio returns
portfolio_returns <- Return.portfolio(R = stock_returns, weights = optimal_weights)

# Plot cumulative returns
chart.CumReturns(portfolio_returns, 
                 main = "Cumulative Returns of Optimized Portfolio", 
                 legend.loc = "topleft")


################################################################################
#  Evaluate Portfolio Performance
################################################################################

# Sharpe Ratio
sharpe_ratio <- SharpeRatio.annualized(portfolio_returns)
print(paste("Annualized Sharpe Ratio:", sharpe_ratio))

# Sortino Ratio
sortino_ratio <- SortinoRatio(portfolio_returns)
print(paste("Sortino Ratio:", sortino_ratio))












