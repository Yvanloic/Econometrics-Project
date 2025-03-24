# ---- First Step : Data loading and processing ----

  # Install and load the different packages needed
    install.packages("quantmod")  
    library(quantmod)
    install.packages("zoo")  
    library(zoo)
    install.packages("psych")  
    library(psych)

    # Define tickers for indexes
    tickers <- c("^GSPC", "^FTSE", "^FCHI")  # S&P 500, FTSE 100, CAC 40
    
    # Define date range for data
    start_date <- "2004-11-27"
    end_date <- "2024-11-27"
    
    # Define folder path for saving files 
    # PLEASE Update this path to your personal path
    save_path <- "C:/Users/2103020/Downloads/NEOMA FBD Courses/Econometrics & Time series/"  
    
    # Initialize empty lists for raw and processed data
    stock_data <- list()
    processed_data <- list()
    
    #Download and save raw data
    for (ticker in tickers) {
      # Download data from Yahoo Finance
      data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      
      data <- data.frame(Date = index(data), coredata(data))
      
      # Handle NA values in raw data (Forward-fill, then Backward-fill)
      data <- na.locf(data, na.rm = FALSE)  # Fill NAs forward
      data <- na.locf(data, fromLast = TRUE)  # Fill NAs backward
      
      # Save raw data as CSV (And overwrite if file exists)
      write.csv(data, paste0(save_path, gsub("^\\^", "", ticker), "_Raw_Data.csv"), row.names = FALSE)
      
      # Store raw data in list
      stock_data[[ticker]] <- data
    }
    #View(stock_data)
    # Process data to keep only Adjusted  Price and Volume columns
    for (ticker in tickers) {
      # Load raw data from the list
      data <- stock_data[[ticker]]
      
      # Rename columns to include ticker names
      colnames(data) <- c("Date", paste0(gsub("^\\^", "", ticker), ".", gsub("^.*\\.", "", colnames(data[-1]))))
      
      # Keep only Date, Adjusted Price, and Volume columns
      adjusted_col <- grep("Adjusted", colnames(data))
      volume_col <- grep("Volume", colnames(data))
      data <- data[, c(1, adjusted_col, volume_col)]
      
      # Rename Adjusted to Price
      colnames(data) <- gsub("Adjusted", "Price", colnames(data))
      
      # Handle NA values in processed data (Forward-fill, then Backward-fill)
      data <- na.locf(data, na.rm = FALSE)  # Fill NA forward
      data <- na.locf(data, fromLast = TRUE)  # Fill NA backward
      
      # Save processed data as CSV (overwrite if file exists)
      write.csv(data, paste0(save_path, gsub("^\\^", "", ticker), "_Processed_Data.csv"), row.names = FALSE)
      
      # Store processed data in the list
      processed_data[[ticker]] <- data
    }
    
    # Merge all datasets by Date
    aligned_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = FALSE), processed_data)
  
    # Save aligned data as a single CSV file (overwrite if file exists)
    write.csv(aligned_data, paste0(save_path, "Aligned_Indexes_Processed_Data.csv"), row.names = FALSE)
    
    # Print preview of aligned dataset
    print(head(aligned_data))
    
    # Check the time range of the aligned data
    print(paste("Aligned data starts from", min(aligned_data$Date), "to", max(aligned_data$Date)))

# ---- Task 1. ---- 
    
# ---- Task 1.2 ---- 
    prices <- aligned_data[, grep("\\.Price$", colnames(aligned_data))]
    colnames(prices) <- c("GSPC.Price", "FTSE.Price", "FCHI.Price")  
    
    # Calculate log returns
    log_returns <- apply(prices, 2, function(x) diff(log(x)))
    
    # Add Date column to log returns
    log_returns <- data.frame(Date = aligned_data$Date[-1], log_returns)
    
    # Rename columns for clarity
    colnames(log_returns)[-1] <- c("GSPC.LogReturn", "FTSE.LogReturn", "FCHI.LogReturn")
    
    # Plotting the log returns
    # Time series plots
    par(mfrow = c(1, 1))  # Ensure single plot layout
    
    # Plot for GSPC (S&P 500)
    plot(log_returns$Date, log_returns$GSPC.LogReturn, type = "l", col = "blue",
         main = "Log Returns Over Time - S&P 500", xlab = "Date", ylab = "Log Returns")
    
    # Plot for FTSE (FTSE 100)
    plot(log_returns$Date, log_returns$FTSE.LogReturn, type = "l", col = "red",
         main = "Log Returns Over Time - FTSE 100", xlab = "Date", ylab = "Log Returns")
    
    # Plot for FCHI (CAC 40)
    plot(log_returns$Date, log_returns$FCHI.LogReturn, type = "l", col = "green",
         main = "Log Returns Over Time - CAC 40", xlab = "Date", ylab = "Log Returns")
    
    # Histograms
      par(mfrow = c(1, 3))  # Create a grid of 3 plots
      
      # Define custom titles for each index
      titles <- c("Histogram for S&P 500", "Histogram for FTSE 100", "Histogram for CAC 40")
      
      # Loop through each index to create histograms
      for (i in 2:4) {
        hist(log_returns[, i], 
             main = titles[i - 1],  # Use custom title
             xlab = "Log Returns", 
             col = "lightblue", 
             breaks = 30)
      }
    
    # Boxplots
      par(mfrow = c(1, 1))  # Reset to single plot
      boxplot(log_returns[, -1], names = colnames(log_returns)[-1], main = "Boxplot of Log Returns", 
              col = c("blue", "red", "green"))
      par(mfrow = c(1, 1))  
      
      # Boxplot for GSPC (S&P 500)
      boxplot(log_returns$GSPC.LogReturn, main = "Boxplot of Log Returns - S&P 500", 
              col = "blue", ylab = "Log Returns", xlab = "S&P 500")
      
      # Boxplot for FTSE (FTSE 100)
      boxplot(log_returns$FTSE.LogReturn, main = "Boxplot of Log Returns - FTSE 100", 
              col = "red", ylab = "Log Returns", xlab = "FTSE 100")
      
      # Boxplot for FCHI (CAC 40)
      boxplot(log_returns$FCHI.LogReturn, main = "Boxplot of Log Returns - CAC 40", 
              col = "green", ylab = "Log Returns", xlab = "CAC 40")
    
    # Descriptive statistics
    desc_stats <- describe(log_returns[, -1])
    print(desc_stats)
    
# ---- Task 2. ---- 
    
# ---- Task 2.1 ---- 
    
    # Load necessary libraries
    install.packages("FinTS")
    library(tseries)       # For UnitRootTest and Jarque-Bera Test
    library(vars)          # For VAR model and serial correlation test
    library(FinTS)         # For ARCH test
    library(psych)         # For descriptive statistics
    
    
    
    # ----------- 1. Stationarity Test (Augmented Dickey-Fuller)
    
    # Apply the ADF test for each log-return series
    adf_gspc <- adf.test(log_returns$GSPC.LogReturn)
    adf_ftse <- adf.test(log_returns$FTSE.LogReturn)
    adf_fchi <- adf.test(log_returns$FCHI.LogReturn)
    
    # Print results
    print("ADF Test for S&P 500:")
    print(adf_gspc)
    
    print("ADF Test for FTSE 100:")
    print(adf_ftse)
    
    print("ADF Test for CAC 40:")
    print(adf_fchi)
    
    # ----------- 2. Serial Correlation Test (Ljung-Box for VAR residuals) 
    
    # Determine optimal lag length using various criteria
    lag_selection <- VARselect(log_returns[, -1], lag.max = 5, type = "const")
    
    # View the optimal lags based on different criteria
    print(lag_selection$selection)
    
    # Extract the optimal lag based on SC (Schwarz Criterion)
    optimal_lag <- lag_selection$selection["SC(n)"]
    
    # Check if optimal_lag is numeric and single-valued
    if (is.null(optimal_lag) || length(optimal_lag) != 1) {
      stop("The selected lag for the VAR model is invalid.")
    }
    
    # Fit a VAR model using the optimal lag length
    var_model <- VAR(as.matrix(log_returns[, -1]), p = optimal_lag, type = "const")
    
    # Check stability of the model (roots should be inside the unit circle)
    stability_test <- stability(var_model, type = "OLS-CUSUM")
    plot(stability_test)
    
    # Perform the serial correlation test on VAR residuals
    serial_test_results <- serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")
    
    # Print serial correlation test results
    print(serial_test_results)
    
    # Check for serial correlation in residuals: H0: No Serial Correlation
    # The Portmanteau test is applied across all of these residual series to 
    # assess the overall presence of serial correlation in the VAR model's residuals. 
    # If significant serial correlation is detected (indicated by a small p-value), 
    # it suggests that the model may need additional lags or model refinements.
    # Print results
    print("Serial Correlation Test for VAR Residuals:")
    print(serial_test_results)
    
    # Display VAR model summary
    summary(var_model)
    
  
    # ----------- 3. Homoscedasticity Test (ARCH Test)
    
    # Perform the ARCH test for each log-return series
    arch_gspc <- ArchTest(log_returns$GSPC.LogReturn, lags = 5)
    arch_ftse <- ArchTest(log_returns$FTSE.LogReturn, lags = 5)
    arch_fchi <- ArchTest(log_returns$FCHI.LogReturn, lags = 5)
    
    # Print results
    print("ARCH Test for S&P 500:")
    print(arch_gspc)
    
    print("ARCH Test for FTSE 100:")
    print(arch_ftse)
    
    print("ARCH Test for CAC 40:")
    print(arch_fchi)
    
    # ----------- 4. Normality Test (Jarque-Bera) 
    
    # Perform the Jarque-Bera test for each log-return series
    jb_gspc <- jarque.bera.test(log_returns$GSPC.LogReturn)
    jb_ftse <- jarque.bera.test(log_returns$FTSE.LogReturn)
    jb_fchi <- jarque.bera.test(log_returns$FCHI.LogReturn)
    
    # Print results
    print("Jarque-Bera Test for S&P 500:")
    print(jb_gspc)
    
    print("Jarque-Bera Test for FTSE 100:")
    print(jb_ftse)
    
    print("Jarque-Bera Test for CAC 40:")
    print(jb_fchi)
    
   
# ---- Task 2.2 ----  
    
    # Ensure required libraries are installed and loaded
    install.packages("forecast")
    library(forecast)
    
    # Plot ACF and PACF for each index log-return series
    par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))  # Set a 2x3 plotting layout for ACF and PACF side by side
    
    # ACF and PACF for S&P 500 Log-Returns
    acf(log_returns$GSPC.LogReturn, main = "ACF - S&P 500")
    pacf(log_returns$GSPC.LogReturn, main = "PACF - S&P 500")
    
    # ACF and PACF for FTSE 100 Log-Returns
    acf(log_returns$FTSE.LogReturn, main = "ACF - FTSE 100")
    pacf(log_returns$FTSE.LogReturn, main = "PACF - FTSE 100")
    
    # ACF and PACF for CAC 40 Log-Returns
    acf(log_returns$FCHI.LogReturn, main = "ACF - CAC 40")
    pacf(log_returns$FCHI.LogReturn, main = "PACF - CAC 40")
    
    par(mfrow = c(1, 1))  # Reset plotting layout to default
    
    
    #Let we estimate Select and estimate ARMA models for log-returns
    gspc_arma <- arima(log_returns$GSPC.LogReturn, order = c(0, 0, 1))
    show(gspc_arma)
    coeftest(gspc_arma) 
    BIC(gspc_arma)
    
    ftse_ma <- arima(log_returns$FTSE.LogReturn, order = c(0, 0, 1))
    show(ftse_ma)
    coeftest(ftse_ma) 
    BIC(ftse_ma)
    
    fchi_ma <- arima(log_returns$FCHI.LogReturn, order = c(0, 0, 1))
    show(fchi_ma)
    coeftest(fchi_ma) 
    BIC(fchi_ma)
    
 
# ---- Task 2.3 ----   
    
      install.packages("rugarch")
      library(rugarch)
  
      # Create our ARMA-GARCH model structure : We use the order ARMA(0,1), as per the analysis of the previous models.
      
      
      arma.garch <- ugarchspec(mean.model = list(armaOrder = c(1, 0)),
                               variance.model = list(garchOrder = c(1, 1)),
                               distribution.model = "norm")
      
      # Fit GARCH(1,1) for S&P 500
      gspc.garch <- ugarchfit(data = log_returns$GSPC.LogReturn, spec = arma.garch)
      show(gspc.garch)
      
      # Fit GARCH(1,1) for FTSE 100
      ftse.garch <- ugarchfit(data = log_returns$FTSE.LogReturn, spec = arma.garch)
      show(ftse.garch)
      
      # Fit GARCH(1,1) for CAC 40
      fchi.garch <- ugarchfit(data = log_returns$FCHI.LogReturn, spec = arma.garch)
      show(fchi.garch)
      
      ### Coefficients interpretation GARCH:
      
      #mu      Constant of the conditional mean equation
      #ar1     AR(1) of the conditional mean equation
      #omega   Constant of the conditional variance equation; 
      #        Represents the long-term or baseline variance level 
      #alpha1  The coefficient of the lagged squared residual e^2(t-1)
      #        It represents the impact of recent shocks to the time series on current 
      #        volatility. A significative alpha1 implies that recent shocks (or news) 
      #        have a significant impact on current volatility.It can be regarded as 
      #        the "news" or "short-term memory" effect. 
      #beta1   The coefficient of the lagged variance sigma^2(t-1), which represents 
      #        the persistence of past volatility. A high beta1 indicates that volatility 
      #        persists over time (i.e., periods of high volatility tend to be followed 
      #        by periods of high volatility).
  
  
      # Create our Asymmetric GJR ARMA-GARCH model structure
      arma.gjr= ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                           variance.model = list(model = "gjrGARCH", 
                                                 garchOrder = c(1,1)), distribution.model = "norm")
      
      gspc.arma.gjr = ugarchfit(data=log_returns$GSPC.LogReturn, arma.gjr)
      show(gspc.arma.gjr)
      
      ftse.arma.gjr = ugarchfit(data=log_returns$FTSE.LogReturn, arma.gjr)
      show(ftse.arma.gjr)
      
      fchi.arma.gjr = ugarchfit(data=log_returns$FCHI.LogReturn, arma.gjr)
      show(fchi.arma.gjr)
      
      
      # Create our Asymmetric EGARCH ARMA-GARCH model structure
      arma.EGARCH= ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                              variance.model = list(model = "eGARCH", 
                                                    garchOrder = c(1,1)), distribution.model = "norm")
      
      gspc.arma.EGARCH = ugarchfit(data=log_returns$GSPC.LogReturn, arma.EGARCH)
      show(gspc.arma.EGARCH)
      
      ftse.arma.EGARCH = ugarchfit(data=log_returns$FTSE.LogReturn, arma.EGARCH)
      show(ftse.arma.EGARCH)
      
      fchi.arma.EGARCH = ugarchfit(data=log_returns$FCHI.LogReturn, arma.EGARCH)
      show(fchi.arma.EGARCH)
      
      # Create our Asymmetric GJR ARMA-GARCH model structure
      arma.gjr= ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                           variance.model = list(model = "gjrGARCH", 
                                                 garchOrder = c(1,1)), distribution.model = "norm")
      
      gspc.arma.gjr = ugarchfit(data=log_returns$GSPC.LogReturn, arma.gjr)
      show(gspc.arma.gjr)
      
      ftse.arma.gjr = ugarchfit(data=log_returns$FTSE.LogReturn, arma.gjr)
      show(ftse.arma.gjr)
      
      fchi.arma.gjr = ugarchfit(data=log_returns$FCHI.LogReturn, arma.gjr)
      show(fchi.arma.gjr)
 
    
# ---- Task 2.4 ----  
    
    # Step 1: Load Necessary Libraries
    # Install and load the required packages
    install.packages("rugarch")
    install.packages("rmgarch")
    library(rugarch)
    library(rmgarch)
    
    # Step 2: Define the Univariate GARCH Specification
    # Univariate GARCH (1,1) Specification for all series
    garch.spec = ugarchspec(
      mean.model = list(armaOrder = c(1, 0)),  # ARMA(1,0)
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      distribution.model = "norm"  # Normal distribution
    )
    
    #Step 3: Define the DCC Specification
    # Specify the DCC-GARCH model : The DCC model combines the individual GARCH models into a multivariate framework.
    dcc.spec = dccspec(
      uspec = multispec(replicate(3, garch.spec)),  # Univariate GARCH specs for 3 series
      dccOrder = c(1, 1),  # DCC(1,1)
      distribution = "mvnorm"  # Multivariate normal distribution
    )
    
    #Step 4: Fit the DCC Model
    # Combine the series into a matrix
    data_matrix = cbind(log_returns$GSPC.LogReturn, 
                    log_returns$FTSE.LogReturn, 
                    log_returns$FCHI.LogReturn)

    # Fit the DCC-GARCH model
    dcc.fit = dccfit(dcc.spec, data = data_matrix)

    # Show the results
    show(dcc.fit)

    # Extract conditional correlations
    dcc.correlations = rcor(dcc.fit)
    
    # Plot conditional correlations
    par(mfrow = c(3, 1))  # Set up 3 plots
    plot(dcc.correlations[1, 2, ], type = "l", col = "blue",
         main = "Conditional Correlation: GSPC & FTSE",
         xlab = "Time", ylab = "Correlation")
    plot(dcc.correlations[1, 3, ], type = "l", col = "red",
         main = "Conditional Correlation: GSPC & FCHI",
         xlab = "Time", ylab = "Correlation")
    plot(dcc.correlations[2, 3, ], type = "l", col = "green",
         main = "Conditional Correlation: FTSE & FCHI",
         xlab = "Time", ylab = "Correlation")
    
# ---- Task 3. ----    
    
# ---- Task 3.1: Johansen Test for Cointegration ---- 
    # Extract price series as a time series object
    price_series_ts <- ts(prices, start = c(2004, 11), frequency = 252)
    
    # Transform variables to log levels
    log_price_series_ts <- log(price_series_ts)
    
    # Plot the log-transformed price series to determine the type of test to perform
    par(mfrow = c(3, 1))
    plot(log(price_series_ts[, 1]), type = "l", col ="blue", main = "Log-Transformed GSPC Price", xlab = "Time", ylab = "Log Price")
    plot(log(price_series_ts[, 2]), type = "l", col = "red", main = "Log-Transformed FTSE Price", xlab = "Time", ylab = "Log Price")
    plot(log(price_series_ts[, 3]), type = "l", col = "green", main = "Log-Transformed FCHI Price", xlab = "Time", ylab = "Log Price")
    
    # Step 1: Test for stationarity on log levels
    adf_gspc_log <- ur.df(log_price_series_ts[, 1], type = "trend", selectlags = "BIC")
    adf_ftse_log <- ur.df(log_price_series_ts[, 2], type = "trend", selectlags = "BIC")
    adf_fchi_log <- ur.df(log_price_series_ts[, 3], type = "trend", selectlags = "BIC")
    print("ADF Test Results for GSPC (Log Levels):")
    print(summary(adf_gspc_log))
    print("ADF Test Results for FTSE (Log Levels):")
    print(summary(adf_ftse_log))
    print("ADF Test Results for FCHI (Log Levels):")
    print(summary(adf_fchi_log))
    
    # Calculate first differences of log levels
    diff_log_gspc <- diff(log_price_series_ts[, 1])
    diff_log_ftse <- diff(log_price_series_ts[, 2])
    diff_log_fchi <- diff(log_price_series_ts[, 3])
    
    # Perform ADF test on first differences of log levels
    adf_gspc_diff_log <- ur.df(diff_log_gspc, type = "trend", selectlags = "BIC")
    adf_ftse_diff_log <- ur.df(diff_log_ftse, type = "trend", selectlags = "BIC")
    adf_fchi_diff_log <- ur.df(diff_log_fchi, type = "trend", selectlags = "BIC")
    
    # Print results for first differences
    print("ADF Test Results for GSPC (Log Differences):")
    print(summary(adf_gspc_diff_log))
    print("ADF Test Results for FTSE (Log Differences):")
    print(summary(adf_ftse_diff_log))
    print("ADF Test Results for FCHI (Log Differences):")
    print(summary(adf_fchi_diff_log))
    
    # Step 2: Determine optimal lag length for log levels
    lag_selection <- VARselect(log_price_series_ts, lag.max = 10, type = "trend") #Here by using type = "both" or type = "trend", we found lag =2. But by using "const" we find 3. 
    print("Lag Selection Criteria:")
    print(lag_selection)
    optimal_lag <- lag_selection$selection["SC(n)"]
    print(paste("Optimal Lag (SC):", optimal_lag))
    
    # Step 3: Perform the Johansen test on log levels
    
    johansen_test <- ca.jo(log_price_series_ts, type = "trace", ecdet = "trend", K = optimal_lag)
    print("Johansen Test Results (Trace Statistic, Trend):")
    print(summary(johansen_test))
    
    
# ---- Task 3.3: Construct and Interpret the VECM for Cointegrated Series ---- 
    #As we don't have cointegration, we built a Var model by taking the first differnce of the log variables.
    
    # 1. Construct VAR model
    var_model_task3 <- VAR(cbind(diff_log_gspc, diff_log_ftse, diff_log_fchi), p = optimal_lag, type = "const")
    summary(var_model_task3)
    
    
    # 2. Impulse Response Functions (IRFs)
    irf_results <- irf(var_model_task3, impulse = "diff_log_gspc", response = c("diff_log_ftse", "diff_log_fchi"), n.ahead = 10, boot = TRUE)
    plot(irf_results)
    
    # 3. Variance Decomposition (FEVD)
    var_decomp <- fevd(var_model_task3, n.ahead = 10)
    print(var_decomp)

# ---- Task 3.4 ---- 
    #Interpretations on the written report. 

#---- Task 4. ----   
    
# ---- Task 4.1 ----   
    
    aligned_data_for_var <- aligned_data
    #Merge data : CAC 40 log-returns, log-volume, and absolute value of the log-returns
    # Extract CAC 40 log-returns
    cac40_log_returns <- log_returns[, c("Date", "FCHI.LogReturn")]
    #head(cac40_log_returns)
    #any(is.na(cac40_log_returns))
    
    # Calculate absolute value of log-returns (proxy for volatility)
    cac40_log_returns$AbsLogReturns <- abs(cac40_log_returns$FCHI.LogReturn)
    #head(cac40_log_returns)
    
    # Compute log-volume from FCHI.Volume
    # Add a small constant (1e-6) to avoid taking the logarithm of 0, which would result in -Inf
    cac40_log_returns$LogVolumeFCHI_VAR <- c(diff(log(aligned_data_for_var$FCHI.Volume + 1e-6)))
    
    var_data <- cac40_log_returns
    head(var_data)
    any(sapply(var_data, function(x) any(is.infinite(x)))) #check Inf values
    any(is.na(var_data)) #check NA values
   
    
    # Load necessary library
    library(vars)
    
    # Convert the data to time series format
    #final_data$Date <- as.Date(final_data$Date)
    #ts_data <- ts(final_data[, -1], frequency = 1)  # Remove the Date column, convert to time series
    
    var_matrix <- as.matrix(var_data[, -1])  # Cancel the Date column
    
    
    # Step 1: Lag selection using VARselect
    lag_selection <- VARselect(var_matrix, lag.max = 5, type = "const")
    print(lag_selection$criteria)
    
    # Optimal lag length based on criteria
    optimal_lag <- lag_selection$selection["AIC(n)"]
    
    # Step 2: Fit the VAR model with the selected lag
    var_model <- VAR(var_matrix, p = optimal_lag, type = "const")
    
    # Step 3: Summary of the model
    summary(var_model)
    
    # Step 4: Goodness of fit (R-squared for each equation)
    R2 <- sapply(var_model$varresult, function(x) summary(x)$r.squared)
    print(R2)
    
    # Step 5: Diagnostics
    # Check residuals for serial correlation
    serial_test <- serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")
    print(serial_test)
    
    # Check residuals for normality
    normality_test <- normality.test(var_model)
    print(normality_test)
    
    # Check stability of the VAR model
    stability_test <- stability(var_model, type = "OLS-CUSUM")  # Specify 'type' explicitly
    plot(stability_test)
    
    
# ---- Task 4.2 ----
    
    # Test if FCHI.LogReturn Granger-causes AbsLogReturns and LogVolumeFCHI
    granger_test_LogReturn <- causality(var_model, cause = "FCHI.LogReturn")
    print(granger_test_LogReturn)
    
    # Test if AbsLogReturns Granger-causes FCHI.LogReturn and LogVolumeFCHI
    granger_test_AbsLogReturns <- causality(var_model, cause = "AbsLogReturns")
    print(granger_test_AbsLogReturns)
    
    # Test if LogVolumeFCHI Granger-causes LogReturn and AbsLogReturns
    granger_test_LogVolumeFCHI <- causality(var_model, cause = "LogVolumeFCHI_VAR")
    print(granger_test_LogVolumeFCHI)
    
# ---- Task 4.3 ----
    
    # Ensure required library is loaded
    library(strucchange)
    
    # Extract residuals from the VAR model
    var_residuals <- residuals(var_model)
    
    colnames(var_residuals) <- c("FCHI_LogReturn", "AbsLogReturns", "LogVolumeFCHI_VAR")
    
    # Perform CUSUM test on each residual series
    cusum_test_log_returns <- efp(var_residuals[, "FCHI_LogReturn"] ~ 1, type = "Rec-CUSUM")
    cusum_test_abs_returns <- efp(var_residuals[, "AbsLogReturns"] ~ 1, type = "Rec-CUSUM")
    cusum_test_log_volume <- efp(var_residuals[, "LogVolumeFCHI_VAR"] ~ 1, type = "Rec-CUSUM")
    
    # Plot CUSUM test for FCHI log returns
    par(mfrow = c(1, 1))  # Ensure single plot layout
    plot(cusum_test_log_returns, 
         main = "CUSUM Test - FCHI Log Returns", 
         col = "blue", 
         ylab = "Recursive Residuals", 
         xlab = "Time")
    
    # Plot CUSUM test for absolute log returns
    par(mfrow = c(1, 1))  # Reset to single plot layout
    plot(cusum_test_abs_returns, 
         main = "CUSUM Test - Absolute Log Returns", 
         col = "red", 
         ylab = "Recursive Residuals", 
         xlab = "Time")
    
    # Plot CUSUM test for log volume
    par(mfrow = c(1, 1))  # Reset to single plot layout
    plot(cusum_test_log_volume, 
         main = "CUSUM Test - Log Volume", 
         col = "green", 
         ylab = "Recursive Residuals", 
         xlab = "Time")
    
    # Summarize the results for each series
    summary_CUSUM_log_returns <- summary(cusum_test_log_returns)
    summary_CUSUM_abs_returns <- summary(cusum_test_abs_returns)
    summary_CUSUM_log_volume <- summary(cusum_test_log_volume)
    
    # Print summaries
    print("Summary for CUSUM Test - FCHI Log Returns:")
    print(summary_CUSUM_log_returns)
    print("Summary for CUSUM Test - Absolute Log Returns:")
    print(summary_CUSUM_abs_returns)
    print("Summary for CUSUM Test - Log Volume:")
    print(summary_CUSUM_log_volume)
    
# ---- Task 4.4 ----
    
    #Step 1: Load necessary libraries
    install.packages("svars")
    library(vars)
    library(svars)
    
    # Step 2: Convert VAR model to Structural VAR (SVAR) with given ordering
    # Use the causal ordering: log-returns, volatility and log-volume
    colnames(var_model$y) #check the order of the columns, important for having the svar model
    svar_model_original <- id.chol(var_model, order = c("FCHI.LogReturn", "AbsLogReturns", "LogVolumeFCHI_VAR"))
    
    # Compute IRFs for original ordering
    irf_svar_original <- irf(svar_model_original, n.ahead = 10, boot = TRUE)
    
    # Plot IRFs for original ordering
    plot(irf_svar_original, main = "IRFs - Original Ordering")
    
    # Step 4: Test Alternative Causal Orderings
    # Alternative ordering 1: Log-volume → Log-returns → Volatility
    svar_model_alt1 <- id.chol(var_model, order = c("LogVolumeFCHI_VAR", "FCHI.LogReturn", "AbsLogReturns"))
    
    # Compute IRFs for alternative ordering 1
    irf_svar_alt1 <- irf(svar_model_alt1, n.ahead = 10, boot = TRUE)
    
    # Plot IRFs for alternative ordering 1
    plot(irf_svar_alt1, main = "IRFs - Alternative Ordering 1")
    
    # Alternative ordering 2: Volatility → Log-volume → Log-returns
    svar_model_alt2 <- id.chol(var_model, order = c("AbsLogReturns", "LogVolumeFCHI_VAR", "FCHI.LogReturn"))
    
    # Compute IRFs for alternative ordering 2
    irf_svar_alt2 <- irf(svar_model_alt2, n.ahead = 10, boot = TRUE)
    
    # Plot IRFs for alternative ordering 2
    plot(irf_svar_alt2, main = "IRFs - Alternative Ordering 2")
    