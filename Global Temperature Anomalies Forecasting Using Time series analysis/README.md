🌍 Global Temperature Anomaly Forecasting
his project investigates global temperature anomalies — deviations from long-term average temperatures — using statistical models to uncover trends and make future predictions. Temperature anomalies are a crucial indicator in climate science, allowing us to track and respond to global warming over time.

We analyzed historical data from the National Centers for Environmental Information (NCEI), decomposed the time series, and implemented two forecasting models:

**Used ARMA and Singular Spectrum Analysis models on temperature anomaly data from NOAA to forecast global warming trends;
 evaluated forecasts with RMSE and Diebold-Mariano test to assess forecasting performances.**

    ARMA (AutoRegressive Moving Average)

    SSA (Singular Spectrum Analysis)

Our goal was to determine which model better captures trends, seasonality, and can provide accurate temperature forecasts, with implications for environmental policy.

🌡️ Why Temperature Anomalies?

Unlike raw temperature values, anomalies reflect changes over time and are more stable for comparative analysis. They provide a clearer signal of climate trends by eliminating local seasonal variation and geographic bias.

🎯 Objectives

    Explore and visualize a century of global temperature anomaly data

    Decompose the time series into trend, seasonality, and noise

    Build forecasting models (ARMA and SSA)

    Compare forecast accuracy using:

        Root Mean Square Error (RMSE)

        Diebold-Mariano Test

    Provide policy-relevant insights on global temperature trends

	📁 Data

    Source: National Centers for Environmental Information (NCEI)

    Time span: 1895–2023

    Final dataset used: Last 100 years (1923–2023) for relevance and model consistency
		🧪 Methods
🔹 ARMA Model

    Combines autoregressive and moving average components

    Order selected using AIC, AICc, and BIC (BIC used to avoid overfitting)

    Required differencing to achieve stationarity

🔹 SSA (Singular Spectrum Analysis)

    Non-parametric decomposition of time series

    Identifies trend and seasonality via eigenvalue analysis

    Forecasts reconstructed components

    No assumption of stationarity

	📈 Results Summary

    Both models predict a future decline in global temperature anomalies

    SSA model performed better:

        Lower RMSE

        Diebold-Mariano test confirmed statistical significance

    SSA also revealed seasonal components not visible in ARMA

📌 Key Insight:
Although anomalies may decline, they remain positive, meaning temperatures are still above the long-term average — highlighting the continued urgency of climate action.

🔍 Limitations & Future Work

    Time series models alone can't fully explain causal factors of climate change

    Future enhancements:

        Include external variables (e.g., CO₂ emissions, fossil fuel use, deforestation)

        Combine with regression or machine learning models for multivariate forecasting

        Regional breakdowns of anomalies
