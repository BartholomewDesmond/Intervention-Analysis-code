from cmath import sqrt
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
df = pd.read_excel(r'C:\DESMOND  2\Python tutorials\GDP.xlsx','sheet2')

# Plotting
import matplotlib as plt
from matplotlib import style
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
fig, ax = plt.subplots(figsize=(12,6))

# Plot covid19 data
ax.scatter(df["Day"], df["Cases"], facecolors='none', edgecolors='steelblue', label="Nigerian COVID19 daily cases data", linewidths=2)

# Plot model mean covid19 cases prediction
ax.plot(df["Day"][:start], y_pred[:start], 'b-', label="model prediction using OLS")
ax.plot(df["Day"][start:], y_pred[start:], 'b-')

# Plot counterfactual mean covid19 cases with 95% confidence interval
ax.plot(df["Day"][start:], cf['mean'][start:], 'k.', label="counterfactual")
ax.fill_between(df["Day"][start:], cf['mean_ci_lower'][start:], cf['mean_ci_upper'][start:], color='k', alpha=0.1, label="counterfactual 95% CI");

# Plot line marking intervention moment
ax.axvline(x = 372, color = 'r', label = 'intervention')

ax.legend(loc='best')
plt.ylim([0, 4100])
plt.xlim([1,800])
plt.xlabel("Days")
plt.ylabel("Nigerian COVID19 daily cases (%)");

#Let’s plot the distribution of residuals:
res.resid.plot()
plt.show()

#Let’s plot the residuals to see if we can observe this autocorrelation:
import altair as alt

rules = alt.Chart(pd.DataFrame({
  'residuals': [0.0],
  'color': ['black']
})).mark_rule().encode(
  y='residuals',
  color=alt.Color('color:N', scale=None)
)
residual_plot = alt.Chart(res.resid).mark_point().encode(
    x=alt.X('Days'),
    y=alt.Y('residuals')
)
rules + residual_plot 

#Autoregressive model solution
#To assess how much an observation correlates with past observations 
# it is useful to do an autocorrelation plot as shown below:
from statsmodels.tsa.stattools import adfuller
def stationarity_check(ts):
            
    # Calculate rolling statistics
    roll_mean = ts.rolling(window=8, center=False).mean()
    roll_std = ts.rolling(window=8, center=False).std()

    # Perform the Dickey Fuller test
    dftest = adfuller(ts) 
    
    # Plot rolling statistics:
    fig = plt.figure(figsize=(12,6))
    orig = plt.plot(ts, color='blue',label='Original')
    mean = plt.plot(roll_mean, color='red', label='Rolling Mean')
    std = plt.plot(roll_std, color='green', label = 'Rolling Std')
    plt.legend(loc='best')
    plt.title('Rolling Mean & Standard Deviation')
    plt.show()
    
    # Print Dickey-Fuller test results

    print('\nResults of Dickey-Fuller Test: \n')

    dfoutput = pd.Series(dftest[0:4], index=['Test Statistic', 'p-value', 
                                             '#Lags Used', 'Number of Observations Used'])
    for key, value in dftest[4].items():
        dfoutput['Critical Value (%s)'%key] = value
    print(dfoutput)

stationarity_check(df.Cases)
df['first_difference'] = df.Cases.diff()
stationarity_check(df.first_difference.dropna(inplace=False))

#Plot the ACF and PACF
fig = plt.figure(figsize=(12,8))
ax1 = fig.add_subplot(211)
fig = sm.graphics.tsa.plot_acf(df.first_difference.iloc[20:], lags=40, ax=ax1)
ax2 = fig.add_subplot(212)
fig = sm.graphics.tsa.plot_pacf(df.first_difference.iloc[20:], lags=40, ax=ax2, method='ywm')

#df['seasonal_first_difference'] = df.first_difference - df.first_difference.shift(12)
#stationarity_check(df.seasonal_first_difference.dropna(inplace=False))
#modeling with ARIMA
from statsmodels.tsa.arima.model import ARIMA
from pmdarima.arima import auto_arima
arima_results3 = ARIMA(df["Cases"], df[["Season","Day","Intervention","DayAfter"]], order=(1,1,4)).fit()
print(arima_results3.summary())
arima_results3.plot_diagnostics()
arima_results1 = ARIMA(df["Cases"], df[["Season","Day","Intervention","DayAfter"]], order=(2,1,4)).fit()
print(arima_results1.summary())
arima_results1.plot_diagnostics()
arima_results = ARIMA(df["Cases"], df[["Season","Day","Intervention","DayAfter"]], order=(0,1,4)).fit()
print(arima_results.summary())
arima_results.plot_diagnostics()
arima_results2 = ARIMA(df["Cases"], df[["Season","Day","Intervention","DayAfter"]], order=(4,1,4)).fit()
print(arima_results2.summary())
arima_results2.plot_diagnostics()
#The models estimates (with counterfactual projections) can be seen in the chart below:
from statsmodels.tsa.arima.model import ARIMA
start = 372
end = 752

predictions = arima_results2.get_prediction(0, end-1)
summary = predictions.summary_frame(alpha=0.05)

arima_cf = ARIMA(df["Cases"][:start], df["Day"][:start], order=(4,1,4)).fit()

# Model predictions means
y_pred = predictions.predicted_mean

# Counterfactual mean and 95% confidence interval
y_cf = arima_cf.get_forecast(380, exog=df["Day"][start:]).summary_frame(alpha=0.05)

# Plot section
plt.style.use('seaborn-whitegrid')
fig, ax = plt.subplots(figsize=(12,6))

# Plot covid19 data
ax.scatter(df["Day"], df["Cases"], facecolors='none', edgecolors='steelblue', label="Nigerian COVID-19 data", linewidths=2)

# Plot model mean bounce prediction
ax.plot(df["Day"][:start], y_pred[:start], 'b-', label="model prediction using ARIMA(4,1,4)")
ax.plot(df["Day"][start:], y_pred[start:], 'b-')

# Plot counterfactual mean bounce rate with 95% confidence interval
ax.plot(df["Day"][start:], y_cf["mean"], 'k.', label="counterfactual")
ax.fill_between(df["Day"][start:], y_cf['mean_ci_lower'], y_cf['mean_ci_upper'], color='k', alpha=0.1, label="counterfactual 95% CI");


# Plot line marking intervention moment
ax.axvline(x = 372, color = 'r', label = 'intervention')

ax.legend(loc='best')
plt.ylim([0, 4200])

plt.xlim([1,800])
plt.xlabel("Days")
plt.ylabel("Nigerian COVID-19 daily cases (%)");

#Let’s now take a look at residuals qqplot to check if they follow a normal distribution:
import scipy as sp
from statsmodels.graphics.gofplots import qqplot

fig, (ax1, ax2) = plt.subplots(1,2, figsize=(12,6))
sm.qqplot(res.resid, sp.stats.t, fit=True, line="45", ax=ax1);
ax1.set_title("OLS qqplot");

sm.qqplot(arima_results2.resid, sp.stats.t, fit=True, line="45", ax=ax2);
ax2.set_title("ARIMA qqplot");
plt.show();

