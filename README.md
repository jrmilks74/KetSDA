# Kettering SDA Church Attendance Dashboard

An interactive **Shiny web application** to track and forecast weekly attendance at Kettering SDA Church.  
The app combines historical attendance records (via Google Sheets) with statistical models to provide insights and predictions.

---

## ✨ Features

- **Historical Trends**
  - Interactive tabs for **Overall**, **Ascent**, and **Sanctuary** services.
  - Weekly and year-to-date attendance summaries.
  - Seasonal patterns and year-over-year comparisons.
  - Clear tables with percent changes and deltas.

- **Forecasting**
  - Predicts weekly attendance for up to **6 months ahead**.
  - Uses a **performance-weighted ensemble** of:
    - Prophet (with holiday effects),
    - Auto ARIMA,
    - Exponential Smoothing (ETS).
  - Holidays and special weekends included:
    - Easter
    - Christmas
    - Memorial Day weekend
    - Academy Graduation weekend
  - Handles missing weeks (e.g. weather cancellations, combined services).
  - Displays **80% confidence intervals** around forecasted values.

- **Interactive Visualization**
  - Powered by `plotly` for zooming, hovering, and toggling series.
  - Confidence intervals shown as shaded ribbons on forecasts.

---

## 📦 Requirements

R (≥ 4.2) with the following packages:

```r
shiny
tidyverse
lubridate
scales
timetk
plotly
knitr
kableExtra
googlesheets4
modeltime
parsnip
workflows
rsample
yardstick
imputeTS
timeDate
modeltime.ensemble   # optional but recommended
