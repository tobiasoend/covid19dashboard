rm(list=ls())

library(httr)
library(jsonlite)
library(coronavirus)
library(forecast)

data(coronavirus)
update_datasets()

df_daily <- coronavirus %>%
    dplyr::filter(Country.Region == "Denmark") %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
        names_from = type,
        values_from = total
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    #dplyr::mutate(active = confirmed - death - recovered) %>%
    dplyr::mutate(active = confirmed - death) %>%
    dplyr::mutate(
        confirmed_cum = cumsum(confirmed),
        death_cum = cumsum(death),
        # recovered_cum = cumsum(recovered),
        active_cum = cumsum(active)
    )


min_data <- min(df_daily$date)
max_data <- max(df_daily$date)
forecast_length = 7

TS_data <- ts(df_daily[,c(1,6)], frequency = 7)
TS_fit <- auto.arima(TS_data[,2])
TS_fc <- forecast(TS_fit, h=forecast_length)

fore.dates <- seq.Date(from = max_data+1,
                       to = max_data+forecast_length,
                       by="days")

plot_daily <- df_daily[df_daily$date > "2020-02-27",]

TS_fc$mean <- round(TS_fc$mean, digits=0)
TS_fc$upper <- round(TS_fc$upper, digits=0)
TS_fc$lower <- round(TS_fc$lower, digits=0)

library(plotly)
plotly::plot_ly() %>%
    plotly::add_lines(x = plot_daily$date, y = plot_daily$confirmed_cum,
                      color = I("black"),
                      name = "observed",
                      marker=list(mode='lines')) %>%
    plotly::add_lines(x = fore.dates, y = TS_fc$mean, color = I("blue"), name = "prediction") %>%
    plotly::add_ribbons(x = fore.dates,
                        ymin = TS_fc$lower[, 2],
                        ymax = TS_fc$upper[, 2],
                        color = I("gray95"),
                        name = "95% confidence") %>%
    plotly::add_ribbons(x = fore.dates,
                        ymin = TS_fc$lower[, 1],
                        ymax = TS_fc$upper[, 1],
                        color = I("gray80"), name = "80% confidence") %>% 
    layout(annotations = 
               list(x = 1, y = -0.1, text = paste("AIC:",TS_fit$aic, "Log-Likelihood:", TS_fit$loglik, sep = " "), 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=-14,
                    font=list(size=8, color="grey95"))
    )