getwd()
df <- read.csv(file.choose())
library(tidyverse)
library(knitr)

data_dictionary <- tribble(
  ~variable,    ~description,
  "instant",    "Record index",
  "dteday",     "Date",
  "season",     "Season (1 = spring, 2 = summer, 3 = fall, 4 = winter)",
  "yr",         "Year (0 = 2011, 1 = 2012)",
  "mnth",       "Month (1 to 12)",
  "holiday",    "1 if public holiday",
  "weekday",    "Day of week (0 = Sunday)",
  "workingday", "1 if neither weekend nor holiday",
  "weathersit", "Weather situation (1 = clear ... 4 = heavy rain/snow)",
  "temp",       "Normalised temperature, t_min = -8, t_max = 39 (°C)",
  "atemp",      "Normalised feeling temperature, t_min = -16, t_max = 50 (°C)",
  "hum",        "Humidity / 100",
  "windspeed",  "Wind speed / 67",
  "casual",     "Rentals by casual users",
  "registered", "Rentals by registered users",
  "cnt",        "Total rentals (casual + registered)"
)
bikes_raw <- read_csv(file.choose())
bikes_raw
kable(data_dictionary)
bikes <- bikes_raw |>       
  mutate(
    temp_c   = temp * (39 - (-8)) + (-8),
    wind_mph = windspeed * 67 * 0.621371
  )

bikes |> select(temp, temp_c, windspeed, wind_mph) |> summary()
ggplot(bikes, aes(x = cnt)) +
  geom_histogram(binwidth = 500, boundary = 0, fill = "#2C7FB8", colour = "white") +
  labs(x = "Bikes rented per day", y = "Number of days",
       title = "Distribution of daily bike rentals") +
  theme_minimal()

ggplot(bikes, aes(x = temp_c)) +
  geom_histogram(binwidth = 2, boundary = 0, fill = "#D95F0E", colour = "white") +
  labs(x = "Temperature (°C)", y = "Number of days",
       title = "Distribution of daily temperature") +
  theme_minimal()

ggplot(bikes, aes(x = wind_mph)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#41AB5D", colour = "white") +
  labs(x = "Wind speed (mph)", y = "Number of days",
       title = "Distribution of daily wind speed") +
  theme_minimal()
ggplot(bikes, aes(x = temp_c, y = cnt)) +
  geom_point(alpha = 0.4, colour = "#D95F0E") +
  geom_smooth(method = "lm", colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "grey40", linetype = "dashed") +
  labs(x = "Temperature (°C)", y = "Bikes rented per day",
       title = "Bike rentals by temperature") +
  theme_minimal()

ggplot(bikes, aes(x = wind_mph, y = cnt)) +
  geom_point(alpha = 0.4, colour = "#41AB5D") +
  geom_smooth(method = "lm", colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "grey40", linetype = "dashed") +
  labs(x = "Wind speed (mph)", y = "Bikes rented per day",
       title = "Bike rentals by wind speed") +
  theme_minimal()

ggplot(bikes, aes(x = wind_mph, y = temp_c)) +
  geom_point(alpha = 0.4, colour = "#756BB1") +
  geom_smooth(method = "lm", colour = "black") +
  labs(x = "Wind speed (mph)", y = "Temperature (°C)",
       title = "Temperature by wind speed") +
  theme_minimal()
cor.test(bikes$cnt, bikes$temp_c)
cor.test(bikes$cnt, bikes$wind_mph)
cor.test(bikes$temp_c, bikes$wind_mph)
m_wind <- lm(cnt ~ wind_mph, data = bikes)
m_temp <- lm(cnt ~ temp_c, data = bikes)
m_both <- lm(cnt ~ wind_mph + temp_c, data = bikes)

summary(m_wind)
summary(m_temp)
summary(m_both)

confint(m_wind)
confint(m_temp)
confint(m_both)

  theme_minimal()
  bikes |>
    summarise(across(c(cnt, temp_c, wind_mph),
                     list(M = mean, SD = sd, Min = min, Max = max))) |>
    pivot_longer(everything(), names_to = c("var", ".value"), names_sep = "_(?=[^_]+$)")
  
  p_temp <- ggplot(bikes, aes(temp_c, cnt)) +
    geom_point(alpha = 0.4, colour = "#D95F0E") +
    geom_smooth(method = "lm", colour = "black") +
    geom_smooth(method = "loess", se = FALSE, colour = "grey40", linetype = "dashed") +
    labs(x = "Temperature (°C)", y = "Bikes rented per day") +
    theme_minimal()
  ggsave("fig1_temp_scatter.png", p_temp, width = 6, height = 4, dpi = 300)
  
  p_wind <- ggplot(bikes, aes(wind_mph, cnt)) +
    geom_point(alpha = 0.4, colour = "#41AB5D") +
    geom_smooth(method = "lm", colour = "black") +
    labs(x = "Wind speed (mph)", y = "Bikes rented per day") +
    theme_minimal()
  ggsave("fig2_wind_scatter.png", p_wind, width = 6, height = 4, dpi = 300)
  
  library(broom)
  
  bikes <- bikes |>
    mutate(month = factor(month.abb[mnth], levels = month.abb))
  
  month_slopes <- bikes |>
    group_by(month) |>
    group_modify(~ tidy(lm(cnt ~ temp_c, data = .x), conf.int = TRUE)) |>
    ungroup() |>
    filter(term == "temp_c") |>
    select(month, estimate, conf.low, conf.high)
  
  month_slopes
  
  p_month <- ggplot(month_slopes, aes(month, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    labs(x = NULL, y = "Change in rentals per °C") +
    theme_minimal()
  ggsave("fig3_month_slopes.png", p_month, width = 6, height = 4, dpi = 300)