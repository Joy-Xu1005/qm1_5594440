library(tidyverse)
bank_raw <- read_delim("bank-additional-full.csv", delim = ";")
glimpse(bank_raw)
count(bank_raw, month)
count(bank_raw, y)
data_dictionary <- tribble(
  ~variable, ~description,
  "y",     "Whether the client subscribed to a term deposit (\"yes\" or \"no\")",
  "month", "Month of the last contact with the client (\"mar\" to \"dec\"; the year is not recorded)"
)
library(knitr)
kable(data_dictionary, col.names = c("Variable", "Description"))
bank <- bank_raw |>
  transmute( subscribed = as.integer(y == "yes"),
             month = factor(month, levels = c("mar","apr","may","jun","jul",
                                              "aug","sep","oct","nov","dec"))
  )
glimpse(bank)
mean(bank$subscribed)
month_summary <- bank |>
  group_by(month) |>
  summarise(n = n(),
            prop = mean(subscribed))
month_summary
m1 <- glm(subscribed ~ month, family = binomial, data = bank)
summary(m1)
m0 <- glm(subscribed~1, family=binomial, data = bank)
anova(m0, m1, test = "Chisq")
pred_df <- tibble(month = factor(levels(bank$month), levels = levels(bank$month)))

pred <- predict(m1, newdata = pred_df, type = "link", se.fit = TRUE)

pred_df <- pred_df |>
  mutate(
    log_odds = pred$fit,
    se       = pred$se.fit,
    p        = plogis(log_odds),
    lo       = plogis(log_odds - 1.96 * se),
    hi       = plogis(log_odds + 1.96 * se)
  )

pred_df
p_pred <- ggplot(pred_df, aes(x = month, y = p)) +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  scale_x_discrete(labels = str_to_title) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  labs(x = NULL,
       y = "Probability of subscribing",
       title = "Predicted probability of subscribing, by month",
       subtitle = "Points are model predictions; bars are 95% confidence intervals") +
  theme_minimal()

p_pred
ggsave("fig1_predicted_probabilities.png", p_pred, width = 7, height = 4, dpi = 300)

sep <- pred_df |> filter(month == "sep")
sep_odds <- sep$p / (1 - sep$p)
sep_odds

b <- coef(m1)
sep_log_odds_hand <- b["(Intercept)"] + b["monthsep"]
sep_odds_hand     <- exp(sep_log_odds_hand)
sep_p_hand        <- sep_odds_hand / (1 + sep_odds_hand)

c(log_odds = sep_log_odds_hand, odds = sep_odds_hand, p = sep_p_hand)

p_why <- ggplot(month_summary, aes(x = n, y = prop)) +
  geom_point(aes(size = n), colour = "#2C7FB8", alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(label = str_to_title(month)), vjust = -1.2, size = 3.5) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  labs(x = "People contacted in the month (log scale)",
       y = "Proportion subscribing",
       title = "Months with fewer contacts had higher subscription rates") +
  theme_minimal()

p_why
ggsave("fig2_contacts_vs_subscription.png", p_why, width = 7, height = 4.5, dpi = 300)