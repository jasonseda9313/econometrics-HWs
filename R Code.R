library(tidyverse)
library(ggplot2)


setwd("..")
setwd("HPS_2020-24")

load("d_HHP2020_24.Rdata")

setwd("..")
setwd("ecob2000_lab3")

d_HHP2020_24$partnered <- (d_HHP2020_24$Mar_Stat == "Married") | 
  (d_HHP2020_24$Mar_Stat == "widowed") | 
  (d_HHP2020_24$Mar_Stat == "divorced") |
  (d_HHP2020_24$Mar_Stat == "separated")

table(d_HHP2020_24$partnered, useNA = "ifany")


table(d_HHP2020_24$Mar_Stat, d_HHP2020_24$partnered)


library(dplyr)
library(ggplot2)

# Summarise share partnered by Age
partner_rate_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(share_partnered = mean(partnered, na.rm = TRUE))

# Plot it
ggplot(partner_rate_by_age, aes(x = Age, y = share_partnered)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share Ever Partnered by Age",
       x = "Age",
       y = "Percent partnered (current or past)") +
  theme_minimal()


# If your data has a "cohabiting" or similar category:
d_HHP2020_24$current_partnered <- 
  (d_HHP2020_24$Mar_Stat == "Married") |
  (d_HHP2020_24$Mar_Stat == "Living with partner")

d_HHP2020_24$current_partnered <- d_HHP2020_24$Mar_Stat == "Married


current_partner_rate_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(share_current_partnered = mean(current_partnered, na.rm = TRUE))


ggplot(current_partner_rate_by_age, aes(x = Age, y = share_current_partnered)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Currently Partnered by Age",
       x = "Age",
       y = "Percent currently partnered") +
  theme_minimal()


library(dplyr)
library(ggplot2)
library(tidyr)  # for pivot_longer

# 1. Create both variables
d_HHP2020_24 <- d_HHP2020_24 %>%
  mutate(
    ever_partnered = (Mar_Stat == "Married") |
      (Mar_Stat == "widowed") |
      (Mar_Stat == "divorced") |
      (Mar_Stat == "separated"),
    current_partnered = (Mar_Stat == "Married") |
      (Mar_Stat == "Living with partner")
  )

# 2. Summarise both by Age
partner_rates_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(
    ever_partnered = mean(ever_partnered, na.rm = TRUE),
    current_partnered = mean(current_partnered, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(ever_partnered, current_partnered),
               names_to = "partner_type",
               values_to = "share")

# 3. Plot both lines on the same graph
ggplot(partner_rates_by_age, aes(x = Age, y = share, color = partner_type)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("ever_partnered" = "steelblue",
                                "current_partnered" = "darkgreen"),
                     labels = c("Ever partnered", "Currently partnered")) +
  labs(title = "Partnering by Age",
       x = "Age",
       y = "Percent",
       color = "Partnership status") +
  theme_minimal()




