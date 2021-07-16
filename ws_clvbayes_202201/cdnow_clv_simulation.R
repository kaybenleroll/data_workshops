# Set up
library(ggridges)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
source("https://raw.githubusercontent.com/alexpavlakis/clv/master/theme_og.R")

# Read in and format a sample of the cdnow data set
custs <- read.table("data/CDNOW_sample.txt") %>%
  select(-V1) %>%
  mutate(customer_id = V2,
         logged = as.Date(as.character(V3), format = "%Y%m%d"),
         total = V5) %>%
  filter(customer_id < 100, total > 0) %>%
  group_by(customer_id, logged) %>%
  summarise(spending = sum(total))

# Plot customer timeline
custs %>%
  ggplot() +
  aes(x = logged, y = customer_id, size = spending) +
  geom_hline(yintercept = c(1:max(custs$customer_id)),
             col = "grey1",
             lty = 1,
             cex = 0.05) +
  geom_point(col = "darkgrey") +
  scale_x_date("",
               limits = c(date("1997-01-01"), date("2000-01-01"))) +
  scale_y_discrete("",
                   limits = c(1:max(custs$customer_id)),
                   breaks = c(1:max(custs$customer_id)),
                   labels = paste("Customer", c(1:max(custs$customer_id)))) +
  geom_vline(xintercept = max(custs$logged),
             col = og_orange,
             lty = 2,
             lwd = 0.5) +
  annotate("text", x = date("1999-04-01"), y = 50,
           label = "?",
           col = og_orange,
           size = 50,
           family = "Times") +
  theme_og() +
  theme(axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(face = "bold", size = 3))

# Format data for modeling (months)
start_day <- date(min(custs$logged))
end_day <- date(max(custs$logged))

customer_data <- custs %>%
  group_by(customer_id) %>%
  summarise(x = n() - 1,
            t_x = difftime(max(logged), start_day, units = "days")/30.41,
            t_cal = difftime(end_day, min(logged), units = "days")/30.41,
            mx = mean(spending)) %>%
  arrange(-customer_id)

model_data <- list(
  x = customer_data$x,
  t_x = as.numeric(customer_data$t_x),
  t_cal = as.numeric(customer_data$t_cal),
  mx = customer_data$mx,
  N = nrow(customer_data),
  N_months = 60
)

# Fit model
model_fit <- stan(file   = "full_clv.stan",
                  data   = model_data,
                  chains =    4,
                  seed   =   42,
                  iter   = 1000)

# Plot of expected transaction distributions
exp_trans_dist <- data.frame(distro = c(extract(model_fit)$exp_trans),
                             customer_id = rev(factor(rep(c(1:99), each = 2000)))) %>%
  mutate(customer = paste("Customer", customer_id)) %>%
  group_by(customer_id) %>%
  mutate(mean_trans = mean(distro)) %>%
  ungroup()

exp_trans_dist %>%
  ggplot() +
  aes(x = distro, y = reorder(customer, mean_trans)) +
  geom_density_ridges() +
  xlim(c(0, 100)) +
  ggtitle("Expected future transactions") +
  ylab("") +
  xlab("") +
  theme_og() +
  theme(axis.text.y = element_text(size = 4))

# PLot of churn rate distributions
churn_p_dist <- data.frame(distro = 1 - c(extract(model_fit)$p_alive),
                           customer_id = rev(factor(rep(c(1:99), each = 2000)))) %>%
  mutate(customer = paste("Customer", customer_id),
         distro = if_else(distro < 0, 0, distro),
         distro = if_else(distro > 1, 1, distro)) %>%
  group_by(customer_id) %>%
  mutate(mean_churn = mean(distro)) %>%
  ungroup()

churn_p_dist %>%
  ggplot() +
  aes(x = distro, y = reorder(customer, mean_churn)) +
  geom_density_ridges() +
  xlim(c(0, 1)) +
  ylab("") +
  xlab("") +
  ggtitle("Distribution of churn probabilities") +
  theme_og() +
  theme(axis.text.y = element_text(size = 4))

# PLot of LTV distributions
lt_val_dist <- data.frame(distro = c(extract(model_fit)$lt_val),
                          customer_id = rev(factor(rep(c(1:99), each = 2000)))) %>%
  mutate(customer = paste("Customer", customer_id)) %>%
  group_by(customer_id) %>%
  mutate(mean_trans = mean(distro)) %>%
  ungroup()

lt_val_dist %>%
  ggplot() +
  aes(x = distro, y = reorder(customer, mean_trans)) +
  geom_density_ridges() +
  xlim(c(0, 5000)) +
  ggtitle("Distribution of total spending over next 5 years") +
  ylab("") +
  xlab("") +
  theme_og() +
  theme(axis.text.y = element_text(size = 4))

# Plot of spending distributions
mx_pred_dist <- data.frame(distro = c(extract(model_fit)$mx_pred),
                             customer_id = rev(factor(rep(c(1:99), each = 2000)))) %>%
  mutate(customer = paste("Customer", customer_id)) %>%
  group_by(customer_id) %>%
  mutate(mean_trans = mean(distro)) %>%
  ungroup()

mx_pred_dist %>%
  ggplot() +
  aes(x = distro, y = reorder(customer, mean_trans)) +
  geom_density_ridges() +
  xlim(c(0, 200)) +
  ggtitle("Distributions of per transaction spending") +
  ylab("") +
  xlab("") +
  theme_og() +
  theme(axis.text.y = element_text(size = 4))
