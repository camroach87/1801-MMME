# Script for producing an animated plot of profiles
# Use a rolling window of 30-days to calculate fixed effect estimates for each day of year

rm(list=ls())

library(tidyverse)
library(gganimate)
library(lubridate)
library(MuMIn)
library(lme4)
library(parallel)
source("multi-model-mixed-effects-energy/R/load-data.R")
source("multi-model-mixed-effects-energy/R/nest-helpers.R")
source("multi-model-mixed-effects-energy/R/plot-helpers.R")

all_df <- load_all_data(
  attribute_file = "../../data/building_level/attributes_20180115.csv",
  qh_file = "../../data/building_level/data_all_qh.RData",
  building_list_file = "../../data/building_level/building_list.csv")

all_df$TenantFeed <- !all_df$BaseBldngFeedOnly

attribute_names <- c("TenantFeed", "WaterCooledCondenser", "DXSystem",
                     "GasFiredBoiler", "ElectricElementHeating",
                     "CentralDist")
min_dt <- dmy('1/12/2012')
max_dt <- dmy('1/12/2015')
days <- 1:365
hours <- 0:23
window_size <- 15  # days eitherside
sample_size <- 0.2



all_df <- all_df %>% 
  filter(ts >= min_dt,
         ts < max_dt) %>% 
  sample_frac(sample_size) %>%
  select(BID, Wh, temperature, attribute_names, ts) %>%
  mutate(Year = year(ts),
         Year = if_else(month(ts) == 12, Year + 1, Year),
         Year = Year - min(Year)) %>% # Scale so lmer doesn't fail
  na.omit()


filter_window_df <- function(cur_day, cur_hour) {
  all_df %>%
    filter(abs(yday(ts) - cur_day)%%(365-window_size) <= window_size,
           hour(ts) == cur_hour) %>%  # FIXME: year boundaries are missing data
    filter_outliers()
}


clust <- makeCluster(11)
invisible(clusterEvalQ(clust, library(lme4)))
options(na.action = "na.fail")

profile_df <- list(day = days,
                   hour = hours) %>% 
  cross_df() %>% 
  arrange(day, hour) %>% 
  mutate(data = map2(day, hour, filter_window_df),
         dredge = map(data, n_pdredge, attribute_names = attribute_names),
         mmavg = map(dredge, possibly(n_mmavg, NULL), level = 0.9),
         coefficients = map(mmavg, possibly(n_coef, NULL), full = TRUE, level = c(0.8, 0.9)))

stopCluster(clust)
options(na.action = "na.omit")



#### Plots ==========
plot_df <- prepare_plot_df(profile_df)
write_csv(plot_df, "multi-model-mixed-effects-energy/cache/plot_df.csv")
# plot_df <- read_csv("multi-model-mixed-effects-energy/cache/plot_df_3.csv")

df <- plot_df

scale_max <- df %>% 
  filter(Profile=="Difference") %>% 
  pull(Value) %>%
  max(na.rm=TRUE)
scale_min <- df %>% 
  filter(Profile=="Difference") %>% 
  pull(Value) %>%
  min(na.rm=TRUE)
scale_midpoint <- -scale_min/(scale_max-scale_min)

p <- ggplot(df) +
  geom_bar(data = . %>%
              filter(Profile %in% c("Difference")),
            aes(x = hour, y = Value, fill = Value),
           stat = "Identity", position = "identity") +
  facet_wrap(~Variable) +
  scale_fill_gradientn(colours = c(ba_palette[2], "white", ba_palette[3],
                                   ba_palette[3]),
                       values = c(0, scale_midpoint, scale_midpoint*2, 1)) +
  labs(title = "Day of year: {strftime(strptime(frame_time, '%j'), '%d %B')}",
       x = "Hour",
       y = expression(Normalised~electricity~(Wh/m^2))) +
  theme(legend.position = "none") +
  transition_time(day) +
  ease_aes("linear")

# animate(p, nframes = 5)
# animate(p, fps = 60, duration = 2, width = 600*sqrt(2), height = 600)
# animate(p, fps = 60, duration = 2, res=300, width = 2400*sqrt(2), height = 2400)
animate(p, duration = 12, res=300, width = 2400*sqrt(2), height = 2400)
anim_save("multi-model-mixed-effects-energy/cache/profiles3.gif")


#### Tests ==========
profile_df %>% 
  mutate(max_date = map(data, ~max(yday(.$ts))),
         min_date = map(data, ~min(yday(.$ts)))) %>% 
  unnest(max_date, min_date)