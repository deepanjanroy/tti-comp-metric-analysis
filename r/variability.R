rm(list=ls())
library(tidyverse)
library(plotly)
library(cowplot)
theme_set(theme_light())

setwd('/home/dproy/code/tti-companion-metric/swarming-log-parser/')

# Avoid using scientific notation in axis ticks, by setting a high penalty.
# What a strange language.
options(scipen=10000)  

df <- read_csv("merged.csv") %>%
  rename(FCP = timeToFirstContentfulPaint,
         TTI = `TTI-50`,
         TBT = `SumOfQueuingTimeGT50_p1.0-navStart-Interactive`,
         TBT10 = `SumOfQueuingTimeGT50_p1.0-navStart-TtiPlus10s`,
         TBT1p5 = `SumOfQueuingTimeGT50_p1.5-FCP-Interactive`,
         TBT2p0 = `SumOfQueuingTimeGT50_p2.0-FCP-Interactive`)

urls <- read_csv("urls_list.csv") %>% arrange(url)

joined <- urls %>%
  inner_join(df, by = 'url')

means <- joined %>%
  group_by(url) %>%
  summarize(mean_tti = mean(TTI), mean_tbt = mean(TBT),
            mean_fcp = mean(FCP), mean_tbt10 = mean(TBT10))

curr_url <- "http://www.fandom.com"

joined_with_mean <- joined %>%
  inner_join(means, by = 'url') %>%
  filter(url == curr_url) %>%
  select(url, TTI, mean_tti)

metric_density <- function(curr_url, metric) {
  metric_quo = enquo(metric)  # how does this work?
  joined %>%
    filter(url == curr_url) %>%
    ggplot(aes(x = !!metric)) +
    geom_density(adjust = 1.2)
}

gen_sorted_point_plots <- function(curr_url) {
  p1 <- joined %>%
    inner_join(means, by='url') %>%
    filter(url == curr_url) %>%
    arrange(FCP) %>%
    mutate(sort_index = row_number()) %>%
    ggplot(aes(y = FCP, x = sort_index)) +
    geom_point(color = "royalblue") +
    geom_line(color = "royalblue") +
    labs(title = curr_url)
  
  p2 <- joined %>%
    inner_join(means, by='url') %>%
    filter(url == curr_url) %>%
    arrange(TTI) %>%
    mutate(sort_index = row_number()) %>%
    ggplot(aes(y = TTI, x = sort_index)) +
    geom_point(color = "royalblue") +
    geom_line(color = "royalblue") +
    labs(title = curr_url)
  
  p3 <- joined %>%
    inner_join(means, by='url') %>%
    filter(url == curr_url) %>%
    arrange(TBT) %>%
    mutate(sort_index = row_number()) %>%
    ggplot(aes(y = TBT, x = sort_index)) +
    geom_point(color = "royalblue") +
    geom_line(color = "royalblue") +
    labs(title = curr_url)
  
  plot_grid(p1, p2, p3, ncol=3)
}

generate_plot_for_url <- function(curr_url) {
  point_plot <- joined %>%
    inner_join(means, by='url') %>%
    filter(url == curr_url) %>%
    mutate(RFCP = FCP / mean_fcp, RTTI = TTI / mean_tti,
           RTBT = TBT / mean_tbt) %>%
    gather("metric", "value", RFCP, RTTI, RTBT) %>%
    ggplot(aes(y = value, x = sort_by_fcp_index)) +
    geom_point(color = "royalblue") +
    geom_line(color = "royalblue") +
    facet_wrap(~metric) +
    labs(title = curr_url)
  
  plot_grid(point_plot,
            nrow = 1
  )
}

generate_all <- function() {
  for (i in seq_along(urls$url)) {
    p <- gen_sorted_point_plots(urls$url[i])
    save_plot(paste("variability-plots/", i, ".png", sep=""), plot = p,
              base_height=3, base_aspect_ratio = 5, nrow = 1)
  }
}

generate_all()

curr_url <- "http://www.rediff.com"
joined %>%
  inner_join(means, by='url') %>%
  filter(url == curr_url) %>%
  arrange(TTI) %>%
  mutate(sort_index = 1:n()) %>%
  ggplot(aes(y = TTI, x = sort_index)) +
  geom_point(color = "royalblue") +
  geom_line(color = "royalblue") +
  labs(title = curr_url)

gen_sorted_point_plots(curr_url)




# Maybe I need to just compare to TTI? 

rstd_df <- df %>%
  group_by(url) %>%
  filter(n() == 25) %>% # Filters out 32 out of 856 urls that doesn't have 25 data points. 
  summarise(rstd_fcp = sd(FCP) / mean(FCP),
            rstd_tti = sd(TTI) / mean(TTI),
            rstd_tbt = sd(TBT) / (mean(TBT) + 1),
            rstd_tbt1p5 = sd(TBT1p5) / (mean(TBT1p5) + 1),
            rstd_tbt2p0 = sd(TBT2p0)/ (mean(TBT2p0) + 1),
            mean_tbt = mean(TBT) + 1,
            mean_tti = mean(TTI),
            mean_tbt1p5 = mean(TBT1p5) + 1,
            mean_tbt2p0 = mean(TBT2p0)+ 1
            )

rstd_df %>%
  ggplot(aes(x = mean_tbt2p0, y = rstd_tbt2p0)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(0, 1))

rstd_df %>% ggplot(aes(x = mean_tti, y = rstd_tti)) +
  geom_point(aes(x = mean_tti, y = rstd_tti)) +
  geom_smooth(method = "lm") + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(0, 1))


rstd_df %>% 
  mutate(tti_bucket = cut(mean_tti,
                          breaks=seq(0, 120000, 5000),
                          labels=c(paste0(seq(0, 115, 5),
                                          "-",
                                          seq(5, 120, 5),
                                          "s")))) %>%
  group_by(tti_bucket) %>%
  summarize(mean_rstd_tti = mean(rstd_tti),
            mean_rstd_tbt = mean(rstd_tbt),
            count = n()) 



std_df <- df %>%
  group_by(url) %>%
  summarize(std_fcp = sd(FCP),
            std_tti = sd(TTI),
            std_tbt = sd(TBT))

std_df %>%
  gather("metric", "std", -url) %>%
  ggplot(aes(x = std)) +
  geom_histogram() + 
  facet_wrap(~metric) +
  scale_x_log10()
    
summary(std_df)

summary(rstd_df)

df %>% select(FCP, TTI, TBT) %>% summary()

df %>% 
  group_by(url) %>% 
  summarize(min_tbt = min(TBT),
            max_tbt = max(TBT),
            min_tti = min(TTI),
            max_tti = max(TTI)) %>%
  mutate(tbt_range = max_tbt - min_tbt,
         tti_range = max_tti - min_tti) %>% 
  ggplot() +
  geom_histogram(aes(x = tti_range))

  

df %>% 
  group_by(url) %>%
  summarize(sd(TBT), sd(TBT)/mean(TTI), sd(TBT)/mean(TBT))

df %>% filter(!is.na(TTI)) %>% group_by(url) %>% count() %>% group_by(n) %>% count()

# Want to find the case where TTI is > 3s:
df %>% 
  group_by(url) %>% 
  filter(n() == 25) %>%
  summarize(min_tbt = min(TBT),
            max_tbt = max(TBT),
            min_tti = min(TTI),
            max_tti = max(TTI),
            sd_tbt = sd(TBT),
            mean_tbt = mean(TBT) + 0.1) %>%
  mutate(tbt_range = max_tbt - min_tbt,
         tti_range = max_tti - min_tti,
         tbt_range_bucket = floor(tbt_range/100)* 100) %>%
  group_by(tbt_range_bucket) %>%
  count() %>% View()

  

df %>% 
  group_by(url) %>% 
  filter(n() == 25) %>%
  summarize(min_tbt = min(TBT),
            max_tbt = max(TBT),
            min_tti = min(TTI),
            max_tti = max(TTI),
            sd_tbt = sd(TBT),
            sd_tti = sd(TTI),
            mean_tbt = mean(TBT) + 0.1,
            mean_tti = mean(TTI)) %>%
  mutate(tbt_range = max_tbt - min_tbt,
         tti_range = max_tti - min_tti) %>%
  ggplot(aes(y=sd_tti/mean_tti, x = mean_tti)) +
  geom_point()


# TODO: Plot these two together. 

ggplotly(p)  

joined %>%
  group_by(url) %>%
  summarise(mean(FCP),
            mean(TTI),
            mean(TBT),
            median(FCP),
            median(TTI),
            median(TBT),
            )
  

# Code the produce the inital url list. Subtracted nsfw sites, added
# some (personally biased) major sites like pinterest/github.  
# set.seed(1)
# median_fcp <- df %>%
#   select(url, sort_by_fcp_index, FCP) %>% 
#   group_by(url) %>%
#   summarize(median_fcp = median(FCP)) %>%
#   arrange(median_fcp) %>%
#   select(url) %>%
#   write_csv("all_urls.csv")


  