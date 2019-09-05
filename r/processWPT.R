rm(list=ls())
library(tidyverse)
library(plotly)
setwd('/home/dproy/code/tti-companion-metric/')

wpt_g4_raw <- read_csv("ct-raw/wpt-raw.csv")

wpt_n5_medians <- read_csv("ct-raw/wpt-nexus5.csv") %>%
  group_by(`document_URL`) %>%
  summarize(`DCL.wpt-n5` = median(`chromeUserTiming.domContentLoadedEventEnd`),
            `FCP.wpt-n5` = median(firstContentfulPaint),
            `LoadEvent.wpt-n5` = median(loadEventEnd),
            # Can't compare TTI because WPT has too many null rows.
            # `TTI` = median(TimeToInteractive),
            `FMP.wpt-n5` = median(firstMeaningfulPaint)
  )

wpt_g4_medians <- wpt_g4_raw %>% 
  group_by(`document_URL`) %>%
  summarize(`DCL.wpt-g4` = median(`chromeUserTiming.domContentLoadedEventEnd`),
            `FCP.wpt-g4` = median(firstContentfulPaint),
            `LoadEvent.wpt-g4` = median(loadEventEnd),
            # Can't compare TTI because WPT has too many null rows.
            # `TTI` = median(TimeToInteractive),
            `FMP.wpt-g4` = median(firstMeaningfulPaint)
            )

ct_medians <- read_csv("out/wpt-ct.csv") %>%
  rename(FMP = timeToFirstMeaningfulPaint,
         LoadEvent = `Load Event`) %>%
  rename_all(list(~paste0(., ".ct-throttled")))

ct_unthrottled <- read_csv("out/wpt-ct-no-throttle.csv") %>%
  rename(FMP = timeToFirstMeaningfulPaint,
         LoadEvent = `Load Event`) %>%
  rename_all(list(~paste0(., ".ct-unthrottled")))

output_table <- inner_join(wpt_g4_medians, wpt_n5_medians,
                          c("document_URL")) %>%
                inner_join(ct_medians,
                           c("document_URL" = "page_name.ct-throttled")) %>%
                inner_join(ct_unthrottled, 
                           c("document_URL" = "page_name.ct-unthrottled")) %>%
  select("document_URL", starts_with("FCP"), starts_with("FMP"), starts_with("DCL"), starts_with("LoadEvent"))

write_csv(output_table, "out/wpt-ct-joined.csv")

# In case you wanted to plot something:
# p <- joined_table %>%
#   ggplot(aes(x = `FCP.wpt`, y = `FCP.ct`)) + 
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0)
#   
# ggplotly(p)