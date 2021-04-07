# PURPOSE: Munge and Analysis of exercise data
# AUTHOR: Tim Essam 
# LICENSE: MIT
# DATE: 2021-03-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  library(tidyverse)
  library(ggplot2)
  library(lubridate)
  library(XML)
  library(dev)
  library(lubridate)
  library(glitr)
  library(glamr)
  library(scales)

  #From: https://taraskaduk.com/posts/2019-03-23-apple-health/
  # Grab latest transfer
  health <- glamr::return_latest("../../../Downloads", 
                                 pattern = "apple_health_export")

  xml <- xmlParse(file.path(health, "export.xml"))
  summary(xml)
  
  # PARSE XML
  df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
  
  steps <- df %>% 
    mutate(across(everything(), as.character),
           value = as.numeric(value, na.rm = T)) %>% 
    filter(str_detect(type, "Running|StepCount"), str_detect(sourceName, "Tims")) %>% 
    select(-device) %>% 
    mutate(start_date = str_extract(startDate, "([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})") %>% as.Date(.),
           start_time = str_extract(startDate, "([[:digit:]]{2}):([[:digit:]]{2})")) %>% 
    mutate(type = if_else(str_detect(type, "Running"), "miles", "steps")) %>% 
    group_by(start_date, start_time, type) %>% 
    summarise(steps = sum(value)) %>% 
    ungroup() %>% 
    mutate(year = year(start_date),
           month = month(start_date),
           day = day(start_date),
           dayname = wday(start_date, label = T),
           month_year = ym(paste(year,month)),
           month_name = paste(year(start_date), month(start_date, label = T))
           ) %>% 
    group_by(month_year, type) %>% 
    mutate(monthly_steps = sum(steps)) %>%
    group_by(month_year) %>% 
    mutate(id_order = cur_group_id()) %>% 
    mutate(weekend = if_else(dayname %in% c("Wed"), genoa, grey30k)) 
    
  
  steps %>% 
    count(month_year, monthly_steps, type) %>% 
    select(-n) %>% 
    # mutate(ave = monthly_steps / n) %>% 
    spread(type, monthly_steps) %>% prinf()
  
  steps %>% 
    group_by(dayname) %>% 
    summarise(steps = sum(steps)) %>% 
    mutate(total = sum(steps))

  
  steps %>% 
    filter(type == "miles") %>% 
    group_by(month_year) %>% 
    mutate(steps_month = sum(steps)) %>% 
    ungroup() %>% 
    ggplot(aes(x = start_date, y = steps, fill = weekend)) + geom_col() +
    facet_wrap(~fct_reorder(paste0(month_name, ": Steps ", comma(steps_month)), id_order), scales = "free_x") +
    scale_fill_identity() +
    si_style_ygrid()
  
  steps %>% 
    group_by(month, year, dayname) %>% 
    mutate(day_ave = mean(steps),
           step_fill = if_else(day_ave > 2, grey10k, grey60k)) %>%
    ggplot(aes(x = dayname, y = day_ave)) + geom_col() +
    facet_wrap(~month_year)
    
    
  steps %>% 
    ggplot(aes(x = month_year, y = dayname, fill = day_ave)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(day_ave, 1), color = step_fill)) +
    scale_fill_si(palette = "scooters", discrete = F) +
    si_style_nolines() +
      scale_color_identity() +
    labs(x = NULL, y = NULL, title = "Pandemic Miles") +
    scale_x_date(date_breaks = "month", 
                 date_label = "%b")
    
  
  ggplot(steps, aes(x = day, y = month_year, fill = steps)) +
    geom_tile(color = "white") +
    scale_x_continuous(breaks = c(seq(1, 31, 1))) +
    scale_fill_viridis_c(direction = -1, option = "A", alpha = 0.85) +
    si_style_nolines() +
    scale_y_date(date_breaks = "1 month", date_labels = "%b")
  
  
  # Questions I'd like to answer:
  #1) What days did I tend to walk the most on?
  #2) What month 
  
  
  
  
  