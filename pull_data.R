library(mmsd.wq)
library(tidyverse)

data_yr_param <- calculate_rep_exceedance(
  start_date = "2015-01-01", 
  end_date = "2021-12-31",
  grouping = c("parameter","year"))

data_param_wtb <- calculate_rep_exceedance(
  start_date = "2015-01-01", 
  end_date = "2021-12-31",
  grouping = c("parameter","water_body"))

data_yr_wtb <- calculate_rep_exceedance(
  start_date = "2015-01-01", 
  end_date = "2021-12-31",
  grouping = c("year","water_body"))

# write.csv(z,"all_data.csv")

p <-data_yr_param[[2]] %>%
  filter(perc_exceeds != 0) %>%
  ggplot() +
  geom_bar(aes(x = year,y = exceeds,
               group = parameter, fill = parameter),
           stat = "identity") +
  ylab("Number of Exceedances")

plotly::ggplotly(p)

#heatmap - year waterbody type
p <- data_yr_wtb[[2]] %>%
  group_by(water_body) %>%
  mutate(sum_exceeds = sum(perc_exceeds))  %>%
  ungroup() %>%
  mutate(water_body = forcats::fct_reorder(water_body, sum_exceeds, max)) %>%
  rename(Percent_Exceedance = perc_exceeds) %>%
  ggplot() +
  geom_tile(aes(
    x = year,
    y = water_body,
    text = samples,
    labels = exceeds,
    fill = Percent_Exceedance
  )) +
  geom_text(aes(
    x = year,
    y = water_body,
    label = paste0(Percent_Exceedance, "%")
  ),
  size = 3) +
  scale_fill_gradient(low = "#d6e4ff", high = "red") +
  ylab("")+
  xlab("")

plotly::ggplotly(p)

#heat map - param wtb
p <- data_yr_param[[2]] %>%
  filter(!parameter %in% c("hardness", 
                           "total_inorganic_carbon_2",
                           "nitrate_nitrogen_auto_chemistry")) %>%
  left_join(param_lookup_table %>%
              select(parameter,label_name)) %>%
  group_by(label_name) %>%
  mutate(sum_exceeds = sum(perc_exceeds))  %>%
  ungroup() %>%
  mutate(label_name = forcats::fct_reorder(label_name, sum_exceeds, max)) %>%
  rename(Percent_Exceedance = perc_exceeds) %>%
  ggplot() +
  geom_tile(aes(
    x = year,
    y = label_name,
    text = samples,
    labels = exceeds,
    fill = Percent_Exceedance
  )) +
  geom_text(aes(
    x = year,
    y = label_name,
    label = paste0(Percent_Exceedance, "%")
  ),
  size = 3) +
  scale_fill_gradient(low = "#d6e4ff", high = "red") +
  ylab("")+
  xlab("")

plotly::ggplotly(p)




#heat map - param wtb
p <- data_param_wtb[[2]] %>%
  filter(!parameter %in% c("hardness", 
                           "total_inorganic_carbon_2",
                           "nitrate_nitrogen_auto_chemistry")) %>%
  left_join(param_lookup_table %>%
              select(parameter,label_name)) %>%
  group_by(label_name) %>%
  mutate(sum_exceeds = sum(perc_exceeds))  %>%
  ungroup() %>%
  mutate(label_name = forcats::fct_reorder(label_name, sum_exceeds, max)) %>%
  rename(Percent_Exceedance = perc_exceeds) %>%
  ggplot() +
  geom_tile(aes(
    y = label_name,
    x = water_body,
    text = samples,
    labels = exceeds,
    fill = Percent_Exceedance
  )) +
  geom_text(aes(
    y = label_name,
    x = water_body,
    label = paste0(Percent_Exceedance, "%")
  ),
  size = 3) +
  scale_fill_gradient(low = "#d6e4ff", high = "red") +
  ylab("") +
  xlab("")

plotly::ggplotly(p)



