# import libraries
library(tidyverse)

# define parameters
batch_frequency <- 28 # days between batches
batch_system_periods <- 4 # overall number of system periods
growout_period <- 168 # period whose factor includes frequency, system period and exceeds minimum growout period

# set production period to two growth periods to observe system loading
prod_plan <- data.frame(facility_day = seq(0, 2 * growout_period - 1)) %>%
  mutate(batch_1 = case_when(facility_day < 168 ~ facility_day),
         batch_2 = case_when(facility_day - batch_frequency * 1 >= 0 & facility_day - batch_frequency * 1 < 168 ~ facility_day - 1 * batch_frequency),
         batch_3 = case_when(facility_day - batch_frequency * 2 >= 0 & facility_day - batch_frequency * 2 < 168 ~ facility_day - 2 * batch_frequency),
         batch_4 = case_when(facility_day - batch_frequency * 3 >= 0 & facility_day - batch_frequency * 3 < 168 ~ facility_day - 3 * batch_frequency),
         batch_5 = case_when(facility_day - batch_frequency * 4 >= 0 & facility_day - batch_frequency * 4 < 168 ~ facility_day - 4 * batch_frequency),
         batch_6 = case_when(facility_day - batch_frequency * 5 >= 0 & facility_day - batch_frequency * 5 < 168 ~ facility_day - 5 * batch_frequency),
         batch_7 = case_when(facility_day - batch_frequency * 6 >= 0 & facility_day - batch_frequency * 6 < 168 ~ facility_day - 6 * batch_frequency),
         batch_8 = case_when(facility_day - batch_frequency * 7 >= 0 & facility_day - batch_frequency * 7 < 168 ~ facility_day - 7 * batch_frequency),
         batch_9 = case_when(facility_day - batch_frequency * 8 >= 0 & facility_day - batch_frequency * 8 < 168 ~ facility_day - 8 * batch_frequency),
         batch_10 = case_when(facility_day - batch_frequency * 9 >= 0 & facility_day - batch_frequency * 9 < 168 ~ facility_day - 9 * batch_frequency),
         batch_11 = case_when(facility_day - batch_frequency * 10 >= 0 & facility_day - batch_frequency * 10 < 168 ~ facility_day - 10 * batch_frequency),
         batch_12 = case_when(facility_day - batch_frequency * 11 >= 0 & facility_day - batch_frequency * 11 < 168 ~ facility_day - 11 * batch_frequency)) %>%
  pivot_longer(c(batch_1:batch_12), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:41) ~ "system_1",
                            batch_day %in% c(42:82) ~ "system_2",
                            batch_day %in% c(83:167) ~ "system_3"))

systems_plot <- prod_plan %>%
  ggplot(aes(facility_day, batch, color = system)) +
  geom_line(size = 3) +
  scale_y_discrete(limits = c('batch_1', 'batch_2', 'batch_3', 'batch_4', 'batch_5', 'batch_6', 'batch_7', 'batch_8', 'batch_9', 'batch_10', 'batch_11', 'batch_12')) +
  theme_light()

pdf("plots/prod_plan.pdf", width = 12, height = 8)
systems_plot
dev.off()


