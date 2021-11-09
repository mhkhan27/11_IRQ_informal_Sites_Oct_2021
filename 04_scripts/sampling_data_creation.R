rm(list = ls())

library(dplyr)

sample_df <- read.csv("01_inputs/sampling_inputs/Informal_Sites_Masterlist_September_2021_V2.csv") 

sub_district_20 <- c("Markaz Sumail","Fayde","Markaz Mosul")
sub_district_0 <- c("Markaz Zakho","Ba'adre","Markaz Al-Shirqat")

sub_district_0 %in% sample_df$Sub.District



sample_df_all_remaining <- sample_df %>% mutate(
  subset = case_when(Sub.District %in% sub_district_20  ~ "ok",
                     Sub.District %in% sub_district_0  ~ "NOT_OK",
                     X..households > 29 ~ "ok", T~"NOT_OK")
) %>% filter(subset == "ok")

sample_df_three_Sub <- sample_df %>% filter(Sub.District %in% sub_district_0)


sample_df_three_Sub$Sub.District %>% unique() %>% length()
sample_df_all_remaining$Sub.District %>% unique() %>% length()
sample_df$Sub.District %>% unique() %>% length()


sample_df_filter <- sample_df_all_remaining %>% bind_rows(sample_df_three_Sub)


write.csv(sample_df_all_remaining,"02_outputs/Sampling/sub_district_level/Final_sampling/df/sample_df_all_remaining.csv")
write.csv(sample_df_three_Sub,"02_outputs/Sampling/sub_district_level/Final_sampling/df/sample_df_three_Sub.csv")
write.csv(sample_df_filter,"02_outputs/Sampling/sub_district_level/Final_sampling/combine/sample_df_filter.csv")




sample_df$X..households %>% min()
