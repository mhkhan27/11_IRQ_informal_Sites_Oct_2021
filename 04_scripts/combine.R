rm(list = ls())
library(dplyr)

sample_frame_rem <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/remaining_ones/sampling_frame20211104-065552.csv") %>% select(-subset)
sample_sum_rem <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/remaining_ones/sampling_summary20211104-065551.csv")


sample_frame_three <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/three/sampling_frame20211104-065729.csv")
sample_sum_three <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/three/sampling_summary20211104-065728.csv")



# summary
summary <- sample_sum_rem %>% bind_rows(sample_sum_three)



# frame -------------------------------------------------------------------


sample_frame <- sample_frame_rem %>% bind_rows(sample_frame_three)

write.csv(sample_frame ,"02_outputs/Sampling/sub_district_level/Final_sampling/combine/sample_frame.csv")
write.csv(summary ,"02_outputs/Sampling/sub_district_level/Final_sampling/combine/summary.csv")
