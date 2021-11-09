rm(list = ls())

library(dplyr)
library(sf)
library(rgdal)
library(stringr)
library(butteR)
library(Lslide)
# read data ---------------------------------------------------------------

data <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/combine/sample_frame.csv")
buffer <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/combine/buffer.csv") %>% 
  select(-number_of_sites,-X)




# recoding ----------------------------------------------------------------

data <- data %>% mutate(
hh_size_class = case_when(X..households <11 ~ "0_to_10",
                          X..households %in% 11:20 ~ "11_to_20",
                          X..households %in% 21:30 ~ "21_to_30",
                          X..households > 30 ~"more_than_30")
)



# add buffer distance -----------------------------------------------------

data <- data %>% left_join(buffer) 



# make st object ----------------------------------------------------------

cords <- c("Site.Longitude","Site.Latitude")
df_st <- data %>% st_as_sf(coords =cords,crs = 4326) %>% st_transform(crs=32638)


# fix buffer --------------------------------------------------------------

df_st <- df_st %>% mutate(
  buffer= case_when(is.na(df_st$buffer)~as.double(500),
                     T~ as.double(df_st$buffer))
)



# make buffer polygon -------------------------------------------------------------

customized_buffer <- st_buffer(df_st,dist = df_st$buffer)


# disolve by sub district
customized_buffer_dis <- customized_buffer %>% st_dissolve(by = "Sub.District")

# calculate number of survey to be calculated by sub_district
data_by_sub <- data %>% group_by(Sub.District) %>% summarise(
  survey_needed = sum(survey_buffer))

## join number of survey needed data
customized_buffer_dis <- customized_buffer_dis %>% left_join(data_by_sub)




# create sample point -----------------------------------------------------

### create random point
sample_point <- st_sample(x = customized_buffer_dis,size = customized_buffer_dis$survey_needed) %>% st_as_sf()

#join sub district information
sample_with_info <- st_intersection(sample_point,customized_buffer_dis)



# write -------------------------------------------------------------------

st_write(sample_with_info,
         "02_outputs/Sampling/sub_district_level/Final_sampling/buffer_poly/sample_point.shp",overwrite=T)







