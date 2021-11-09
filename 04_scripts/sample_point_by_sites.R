rm(list = ls())

library(dplyr)
library(sf)
library(rgdal)
library(stringr)
library(butteR)
library(Lslide)
library(plotKML)
library(snakecase)
# read data ---------------------------------------------------------------

data <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/combine/sample_frame.csv") %>% mutate(
  site_name_unique = paste0(ID., "_", Masterlist.Informal.Site.Name) %>% 
    str_replace_all("/","_") %>% str_replace_all(" ","_") %>%  str_replace_all("&","_") 
)
data$District <- data$District %>% snakecase::to_snake_case()
data$Governorate <- data$Governorate %>% snakecase::to_snake_case()
data$Sub.District <- data$Sub.District %>% snakecase::to_snake_case()


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




# create sample -----------------------------------------------------------

sample <- list()
for (i in customized_buffer$site_name_unique) {
  
  df_for_sampling <- customized_buffer %>% filter(site_name_unique==i)
  
  sample[[i]] <- st_sample(x = df_for_sampling,size = df_for_sampling$survey_buffer) %>% as.data.frame() %>% mutate(
    site_name_unique = i)
  
}

total_sample <- do.call("bind_rows",sample)




total_Sample_with_info <- total_sample %>% 
  left_join((data %>% select(site_name_unique,Governorate,District,Sub.District,ID.,psu_id)))


total_Sample_with_info <- total_Sample_with_info %>% group_by(ID.) %>% mutate(
  n_by_informal_site = row_number()
) %>% ungroup()

total_Sample_with_info <- total_Sample_with_info %>% group_by(Sub.District) %>% mutate(
  n_by_sub_district = row_number()
) %>% ungroup()

total_Sample_with_info <- total_Sample_with_info %>% group_by(District) %>% mutate(
  n_by_district = row_number()
) %>% ungroup()

total_Sample_with_info <- total_Sample_with_info %>% group_by(Governorate) %>% mutate(
  n_by_govornorate = row_number()
) %>% ungroup()





####################### check with summary ####################################################################################
count_from_point <- total_Sample_with_info %>% as.data.frame() %>% dplyr::group_by(Sub.District) %>% summarise(
  count_from_point = n()
)
summary_df <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/combine/summary.csv")

summary_df$Stratification <- summary_df$Stratification  %>% snakecase::to_snake_case()

count_from_point_with_summary <- summary_df %>% left_join(count_from_point,
                                                          by = c("Stratification" ="Sub.District"  )) %>% 
  select(Stratification,count_from_point,X..surveys,everything()) %>% mutate(
  tf = count_from_point ==  X..surveys 
  )
count_from_point_with_summary$tf %>% table()

#############################################################################################################################



total_Sample_with_info <- total_Sample_with_info %>% st_as_sf()




# plot kml

for(i in unique(total_Sample_with_info$site_name_unique)){

  total_Sample_with_info_for_site <- total_Sample_with_info %>% filter(site_name_unique == i)
  
  
  write_kl <- "02_outputs/Sampling/sub_district_level/Final_sampling/kmls"
  
  gov <- (total_Sample_with_info_for_site %>% as.data.frame())$Governorate %>% unique() %>% dput
  dis <- (total_Sample_with_info_for_site %>% as.data.frame())$District %>% unique() %>% dput
  sub_dis <- (total_Sample_with_info_for_site %>% as.data.frame())$Sub.District %>% unique() %>% dput
  
  
  
plotKML(sf::as_Spatial(total_Sample_with_info_for_site),
        paste0(write_kl,"/",gov,"/",dis,"/",sub_dis,"/",i,"_",nrow(total_Sample_with_info_for_site)),
        kmz=FALSE,altitude=0,plot.labpt=TRUE,
        labels= paste0(total_Sample_with_info_for_site$site_name_unique,"_",total_Sample_with_info_for_site$n_by_informal_site),
        LabelScale=0.5)

}
######################################### Create folders ###################################################################333

# 
# for (i in unique(total_Sample_with_info$Governorate)) {
# 
#   dir.create(path = paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",i))
# }
# 
# 
# 
# for (i in unique(total_Sample_with_info$District)) {
#   total_Sample_with_info_for_site <- total_Sample_with_info %>% filter(District == i)
# 
#   gov <- (total_Sample_with_info_for_site %>% as.data.frame())$Governorate %>% unique() %>% dput
# 
#   dir.create(path = paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",gov,"/",i))
# }
# 
# 
# 
# for (i in unique(total_Sample_with_info$Sub.District)) {
# 
#   total_Sample_with_info_for_site <- total_Sample_with_info %>% filter(Sub.District == i)
# 
#   gov <- (total_Sample_with_info_for_site %>% as.data.frame())$Governorate %>% unique() %>% dput
#   dis <- (total_Sample_with_info_for_site %>% as.data.frame())$District %>% unique() %>% dput
# 
#   dir.create(paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",gov,"/",dis,"/",i))
# }
###################################### END: CREATE FOLDER #########################################################################3



st_write(total_sample_id,
         "02_outputs/Sampling/sub_district_level/Final_sampling/buffer_poly/sample_point2.shp",overwrite=T)



# write buffer area -------------------------------------------------------

spatial_buffer <- customized_buffer %>% as_Spatial()
writeOGR(spatial_buffer,dsn = "02_outputs/Sampling/sub_district_level/Final_sampling/buffer_poly/buffer_2",
         layer = "buffer_2",driver = "ESRI Shapefile" )





