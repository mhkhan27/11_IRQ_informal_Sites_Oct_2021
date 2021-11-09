rm(list = ls())

library(dplyr)
library(sf)
library(rgdal)
library(stringr)
library(butteR)
library(Lslide)
library(plotKML)
library(snakecase)
library(st)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(htmltools)
library(htmlwidgets)

create_directory <- c(T,F)[1]

# read data ---------------------------------------------------------------

data <- read.csv("02_outputs/Sampling/sub_district_level/Final_sampling/combine/sample_frame.csv") %>% mutate(
  site_name_unique = paste0(ID., "_", Masterlist.Informal.Site.Name) %>% 
    str_replace_all("/","_") %>% str_replace_all(" ","_") %>%  str_replace_all("&","_") 
)


data$District <- data$District %>% snakecase::to_snake_case()
data$Governorate <- data$Governorate %>% snakecase::to_snake_case()
data$Sub.District <- data$Sub.District %>% snakecase::to_snake_case()
data$site_name_unique <- data$site_name_unique %>% snakecase::to_snake_case()




# read spatial data -------------------------------------------------------

admin_zero <-st_read("01_inputs/shapefiles/irq_admbnda_adm0_cso_itos_20190603.shp")
admin1_boundary <- st_read("01_inputs/shapefiles/irq_admbnda_adm1_cso_20190603.shp")
admin2_boundary <- st_read("01_inputs/shapefiles/irq_admbnda_adm2_cso_20190603.shp")
admin3_boundary <- st_read("01_inputs/shapefiles/irq_admbnda_adm3_cso_20190603.shp")




write_kl <- "02_outputs/Sampling/sub_district_level/Final_sampling/kmls"



# map ---------------------------------------------------------------------

base_map <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>%  
  # leaflet::addRasterImage(worldpop) %>% 
  leaflet::addPolygons(data = admin1_boundary,color = "#D2CBB8",
                       label = ~htmlEscape(ADM1_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "15px",
                                                     "font-weight" =  "bold"
                                                   )),
                       popup = paste("Governorate:", admin1_boundary$ADM1_EN),
                       weight = 3,fillColor = "transparent") %>% 
  
  leaflet::addPolygons(data = admin2_boundary,
                       color = "#58585A",
                       label = ~htmlEscape(ADM2_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "12px"
                                                   )),
                       
                       popup = paste("Governorate:", admin2_boundary$ADM1_EN, "<br>",
                                     "District:", admin2_boundary$ADM2_EN),
                       
                       weight = 1,fillColor = "transparent",group = "District") %>% 
  
  leaflet::addPolygons(data = admin3_boundary,
                       color = "#F69E61",
                       label = ~htmlEscape(ADM3_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "Arial Narrow",
                                                     "font-size" = "10px",
                                                     "font-style" = "italic"
                                                   )),
                       popup = paste("Governorate:", admin3_boundary$ADM1_EN, "<br>",
                                     "District:", admin3_boundary$ADM2_EN, "<br>",
                                     "Sub-District:", admin3_boundary$ADM3_EN),
                       
                       weight = 1,dashArray = "9",
                       fillColor = "transparent",
                       group = "Sub-district") %>% 
  
  addLayersControl(
    overlayGroups = c("District", "Sub-district"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("Sub-district")) %>% 
  groupOptions("District", zoomLevels = 9:13) %>% 
  groupOptions("Sub-district", zoomLevels = 10:20) 



# folder_create -----------------------------------------------------------

if(create_directory ==T){

for (i in unique(data$Governorate)) {

  dir.create(path = paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",i))
}



for (i in unique(data$District)) {
  data_district <- data %>% filter(District == i)

  gov <- (data_district %>% as.data.frame())$Governorate %>% unique() %>% dput

  dir.create(path = paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",gov,"/",i))
}



for (i in unique(data$Sub.District)) {

  data_sub_district <- data %>% filter(Sub.District == i)

  gov <- (data_sub_district %>% as.data.frame())$Governorate %>% unique() %>% dput
  dis <- (data_sub_district %>% as.data.frame())$District %>% unique() %>% dput

  dir.create(paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",gov,"/",dis,"/",i))
}

for (i in unique(data$site_name_unique)) {

  data_site_name_unique <- data %>% filter(site_name_unique == i)

  gov <- (data_site_name_unique %>% as.data.frame())$Governorate %>% unique() %>% dput
  dis <- (data_site_name_unique %>% as.data.frame())$District %>% unique() %>% dput
  sub_dis <- (data_site_name_unique %>% as.data.frame())$Sub.District %>% unique() %>% dput

  dir.create(paste0("02_outputs/Sampling/sub_district_level/Final_sampling/kmls/",gov,"/",dis,"/",sub_dis,"/",i))
}

}

# sampling ----------------------------------------------------------------

cords <- c("Site.Longitude","Site.Latitude")
df_st <- data %>% st_as_sf(coords =cords,crs = 4326) 


for (i in unique(df_st$site_name_unique)){
  print(i)
 site_df <-   df_st %>% filter(site_name_unique ==i)
 


 gov <- (site_df %>% as.data.frame())$Governorate %>% unique() %>% dput
 dis <- (site_df %>% as.data.frame())$District %>% unique() %>% dput
 sub_dis <- (site_df %>% as.data.frame())$Sub.District %>% unique() %>% dput
 sites <- (site_df %>% as.data.frame())$site_name_unique %>% unique() %>% dput
 
 
 site_df <- site_df %>% mutate(
   threshold = round(X..households/survey_buffer),
   label = paste0("Site_name:",site_df$site_name_unique," ",
                  "Surveys_needed:",site_df$survey_buffer," ",
                  "Families:", site_df$X..households," ",
                  "Threshold:", threshold
                  )
 ) %>% select(label,everything())
 
 
# map ---------------------------------------------------------------------

site_df <- site_df %>% mutate( 
long = st_coordinates(site_df)[[1]],
lat = st_coordinates(site_df)[[2]]
)

 

# css ---------------------------------------------------------------------

 tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
")) 
 
 title <- tags$div(
   tag.map.title, HTML(site_df$site_name_unique)
                       
 )  



leaflet_st_point <-  base_map %>% 
  leaflet::addMarkers(data = site_df,~long,~lat,
                      labelOptions = labelOptions(noHide = T,
                                                  direction = 'center',
                                                  textOnly = T,
                                                  style = list(
                                                    "font-family" = "Arial Narrow",
                                                    "font-size" = "15px",
                                                    "font-style" = "italic",
                                                    "font-color" = "red"
                                                  )),
                      label = paste0(site_df$site_name_unique,"_",
                                     "PT_NEEDED_:",site_df$survey_buffer, 
                                     "families:", site_df$X..households,
                                     "Threshold:", site_df$threshold
                                     ),
                      popup = ~paste0("Site_name:",site_df$site_name_unique,"<br>",
                                      "Governorate:", site_df$Governorate,"<br>",
                                      "District:", site_df$District,"<br>",
                                      "Sub_district:",site_df$Sub.District,"<br>", 
                                      "Total_number_of_hh:", site_df$X..households, "<br>",
                                      "Threshold:", site_df$threshold, "<br>",
                                      "Surveys_needed:",site_df$survey_buffer)) %>% 
  addControl(title, position = "topleft", className="map-title") %>% setView(lat = site_df$lat,lng = site_df$long,zoom = 11)



saveWidget(leaflet_st_point, "map.html",title = sites)


file_name_to <- paste0(write_kl,"/",gov,"/",dis,"/",sub_dis,"/",sites,"/ID-",site_df$ID.,
                       "_families-", site_df$X..households,
                       "_threshold-",site_df$threshold,
                       "_surveys-",site_df$survey_buffer,
                       ".html")
file.copy(from = "map.html",to = file_name_to,overwrite = T)

 

plotKML(sf::as_Spatial(site_df),
         i,
         kmz=FALSE,altitude=0,plot.labpt=TRUE,
         LabelScale=0.8)


file_name_to_kml  <- paste0(write_kl,"/",gov,"/",dis,"/",sub_dis,"/",sites,"/ID-",site_df$ID.,
                            "_families-", site_df$X..households,
                            "_threshold-",site_df$threshold,
                            "_surveys-",site_df$survey_buffer,".kml")


file.copy(from = paste0(i,".kml"),to = file_name_to_kml,overwrite = T)  

}

