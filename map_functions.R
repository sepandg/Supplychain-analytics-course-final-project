# added shipminh dist to all teh graphs



draw_cust_dist <- function(product_id,data_df){
  gg <- data_df %>%filter(Product_Id == product_id)
  #selecting distrubution centers that send out these products
  dist_gg <- sqldf("select dist_region_8 as dist_region,CITY_NM as NAME,REGN_CD as state from gg group by CITY_NM,REGN_CD")
  dist_gg$NAME <- tolower(dist_gg$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_gg <- inner_join(map_us,dist_gg,by=c('NAME'='NAME',"ST" = "state"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_gg <- gg %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_gg <- sqldf("select city,state,latitude,longitude, dist_region_8 from cust_zip_gg group by city,state")
  
  cust_city_gg_sf <- cust_city_gg %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_gg <- st_transform(cust_city_gg_sf,crs = 2163)
  
  
  #potential dist centers
  dist_poten <- dist_cent %>% filter(dist_region_8 == 'east_north_central')
  dist_poten$CITY_NM <- tolower(dist_poten$CITY_NM)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_poten <- inner_join(map_us,dist_poten,by=c('NAME'='CITY_NM',"ST" = "REGN_CD"))
  
  #plot
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t_gg,aes(color=dist_region_8),alpha = 0.8)+
    scale_color_manual(values = c('east_north_central'= '#76a8f5',
                                  'east_south_central'='#f7aa3e',
                                  'mountain_reg'='#77e362',
                                  'north_east'='#d9ca43',
                                  'pacific_reg'='#cc6ee6',
                                  'south_atlantic'='#7ce6db',
                                  'west_north_central'='#b7ed5a',
                                  'west_south_central'='#09ba76'))+
    scale_shape_manual(values=c('east_north_central'=16 ,
                                'east_south_central'=7,
                                'mountain_reg'=10,
                                'north_east'=15,
                                'pacific_reg'=19,
                                'south_atlantic'=17,
                                'west_north_central'=12,
                                'west_south_central'=18))+
    geom_sf(data = map_us_gg,size=3.6,color ='#ff0000',aes(shape=dist_region))+
    theme_minimal()
  return(map)
}
#### show customer and dist centers for that product ####
show_cust_and_dist <- function(product_id,data_df,cust_alpha = 0.4,g_title){
  
  bd <- data_df %>% filter(Product_Id == product_id)
  
  #selecting distrubution centers that send out these products
  dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from bd group by CITY_NM,REGN_CD")
  dist_bd$NAME <- tolower(dist_bd$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_bd <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_bd <- bd %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_bd <- sqldf("select city,state,latitude,longitude from cust_zip_bd group by city,state")
  cust_city_bd <- cust_city_bd %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  #ading ship centers
  cust_city_bd_sf <- cust_city_bd %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_bd <- st_transform(cust_city_bd_sf,crs = 2163)
  
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  

  #plot
  g_title <- as.character(g_title)
  
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
    geom_sf(data = map_us_bd,size=2,color ='red')+
    #geom_sf(data = late_bd_df,color='purple')+
    theme_minimal()+
    labs(title = g_title)+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.caption = element_blank(),
      plot.title = element_text(size = 14),
      plot.subtitle = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 8),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  
  return(map)
}

#### customer and dist for a product with potential places ####
show_cust_and_poten_dist <- function(product_id,data_df,cust_alpha = 0.4,regions){
  bd <- data_df %>% filter(Product_Id == product_id)
  
  #selecting distrubution centers that send out these products
  dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from bd group by CITY_NM,REGN_CD")
  dist_bd$NAME <- tolower(dist_bd$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_bd <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))
  
  #potential dist centers
  dist_poten <- dist_cent %>% filter(dist_region_8 %in% regions)
  dist_poten$CITY_NM <- tolower(dist_poten$CITY_NM)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_poten <- inner_join(map_us,dist_poten,by=c('NAME'='CITY_NM',"ST" = "REGN_CD"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_bd <- bd %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_bd <- sqldf("select city,state,latitude,longitude from cust_zip_bd group by city,state")
  cust_city_bd <- cust_city_bd %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  
  cust_city_bd_sf <- cust_city_bd %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_bd <- st_transform(cust_city_bd_sf,crs = 2163)
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  #plot
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
    geom_sf(data = map_us_bd,size=2,color ='red')+
    geom_sf(data = map_us_poten,size=2,alpha = 0.4,color ='darkgreen')+
    geom_sf(data = ship_cen_sf_t,color='purple')+
    theme_minimal()
  
  return(map)
}

#### Show all cust and dist ####
show_all_cust_dist <- function(data_df){
  dist_n <- sqldf("select CITY_NM as NAME,REGN_CD as state from dist_cent group by CITY_NM,REGN_CD")
  dist_n$NAME <- tolower(dist_n$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_dist <- inner_join(map_us,dist_n,by=c('NAME'='NAME',"ST" = "state"))
  
  #finding the number of customers for each city and joining for long/lat
  
  cust_zip <- data_df %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                    longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city <- sqldf("select city,state,latitude,longitude from cust_zip group by city,state")
  cust_city <- cust_city %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  cust_city_sf <- cust_city %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t <- st_transform(cust_city_sf,crs = 2163)
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t,alpha = 0.18,color = 'lightblue')+
    geom_sf(data = map_us_dist,size=2,color ='red')+
    geom_sf(data = ship_cen_sf_t,color='purple')+
    theme_minimal()
  return(map)
}


#### show CD and cust it serves ####
show_cust_cd <- function(source,data_df,cust_alpha = 0.4){
  bd <- data_df %>% filter(Source_Ship_Warehouse_Nbr == source )
  
  #selecting distrubution centers that send out these products
  dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from bd group by CITY_NM,REGN_CD")
  dist_bd$NAME <- tolower(dist_bd$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_bd <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_bd <- bd %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_bd <- sqldf("select city,state,latitude,longitude from cust_zip_bd group by city,state")
  cust_city_bd <- cust_city_bd %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  #ading ship centers
  cust_city_bd_sf <- cust_city_bd %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_bd <- st_transform(cust_city_bd_sf,crs = 2163)
  
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  #plot
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
    geom_sf(data = map_us_bd,size=2,color ='red')+
    theme_minimal()+
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 8),
      plot.caption = element_text( size = 8),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 8),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'))
  
  return(map)
}

#### test show_cust2 ####
show_cust_cd2 <- function(source,data_df,cust_alpha = 0.4){
  bd <- data_df %>% filter(Source_Ship_Warehouse_Nbr == source )
  
  #
  top_delayed <- as.data.frame(head(sort(table(data_df$city),decreasing = T),20))
  a <- unique(top_delayed$Var1)
  bd <- bd %>% filter(city %in% a)
  #selecting distrubution centers that send out these products
  dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from bd group by CITY_NM,REGN_CD")
  dist_bd$NAME <- tolower(dist_bd$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_bd <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_bd <- bd %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_bd <- sqldf("select city,state,latitude,longitude from cust_zip_bd group by city,state")
  cust_city_bd <- cust_city_bd %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  #ading ship centers
  cust_city_bd_sf <- cust_city_bd %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_bd <- st_transform(cust_city_bd_sf,crs = 2163)
  
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  #plot
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
    geom_sf(data = map_us_bd,size=2,color ='red')+
    theme_minimal()
  
  return(map)
}


#### show ####
show_cust <- function(data_df,cust_alpha = 0.4,g_title){
  
  bd <- data_df
  temp1 <- bd %>% group_by(state) %>% summarise(mean(time_dif))
  colnames(temp1) <- c('state','avg_time_dif')
  #selecting 
  map_us_bd <- inner_join(map_us,temp1,by=c("ST" = "state"))
  
  #selcting custoemrs that bought this item
  
  cust_zip_bd <- bd %>% filter((!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))&
                                 longitude < (-10)) #just focusing on the mainland US hence -10
  cust_city_bd <- sqldf("select city,state,latitude,longitude from cust_zip_bd group by city,state")
  cust_city_bd <- cust_city_bd %>% filter(!state %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))
  
  #ading ship centers
  cust_city_bd_sf <- cust_city_bd %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  cust_city_sf_t_bd <- st_transform(cust_city_bd_sf,crs = 2163)
  
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  
  #plot
  g_title <- as.character(g_title)
  
  map <- ggplot()+
    geom_sf(data = map_state,fill='#ffffff',size=1) +
    #geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
    geom_sf(data = map_us_bd,aes(fill=avg_time_dif),size=2,color ='red')+
    #geom_sf(data = late_bd_df,color='purple')+
    theme_minimal()+
    labs(title = g_title)+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.caption = element_blank(),
      plot.title = element_text(size = 14),
      plot.subtitle = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 8),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  
  return(map)
}
#### Show warehouse and dist ####
show_ware_dist <- function(data_df,cust_alpha = 0.4){
    bd <- data_df 
    
    #selecting distrubution centers that send out these products
    dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from bd group by CITY_NM,REGN_CD")
    dist_bd$NAME <- tolower(dist_bd$NAME)
    map_us$NAME <- tolower(map_us$NAME)
    map_us_bd <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))

        ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
    ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
    
    #plot
    map <- ggplot()+
      geom_sf(data = map_state,fill='#ffffff',size=1) +
      #geom_sf(data = cust_city_sf_t_bd,alpha = cust_alpha,color = 'lightblue')+
      geom_sf(data = map_us_bd,aes(color='red'),size=1.5,color ='red')+
      #geom_sf(data = map_us_poten,size=2,alpha = 0.4,color ='darkgreen')+
      geom_sf(data = ship_cen_sf_t,aes(color='purple'),size= 2,color='purple')+
      #geom_sf_label(data =ship_cen_sf_t,aes(label= city))+
      theme_minimal()+
      labs(title = 'QVC warehouse(red) and Distribution centers (purple)')+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.background = element_rect(fill = "#ffffff"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
      
    
    return(map)
}
#### orders and dist ship ####
show_cus_sales <- function(data_df){
  states_sf <- get_urbn_map("states", sf = TRUE)
  states_sf <- states_sf %>% filter(!state_abbv %in% c("AK",'HI'))
  temp1 <- data_df %>% filter(!state %in% c("AK",'HI'))%>% group_by(state) %>% count(state)
  temp1 <- temp1 %>% mutate(count_scale = n/1000)
  colnames(temp1) <- c('state','n','total_count')
  temp1 <- inner_join(states_sf,temp1,by=c('state_abbv'='state'))
  
  temp2 <- data_df %>% group_by(state) %>% count(state)
  colnames(temp2) <- c('state','n')
  temp2 <- inner_join(states_sf,temp2,by=c('state_abbv'='state'))
  
  dist_bd <- sqldf("select CITY_NM as NAME,REGN_CD as state from data_df group by CITY_NM,REGN_CD")
  dist_bd$NAME <- tolower(dist_bd$NAME)
  map_us$NAME <- tolower(map_us$NAME)
  map_us_bd2 <- inner_join(map_us,dist_bd,by=c('NAME'='NAME',"ST" = "state"))
  
  
  ship_cen_sf <- ship_cen %>% st_as_sf(coords=c("longitude","latitude"),crs=4326)
  ship_cen_sf_t <- st_transform(ship_cen_sf,crs = 2163)
  
  
  g_avg_ship <- ggplot()+ theme_bw()+
    geom_sf(data = states_sf,fill='#ffffff',size = 0.05)+
    geom_sf(data = temp1,aes(fill=total_count))+
    geom_sf(data = map_us_bd2,size=1.5,color ='red',alpha=0.6)+
    geom_sf(data = ship_cen_sf_t,color='purple',size=2)+
    scale_fill_gradient(low = "#e5ffcc", high = "#3d700b", na.value = NA)+
    coord_sf(datum = NA)+
    labs(title = 'Total order count for each state in mainland USA',
         x=element_blank(),y=element_blank(),fill='Order count scale \nin thousand')+
    theme(strip.text.x = element_text(size = 12,face = 'bold'))
  return(g_avg_ship)
}
#### Saving maps ####
save(draw_cust_dist,file = 'draw_cust_dist.Rda')
save(show_cust_and_dist,file = 'show_cust_and_dist.Rda')
save(show_cust_and_poten_dist,file = 'show_cust_and_poten_dist.Rda')
save(show_all_cust_dist, file = 'show_all_cust_dist.Rda')
save(show_cust_cd,file = 'show_cust_cd.Rda')
save(show_cus_sales,file = 'show_cus_sales.Rda')