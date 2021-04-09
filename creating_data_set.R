# creating the data set
library(tidyverse)
library(readxl)

df1 <- read_excel(path = 'QVC Data 1.xlsx')
df2 <- read_excel(path = 'QVC Data 2.xlsx')
df3 <- read_excel(path = 'QVC Data 3.xlsx')
df4 <- read_excel(path = 'QVC Data 4.xlsx')
df5 <- read_excel(path = 'QVC Data 5.xlsx')
df6 <- read_excel(path = 'QVC Data 6.xlsx')
dist_cent <- read_excel('QVCdist_ctr.xlsx')

#### add region to dist center ####
dist_cent <- dist_cent %>% rowwise() %>% 
  mutate(dist_region_8= ifelse(REGN_CD %in% north_east,'north_east',
                               ifelse((REGN_CD %in% south_atlantic),'south_atlantic',
                                      ifelse((REGN_CD %in% south_atlantic),'south_atlantic',
                                             ifelse((REGN_CD %in% east_south_central),'east_south_central',
                                                    ifelse((REGN_CD %in% west_south_central),'west_south_central',
                                                           ifelse((REGN_CD %in% east_north_central),'east_north_central',
                                                                  ifelse((REGN_CD %in% west_north_central),'west_north_central',
                                                                         ifelse((REGN_CD %in% mountain_reg),'mountain_reg',
                                                                                ifelse((REGN_CD %in% pacific_reg),'pacific_reg',NA))))))))))

#### fixing date difference to intiger ####
df$oder_til_ship <- as.integer(df$oder_til_ship)
df$time_dif <- as.integer(df$time_dif)
df$SHIP_TO_ZIP <- as.character(df$SHIP_TO_ZIP)
dist_cent$Source_Ship_Warehouse_Nbr <- as.integer(dist_cent$Source_Ship_Warehouse_Nbr)
#dist_cent$Source_Ship_Warehouse_Nbr <- as.character(dist_cent$Source_Ship_Warehouse_Nbr)
#df$Source_Ship_Warehouse_Nbr <- as.integer(df$Source_Ship_Warehouse_Nbr)
df <- df %>% filter(oder_til_ship >=0)


#adding an extra column to df4 and df5
df4 <- df4 %>% mutate(`...37`= NA)
df6 <- df6 %>% mutate(`...37`= NA)

# creating one dataset
df <- rbind(df1,df2,df3,df4,df5,df6)

#### Taking the unimportant columns out####
un_important_columns <- c('...37','SHIP_TO_CITY')
df <- select(df,-one_of(un_important_columns))

#### Finding how long shiping takes difference between ship date and confirmation date ####
df <- df %>% mutate(time_dif = difftime(Delivery_Confirmation_Dt,Shipped_Dt,units = 'days'))

#removing rows that had a negetive delivery date since it is most likely a entery mistake
df <- df %>% filter(time_dif > 0)


#### date order placed till ship ####
df <- df %>% mutate(oder_til_ship = difftime(Shipped_Dt,Order_Dt,units = 'days'))
df <- df %>% filter(Shipped_Dt > 0) #filter out orders that have negetive time
# finding average processing
temp <- df
avg_process <- temp %>% group_by(Product_Id) %>% summarise(avg_process_time = mean(oder_til_ship))
avg_process$avg_process_time <- round(avg_process$avg_process_time)
df <- inner_join(temp,avg_process,by = 'Product_Id')
rm(temp)
df <- df %>% filter(avg_process_time > 0)
#### Finding the average time for each product shiping ####
temp <- df
product_avg_ship <- temp %>% group_by(Product_Id) %>% summarise(avg_ship_time = mean(time_dif))
product_avg_ship$avg_ship_time <- round(product_avg_ship$avg_ship_time)
df <- inner_join(df,product_avg_ship,by = 'Product_Id')
rm(temp)

#### joining warehouse and df ####
df <- inner_join(df,dist_cent, by = "Source_Ship_Warehouse_Nbr")

#### Adding number of products overall ####
temp <- df %>% count(Product_Id)
colnames(temp) <- c('Product_Id','Overall_count')
df <- inner_join(df,temp,by = 'Product_Id')
rm(temp)

#### adding city_name ####
df <- inner_join(df,zipcode, by= c('SHIP_TO_ZIP'='zip'))

#### Creating regions####
east_coast <- c('ME','NH','VT','MA','RI','CT','NY','NJ','PA','WV','VA','DE','MD','DC','NC','SC','GA','FL')
west_coast <- c('WA','OR','CA','NV','ID','MT','UT','AZ','WY','CO','NM')
middle_atlantic <- c('NY','PA','NJ')
new_england <- c('MA','ME','NH','VT','RI','CT')
south_atlantic <- c('DE','MD','DC','VA','WV','NC','SC','GA','FL')
east_south_central <- c('KY','TN','AL','MS')
west_south_central <- c('LA','AR','TX','OK')
east_north_central <- c('WI','MI','OH','IN','IL')
west_north_central <- c('MN','IA','MO','KS','NE','SD','ND')
mountain_reg <- c('MT','WY','ID','UT','NV','CO','AZ','NM')
pacific_reg <- c('WA','OR','CA')
south_reg <- c(south_atlantic,east_south_central,west_south_central)
northeast_reg <- c(middle_atlantic,new_england)
west_reg <- c(pacific_reg,mountain_reg) 
midwest_reg <- c(west_north_central,east_north_central)
south_reg <- c(west_south_central,east_south_central,south_atlantic)

#### adding 8 regions to df (df1 is last save)####
df <- df %>% rowwise() %>% 
  mutate(cust_region_8= ifelse(state %in% north_east,'north_east',
                                                          ifelse((state %in% south_atlantic),'south_atlantic',
                                                                 ifelse((state %in% south_atlantic),'south_atlantic',
                                                                        ifelse((state %in% east_south_central),'east_south_central',
                                                                               ifelse((state %in% west_south_central),'west_south_central',
                                                                                      ifelse((state %in% east_north_central),'east_north_central',
                                                                                             ifelse((state %in% west_north_central),'west_north_central',
                                                                                                    ifelse((state %in% mountain_reg),'mountain_reg',
                                                                                                           ifelse((state %in% pacific_reg),'pacific_reg',NA))))))))))

#### adding 8 regions for distributor ####
df <- df %>% rowwise() %>% 
  mutate(dist_region_8= ifelse(REGN_CD %in% north_east,'north_east',
                               ifelse((REGN_CD %in% south_atlantic),'south_atlantic',
                                      ifelse((REGN_CD %in% south_atlantic),'south_atlantic',
                                             ifelse((REGN_CD %in% east_south_central),'east_south_central',
                                                    ifelse((REGN_CD %in% west_south_central),'west_south_central',
                                                           ifelse((REGN_CD %in% east_north_central),'east_north_central',
                                                                  ifelse((REGN_CD %in% west_north_central),'west_north_central',
                                                                         ifelse((REGN_CD %in% mountain_reg),'mountain_reg',
                                                                                ifelse((REGN_CD %in% pacific_reg),'pacific_reg',NA))))))))))





#### Adding distribution/shipping centers ####
ship_cen <- df %>% filter(city %in% c('Lancaster','Bethlehem','Suffolk','Florence','Rocky Mount','Ontario'))  

ship_cen <- sqldf("select city,state,latitude,longitude,dist_region_8 from ship_cen 
              where (city = 'Lancaster' and state = 'PA') or
              (city = 'Bethlehem' and state = 'PA') or
              (city = 'Suffolk' and state = 'VA') or
              (city = 'Florence' and state = 'SC') or
              (city = 'Rocky Mount' and state = 'NC') or
              (city = 'Ontario' and state = 'CA')
              group by city,state")
CD_number <- c(1997,150,530,125,540,200)
ship_cen <- cbind(ship_cen,CD_number)

####Saving the data ####
save(df,file="df.Rda")
save(dist_cent,file='dist_centr.Rda')
save(ship_cen,file = 'ship_cen.rda')
