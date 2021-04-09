#### creating the data set ####
library(tidyverse)
library(lubridate)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(readxl)
library(sf)
library(zipcode)
library(urbnmapr)
library(gridExtra)
library(urbnmapr)
library(ggrepel)

#### laoding the dataset and dictionary ####
load("df.Rda")
load('dist_centr.Rda')
load('ship_cen.rda')
data("zipcode")

#### laoding map functions ####
load('draw_cust_dist.Rda')
load('show_cust_and_dist.Rda')
load('show_cust_and_poten_dist.Rda')
load('show_all_cust_dist.Rda')
load('show_cust_cd.Rda')
CD_number <- c(1997,150,530,125,540,200)
#### Creating plot maps ####

map_us <- read_sf('USA_Major_Cities/0c5a2fa1-3463-4fc7-99e5-e206023a7e682020313-1-nmlntc.mln9c.shp')
map_state <- read_sf('US_States-shp/81463890-e860-45bd-84ad-5c8fbdca8849202044-1-19ws3da.tb4z.shp')
map_zip <- read_sf('tl_2019_us_zcta510/tl_2019_us_zcta510.shp')

#removing teh teretories
map_state <- map_state %>% filter(!STUSPS %in% c("AK","HI","AS","FM","GU","MH","MP","PW","PR","VI"))

#### all teh customers and dist cebters ####

map1 <- show_all_cust_dist(df)


#### Finding Distribution centers that had most delay for products based on average process time delay (7 days)####
#chose 50 so orders that have a higher sale volume

temp <- df %>% filter(oder_til_ship > avg_process_time+10)
#head(sort(table(bd$Product_Id),decreasing = T)) 
# results in 361200001 most common product (home decor)

#### mapping result for 361200001 which are 7 days late####
temp1 <- gg %>% filter(time_dif >= 7)
map2 <- show_cust_and_dist(product_id = 361200001,cust_alpha = 0.9,data_df = temp1,g_title = 'Cases where item 361200001 a Home Decor product was delivered more than 7 days')
map2

ggsave(plot = map2,filename = 'HD_7day_late.jpeg',width = 9,height = 8)

#### warehouse and distribution centers ####
g_ware_dist <- show_ware_dist(df)
ggsave(plot = g_ware_dist,filename = 'g_ware_dist.jpeg',height = 5,width = 8)

#### mapping all the individuals that bought this item and potential dist centers####
map3 <- show_cust_and_dist(387600061,df,cust_alpha = 1)
map4 <- show_cust_and_poten_dist(387600061,df,cust_alpha = 1,regions = 'east_north_central')

#### average ship time map ####
states_sf <- get_urbn_map("states", sf = TRUE)
temp1 <- df %>% group_by(state) %>% summarise(round(mean(time_dif)))
colnames(temp1) <- c('state','avg_time_dif')
temp1 <- inner_join(states_sf,temp1,by=c('state_abbv'='state'))

temp2 <- df %>% group_by(state) %>% count(state)
colnames(temp2) <- c('state','n')
temp2 <- inner_join(states_sf,temp2,by=c('state_abbv'='state'))

g_avg_ship <- ggplot()+ theme_bw()+
  geom_sf(data = states_sf,fill='#ffffff',size = 0.05)+
  geom_sf(data = temp1,aes(fill=avg_time_dif))+
  #geom_sf_text(data = temp2,aes(label=n),size=2.5)+
  scale_fill_gradient(low = "#e5ffcc", high = "#3d700b", na.value = NA)+
  coord_sf(datum = NA)+
  labs(title = 'Average shipping time for each state',
       x=element_blank(),y=element_blank(),fill='shipping time\nscale')+
  theme(strip.text.x = element_text(size = 12,face = 'bold'))
g_avg_ship

ggsave(plot = g_avg_ship,filename = 'g_avg_ship.jpeg',width = 8,height = 5)


#### products that have process tiem of more than 2 weeks ####

temp2 <- df %>% filter(Source_Ship_Warehouse_Nbr %in% CD_number)
temp3 <- sqldf("select Source_Ship_Warehouse_Nbr,Product_Id,Merchandise_Dept_Desc,count(*) as n 
               from temp2
               where (oder_til_ship >=14)  
               group by Source_Ship_Warehouse_Nbr,Product_Id")

temp4 <- temp3 %>%group_by(Source_Ship_Warehouse_Nbr) %>%
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% filter(my_ranks <=5)

temp5 <- ship_cen %>% mutate(warehouse_nm = paste(city,",",state)) %>% 
  select(warehouse_nm,CD_number)

temp4 <- inner_join(temp4,temp5,by=c('Source_Ship_Warehouse_Nbr'='CD_number'))

g_CD_process_14 <- temp4 %>% ggplot(aes(x= as.factor(Product_Id),y= n))+
  geom_bar(aes(fill = Merchandise_Dept_Desc),stat = 'identity')+
  facet_wrap(~warehouse_nm,scales = 'free')+
  coord_flip()+
  theme_classic()+
  labs(title = 'Top 5 items for each distribution center that have processing time longer than 2 weeks ',
       fill='Product Department')+
  ylab(label = 'Total items sold')+
  xlab(label = 'Product ID')+
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
    #plot.background = element_rect(fill = "#ffffff"),
    axis.title.x = element_text(size =12,face='bold'),
    axis.title.y = element_text(size =12,face='bold'),
    #strip.text.y = element_blank()
  )

ggsave(plot = g_CD_process_14,filename = 'g_CD_process_14.jpeg',height = 7,width = 15)
#### count sale by state ####
g_state_sale <- show_cus_sales(data_df = df)
ggsave(plot = g_state_sale,filename = 'g_state_sale.jpg',width = 8,height = 5)
#### popoular product sales over weeks ####
fluc_graph <- function(data_df,productid){ 
  #leg_color <- c('avg_price' = '#19d426','Items sold' = '#1688db')
  gg <- productid
  b <- week_order_count(data_df = data_df,productid = gg)
  gg <- as.character(gg)
  graph_1 <- b %>% ggplot(aes(x=week))+
    geom_line(aes(y=avg_price*150,color = 'Average price'),size=1.5)+
    geom_line(aes(y=n_count,color = 'Items sold'),size=1.5)+
    scale_y_continuous(sec.axis = sec_axis(~./200, name = "Average Price"))+
    theme_minimal()+
    ylab("Items sold")+
    labs(title = paste("Weekly sales for item",gg),color='legend')+
    scale_color_manual(values = c('#19d426','#1688db'))+
    theme(
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 8),
      plot.caption = element_text( size = 8),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'))
  
  return(graph_1)}


g_320400033 <- fluc_graph(data_df = temp,productid = 320400033)
g_269200894 <- fluc_graph(data_df = temp,productid = 269200894)
g_265601042 <- fluc_graph(data_df = temp,productid = 265601042)
g_274201195 <- fluc_graph(data_df = temp,productid = 274201195)
g_210100177 <- fluc_graph(data_df = temp,productid = 210100177)
g_304600029 <- fluc_graph(data_df = temp,productid = 304600029)

g_320400033
g_269200894
g_265601042
g_274201195
g_210100177
g_304600029

g_combine <- grid.arrange(g_320400033,g_269200894,g_265601042,g_274201195,g_210100177,g_304600029)

ggsave(plot = g_combine,filename ='g_combined.jpeg',width = 12,height = 8)
