library(reshape)

temp <- df %>% filter(!state %in% c('AK','HI'))
temp <- temp %>% group_by(Product_Id,cust_region_8) %>% tally()
temp <- cast(temp,Product_Id~cust_region_8)

a <- temp %>% filter((east_north_central>=50)&(east_south_central>=50)&
                     (mountain_reg>=50)&(north_east>=50)&
                     (pacific_reg>=50)&(south_atlantic>=50)&
                     (west_north_central>=50)&(west_south_central>=50))
                  
#finding a product that is popoular in pacific than any otehr region
a <- temp %>% filter(pacific_reg > 
                       (east_north_central+east_south_central+mountain_reg+north_east+
                          south_atlantic+west_north_central+west_south_central))
#a <- temp %>% filter( mountain_reg> 
#                       (east_north_central+east_south_central+pacific_reg+north_east+
#                          south_atlantic+west_north_central+west_south_central))
a <- temp %>% filter( east_north_central> 
                        (mountain_reg+east_south_central+pacific_reg+north_east+
                           south_atlantic+west_north_central+west_south_central))
#a <- temp %>% filter( east_south_central> 
#                        (east_north_central+mountain_reg+pacific_reg+north_east+
#                           south_atlantic+west_north_central+west_south_central))
a <- temp %>% filter( north_east> 
                        (east_north_central+east_south_central+pacific_reg+mountain_reg+
                           south_atlantic+west_north_central+west_south_central))
a <- temp %>% filter( south_atlantic> 
                        (east_north_central+east_south_central+pacific_reg+north_east+
                           mountain_reg+west_north_central+west_south_central))
#a <- temp %>% filter( west_north_central> 
#                        (east_north_central+east_south_central+pacific_reg+north_east+
#                           south_atlantic+mountain_reg+west_south_central))
#a <- temp %>% filter( west_south_central> 
#                        (east_north_central+east_south_central+pacific_reg+north_east+
#                           south_atlantic+west_north_central+mountain_reg))





