#Data Translation Challenge
#The aim of this assignment is to analyze Amazon's 2019 Online Product Sales Data to find some interesting analytics. Further, we need to develop 5 well presented Graphics that are interconnected and support a common story narrative.
#Prompt: Amazon's Logistics Division plans for product imports. Its primary target is to optimize its import and transportation cost against the revenue such that the profit is maximized. A bulk of imports are heavy lifted by the shipping industry (which is cheaper than air transportation of goods). It is in the interest of this division to find out the breakdown of import cost.
#According to CNBC [1], a standard Sea Freight shipment of 40 foot container from china to West Coast costs about $20,000. Similar container costs $14,000 to East Coast. Given that these prices have risen about 500% from their earlier price point in 2020,
#-> it is imperative that Amazon focuses on Consumer Electronics that have high purchase frequency in the States. A Plot to understand this would make this comparison easier.
#->It is also interesting to observe the growth rate of products with a high 'premium price vs purchase frequency' ratio among their respective categories.
#Audience: Amazon's General Managers, Logistic Staff, Upper Management and Port and Warehouse Level Employees.
#Foray into the data| Product Frequency Distribution
#We first look at the Frequency distribution of Different product by their product categories, such that similar products from different brands are clubbed together

#importing the libraries
library(dplyr)
library(kableExtra)
library(vtable)
library(scales)
library(lubridate)
library(ggplot2)
library(stringr)
library(gridExtra)

#changing the directory 
setwd("C:/Users/hitma/Documents/Python Scripts/Data_visualization_class/Data Translation Challenge")

#importing the dataset
load("sales_data (1).Rdata")
dat_zip <- read.csv("zip_info.csv")

#Looking at the frequency distibution
dt <- sales %>%
  group_by(Product) %>%
  summarize(ProductFrequency= n()) %>%
  arrange(desc(ProductFrequency))

dt %>%
  kbl() %>%
  kable_material_dark() 
  
  #distribution of Purchase frequency of USB C charging cable by state

# zip_dat<-sales %>%
#   group_by(ZIP) %>%
#   summarize(ZipFrequency= n()) %>%
#   arrange(ZipFrequency, ...=desc())

usbc_dat <- sales %>% 
  filter(str_detect(Product,'USB-C'))

grp_state_usbc <- usbc_dat %>%
  group_by(State)%>%
  summarize(TotalPurchases= n())


plt_1 <- grp_state_usbc %>%
  arrange(desc(TotalPurchases))%>%
  ggplot( aes(x=State, y=TotalPurchases)) +
    geom_segment( aes(xend=State, yend=0)) +
    geom_point( size=4, color="orange") +
    theme_bw() +
    xlab("") +
    theme_classic()+
    scale_y_continuous(breaks= scales::pretty_breaks(n=10))+
  ylab('# of items sold')+
  xlab('Us States')+
  theme(plot.background=element_rect(fill="#651e3e"),
        panel.background = element_rect(fill="#651e3e"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
   labs(title="US State that buys USB-C Charger the most",
       subtitle=str_wrap("Distribution of Number of Amazon Purchases of USB-C charger among different US states in 2019",75),
       caption = "Data Recorded in 1st Jan 2019- 1 Jan 2020")

plt_1

#For lightning cable
lightc_dat <- sales %>% 
  filter(str_detect(Product,'Lightning Charging Cable'))

grp_state_lightc <- lightc_dat %>%
  group_by(State)%>%
  summarize(TotalPurchases= n())

plt_2 <- grp_state_lightc %>%
  arrange(desc(TotalPurchases))%>%
  ggplot( aes(x=State, y=TotalPurchases)) +
    geom_segment( aes(xend=State, yend=0)) +
    geom_point( size=4, color="orange") +
    theme_bw() +
    xlab("") +
    theme_classic()+
    scale_y_continuous(breaks= scales::pretty_breaks(n=10))+
  ylab('# of items sold')+
  xlab('Us States')+
  theme(plot.background=element_rect(fill="#451e3e"),
        panel.background = element_rect(fill="#451e3e"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
   labs(title="US State that buys Lightning Cable the most",
       subtitle=str_wrap("Similar Distibution for Lightning Cable",75),
       caption = "Data Recorded in 1st Jan 2019- 1 Jan 2020")
plt_2


#For lightning cable
aaa_dat <- sales %>% 
  filter(str_detect(Product,'AAA'))

grp_state_aaa <- aaa_dat %>%
  group_by(State)%>%
  summarize(TotalPurchases= n())

plt_3 <- grp_state_lightc %>%
  arrange(desc(TotalPurchases))%>%
  ggplot( aes(x=State, y=TotalPurchases)) +
    geom_segment( aes(xend=State, yend=0),color='white') +
    geom_point( size=4, color="orange") +
    theme_bw() +
    xlab("") +
    theme_classic()+
    scale_y_continuous(breaks= scales::pretty_breaks(n=10))+
  ylab('# of items sold')+
  xlab('Us States')+
  theme(plot.background=element_rect(fill="#051e3e"),
        panel.background = element_rect(fill="#051e3e"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
   labs(title="US State that buys AAA batteries the most",
       subtitle=str_wrap("Similar Distibution for AAA",75),
       caption = "Data Recorded in 1st Jan 2019- 1 Jan 2020")
plt_3


library(cowplot)
suppressWarnings({
grid.arrange(plt_1,plt_2,plt_3, ncol=2, nrow=2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))
})


dt2<- sales %>%
  select(Product, PriceEach, Quantity)%>%
  group_by(Product, PriceEach) %>%
  summarize(Total_revenue= sum(as.numeric(Quantity))*as.numeric(PriceEach)) 
  
  
 dt2 <- dt2 %>% 
  distinct()

dt2 %>%
  arrange(desc(Total_revenue)) %>%
  kbl() %>%
  kable_material_dark()
  
  
 options(scipen = 999)

cols <- c("ThinkPad Laptop"= "grey", "Macbook Pro Laptop"= "red", "iPhone"="grey")
dt2 %>%
  arrange(PriceEach)%>%
  filter(str_detect(Product,'Macbook|iPhone|ThinkPad')) %>%
  ggplot(aes(Product, Total_revenue, color= PriceEach))+ geom_bar(stat='identity',
                                                                  show.legend = FALSE)+ coord_flip()+
  xlab('Total Revenue')+
  ylab('Product')+
  theme(
    panel.grid.major.y = element_blank(),
      axis.text.x = element_text() 
  )+
  scale_y_continuous(breaks= scales::pretty_breaks(n=6),
                     labels= label_number(suffix= " K", scale= 1e-6))+
  scale_color_brewer()+
   theme(plot.background=element_rect(fill="#051e3e"),
        panel.background = element_rect(fill="#051e3e"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
   labs(title="Top 3 Most Profitable Premium Products",
       subtitle=str_wrap("Products that are high priced and sell very well",75),
       caption = str_wrap("Total Revenue= Price of an article * Qty * No. of transactions",width = 105))
  
  
  dt3 <- sales %>%
  filter(str_detect('Macbook Pro Laptop', Product))%>%
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarize(sum = sum(Total_revenue= sum(as.numeric(Quantity))*as.numeric(PriceEach)))
  
  library(hrbrthemes)
qpt1<- qplot(data = dt3, x=month, y=sum)+
  geom_line(color="black")+
  geom_point(color="yellow")+
  xlab("Month")+
  ylab("Monthly Sales ($)")+
  hrbrthemes::theme_modern_rc()+
  theme(plot.background=element_rect(fill="#2ab7ca"),
        panel.background = element_rect(fill="#2ab7ca"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  scale_y_continuous(breaks= scales::pretty_breaks(n=6),
                     labels= label_number(suffix= " K", scale= 1e-6))+
   labs(title="Monthly Purchase Growth of Macbook Pro Laptop",
       subtitle=str_wrap("Consistent growth  rate pre-Covid-19 Era",75),
       caption = str_wrap("Covid-19 starts at around Nov 2019",width = 105))
qpt1


dt3_thinkpad <- sales %>%
  filter(str_detect('ThinkPad Laptop', Product))%>%
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarize(sum = sum(Total_revenue= sum(as.numeric(Quantity))*as.numeric(PriceEach)))


qpt2<- qplot(data = dt3_thinkpad, x=month, y=sum)+
  geom_line(color="black")+
  geom_point(color="yellow")+
  xlab("Months")+
  ylab("Monthly Sales($)")+
  hrbrthemes::theme_modern_rc()+
  theme(plot.background=element_rect(fill="#7bc043"),
        panel.background = element_rect(fill="#7bc043"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  scale_y_continuous(breaks= scales::pretty_breaks(n=6),
                     labels= label_number(suffix= " K", scale= 1e-6))+
   labs(title="Monthly Purchase Growth of Thinkpad Laptop",
       subtitle=str_wrap("Consistent growth  rate pre-Covid-19 Era",75),
       caption = str_wrap("Covid-19 starts at around Nov 2019",width = 105))
qpt2


dt3_iphone <- sales %>%
  filter(str_detect('iPhone', Product))%>%
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarize(sum = sum(Total_revenue= sum(as.numeric(Quantity))*as.numeric(PriceEach)))
  
  
  qpt3<- qplot(data = dt3_iphone, x=month, y=sum)+
  geom_line(color="red")+
  geom_point(color="yellow")+
  xlab("Months")+
  ylab("Monthly Sales($)")+
  hrbrthemes::theme_modern_rc()+
  theme(plot.background=element_rect(fill="#283655"),
        panel.background = element_rect(fill="#283655"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  scale_y_continuous(breaks= scales::pretty_breaks(n=6),
                     labels= label_number(suffix= " K", scale= 1e-6))+
   labs(title="Monthly Purchase Growth of iPhone",
       subtitle=str_wrap("Consistent growth  rate pre-Covid-19 Era",75),
       caption = str_wrap("Covid-19 starts at around Nov 2019",width = 105))
suppressWarnings({qpt3})



library(cowplot)
suppressWarnings({
grid.arrange(qpt1,qpt2, qpt3, ncol=2, nrow=2,
             layout_matrix = rbind(c(1,2), c(1,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))
})


dt_coast <- sales %>%
  mutate(region= case_when(
    as.numeric(ZIP)== 94016 ~ "West Coast",
    as.numeric(ZIP)== 90001 ~ "West Coast", 
    as.numeric(ZIP)== 10001 ~ "East Coast",
    as.numeric(ZIP)== 02215 ~ "East Coast",
    as.numeric(ZIP)== 75001 ~ "East Coast",
    as.numeric(ZIP)== 98101 ~"West Coast", 
    as.numeric(ZIP)== 97035 ~ "West Coast",
    as.numeric(ZIP)== 73301 ~ "East Coast",
    as.numeric(ZIP)== 04101 ~ "East Coast",
    as.numeric(ZIP)== 30301 ~ "East Coast",
    
  ))
  
  
  dt_coast_grp <- dt_coast %>%
  filter(str_detect('Macbook Pro Laptop|USB-C Charging Cable|Lightning Charging Cable|iPhone', Product)) %>%
  group_by(region,Product) %>%
  summarise(ArticlesBought= sum(as.numeric(Quantity)))

dt_coast_grp %>%
  kbl() %>%
  kable_material_dark()
  
  
  ggplot(dt_coast_grp,
       aes(str_wrap(Product,6), ArticlesBought, fill= region))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.background=element_rect(fill="#283655"),
        panel.background = element_rect(fill="#283655"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold", size= 8),
        axis.title.x = element_text(colour="white",face="bold",size=8, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=8, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  scale_y_continuous(breaks= scales::pretty_breaks(n=6),
                     )+
   labs(title="East Coast vs West Coast Product Sale",
       subtitle=str_wrap("Comparing Sales of Popular products in 2 US Regions",75),
       caption = str_wrap("States Like Texas which are geographically closer to East coast are included in East Coast since shipment from East Coast to them is more practical from logistic perspective (Proximity to Major Ports on that end)",width = 79))+
  ylab('No. of Products bought')+
  xlab('Products')
  
  
  
  library(treemapify)



Products <- c("Macbook Pro", "USB-C Charger", "Lightning Cable", "iPhone")

value <- c(40,30,15,15)

df <- data.frame(Products, value) 
ggplot(df, aes(area = value, fill = Products)) +
  geom_treemap()+
  theme(plot.background=element_rect(fill="#3b5998"),
        panel.background = element_rect(fill="#3b5998"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        axis.line.x.bottom = element_line(color="white"),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold", size= 8),
        axis.title.x = element_text(colour="white",face="bold",size=8, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=8, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
   labs(title="West Coast Container Optimization for Profit",
       subtitle=str_wrap("Shows size allotment for different products for shipment",75),
       caption = str_wrap("Calculation is based on standard 40-foot shipment container size aswell as Apple Packages for Laptops and USB cables",width = 79))+
  scale_color_manual(values= c('#4b3832','#854442','#fff4e6','#3c2f2f'))
