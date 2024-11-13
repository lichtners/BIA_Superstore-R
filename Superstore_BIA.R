#
summary(Superstore)

#
names(Superstore) <- tolower(names(Superstore))
names(Superstore) <- gsub(" ","_", names(Superstore),)
names(Superstore) <- sub("-","_", names(Superstore),)
names(Superstore)

#
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggeasy")
install.packages("ggrepel")
#
library(tidyverse)
library(lubridate)
library(ggeasy)
library(ggrepel)

#
data_sub <- Superstore %>% select(sub_category, sales, discount, profit)
head(data_sub)


#Discount Pie Chart
data_sub %>% 
  group_by(sub_category) %>% 
  summarise(Percent_discount = (sum(discount) / sum(data_sub$discount) * 100)) %>% 
  ggplot(aes(x = "", y = Percent_discount, fill = sub_category)) + 
  geom_bar(position = "fill", stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  labs(title = "Pie chart of the Discount") + 
  theme_void() + 
  easy_add_legend_title("Sub category")

#Sales of Category by Year
Superstore <- Superstore %>% mutate(order_year = year(order_date))
Superstore %>% group_by(order_year, category) %>% summarise(total = sum(sales)) %>% 
  ggplot(aes(x = order_year, y = total, color = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_light() +
  labs(y = 'Sales',x = 'Year', title = 'Sales of Category by year')

#Bubble Chart
Superstore %>% group_by(sub_category) %>% summarise(profit = sum(profit), sales = sum(sales)) %>% 
  ggplot(aes(x = profit, y = sales)) +
  geom_point(aes(color = sub_category, size = profit*sales)) +
  geom_text_repel(aes(label = sub_category)) +
  theme_light() +
  theme(legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(y = 'Sales',x = 'Profit', title = 'Bubble Chart of Sales and Profit')
