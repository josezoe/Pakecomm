library(tidyverse)
df=read.csv(choose.files())
pak=df


#Let's check if any duplicates in the data 
pak=pak[!duplicated(pak),]  # 584525 obs 26 variable 

# Lets remove column 22to 26
pk=pak[-c(22:26)] #  now only 584525 and 21 variable


# let select column to get data for category wise by year

e2=select(pk,payment_method,Year,category_name_1,status,qty_ordered) %>%
  count(Year,category_name_1,qty_ordered) %>% 
  mutate(category_name_1=recode(category_name_1, 
   "Appliances"="Appliances","Beauty & Grooming"="Beauty & Grooming","Books"="Books",
   "Computing"="Computing","Entertainment"="Entertainment","Health & Sports"="Health & Sports",
   "Home & Living"="Home & Living","Kids & Baby"="Kids & Baby","Men's Fashion"="Men's Fashion",
   "Mobiles & Tablets"="Mobiles & Tablets","Others"="Others",
   "School & Education"="School & Education",	"Soghaat"="Soghaat",
   "Superstore"="Superstore","Women's Fashion"="Women's Fashion",
    "\\N"="Not Available",".default "= "Not Available"))%>% 
  mutate(Total = n*qty_ordered) 



# Lets remove the column 3 and 4
e2x=select(e2,-c(3,4))

# Let aggregate category wise and Year wise


E2total=aggregate(e2x$Total, by=list(Year=e2x$Year,Category=e2x$category_name_1), FUN=sum)


glimpse(E2total)

# Lets rename the column name
names(E2total)[names(E2total)=="x"]<-"Total"

glimpse(E2total)

pak1=E2total

# Lets plot with data 

ggplot(data=pak1,aes(x =Year, y = Total,fill=Total))+geom_bar(stat = "identity",color = "lightpink3", fill="lightpink")+facet_wrap(~Category,ncol = 3)



