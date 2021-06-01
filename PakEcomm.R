library(tidyverse)
df=read.csv(choose.files())
pak=df

pak=pak[!duplicated(pak),]
pk=pak[-c(22:26)]

pk=select(pak,payment_method,Year,category_name_1,status,qty_ordered,status,count=) %>% 
  count(Year,category_name_1,qty_ordered)



e2=select(pak,payment_method,Year,category_name_1,status,qty_ordered) %>%
  count(Year,category_name_1,qty_ordered) %>% 
  mutate(category_name_1=recode(category_name_1, 
                                "Appliances"="Appliances","Beauty & Grooming"="Beauty & Grooming","Books"="Books","Computing"="Computing","Entertainment"="Entertainment","Health & Sports"="Health & Sports","Home & Living"="Home & Living","Kids & Baby"="Kids & Baby","Men's Fashion"="Men's Fashion","Mobiles & Tablets"="Mobiles & Tablets","Others"="Others","School & Education"="School & Education",	"Soghaat"="Soghaat","Superstore"="Superstore","Women's Fashion"="Women's Fashion",
                                "\\N"="Not Available",".default "= "Not Available"))%>% 
  mutate(Total = n*qty_ordered) 


e2x=select(e2,-c(3,4))
e2total=aggregate(e2x$total,by=list(year=e2x$Year,category=e2x$category_name_1),FUN=sum)
E2total=aggregate(e2x$Total, by=list(Year=e2x$Year,Category=e2x$category_name_1), FUN=sum)
glimpse(E2total)
E2total[3]=="Total"
E2total[3]="Total"
names(E2total)[names(E2total)=="total"]<-"Total"
plot(E2total)
glimpse(E2total)

pak1=group_by(E2total,Year)

pak2016=select(pak1$Year==2016)

p2018=which(pak1$Year==2018)

Pak2018=pak1[p2018,]

ggplot(data=pak1,aes(x =Year, y = Total,fill=Total))+geom_bar(stat = "identity",color = "lightpink3", fill="lightpink")+facet_wrap(~Category,ncol = 3)
