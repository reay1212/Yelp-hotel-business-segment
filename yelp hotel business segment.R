# ******************************************************************************* #
#                                                                                 #
# Yelp Data set 프로젝트                                                          #
# URL : https://www.kaggle.com/yelp-dataset/yelp-dataset/home                     #
#                                                                                 #
#                                                                                 #
#                                                                                 #
# ******************************************************************************* #
options(digits=10, scipen=100)


########## Packages & Library ##########
install.packages("Imap")
install.packages("ggmap", type="source")
install.packages("ggmap")
install.packages("readr")

library(readr) ; library(data.table) ; library(stringr)
library(doBy) ; library(dplyr) ; library(Imap)
library(ggmap) ; library(ggplot2) ; library(reshape2)
library(tm) ; library(SnowballC) ; library(wordcloud)
library(RColorBrewer)

########## Data Load ##########
setwd("E:\\1. R\\Study\\Yelp Project\\data")


##### Business Data Load #####
yp_b <- fread("yelp_business.csv")
yp_b_h <- as.data.table(read_csv("yelp_business_hours.csv"))
yp_b_c <- fread("yelp_checkin.csv")
yp_b_a <- fread("yelp_business_attributes.csv", stringsAsFactors = TRUE)
##### User Data Load #####
yp_u <- fread("yelp_user.csv")

yp_r <- read_csv("yelp_review.csv")
yp_r <- as.data.table(yp_r)
yp_r <- yp_r[,text:=NULL]

yp_t <- fread("yelp_tip.csv")

########## Data Pre-processing ##########
##### Business Data Pre-Processing #####
#### yelp_business.csv ####
length(unique(yp_b$business_id)) # 174,567 사업체

# 'Hotels' 업체들만 분석 대상으로 선정
catego <- data.frame(table(unlist(str_split(yp_b$categories, ";"))))
catego <- catego[grep("Hotels",catego$Var1),] # Hotels & Travel : 5,736 | Hotels : 2,683
yp_b_H <- yp_b[grep("Hotels", yp_b$categories),]

rm(catego)

#### yelp_business_hours.csv ####
yp_b_h_H <- yp_b_h[business_id %in% yp_b_H$business_id,]

yp_b_h_H[,.(.N),by=c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")][order(-N)]
yp_b_h_H_b<-data.frame(apply(yp_b_h_H[,2:length(yp_b_h_H)],2,function(x) str_count(x,"-")))
names(yp_b_h_H_b)<-paste(names(yp_b_h_H_b),1:length(yp_b_h_H_b),sep="_")
yp_b_h_H<-cbind(yp_b_h_H,yp_b_h_H_b)
yp_b_h_H$open_days<-rowSums(yp_b_h_H[,9:15])

yp_b_h_H <- yp_b_h_H[,c(1:8,16)]

rm(yp_b_h_H_b)

#### yelp_checkin.csv ####
yp_b_c_H <- yp_b_c[business_id %in% yp_b_H$business_id,]
yp_b_c_H <- dcast(yp_b_c_H, business_id ~ weekday, value.var="checkins", sum)
yp_b_c_H$checkins <- rowSums(yp_b_c_H[,2:8])




#### Business Data Master ####
yp_b_H <- as.data.frame(yp_b_H)
yp_b_h_H <- as.data.frame(yp_b_h_H)
yp_b_c_H <- as.data.frame(yp_b_c_H)

Biz_Hotel <- merge(yp_b_H, yp_b_h_H, all.x=TRUE)
Biz_Hotel <- merge(Biz_Hotel, yp_b_c_H, all.x=TRUE)

rm(yp_b, yp_b_H, yp_b_h_H, yp_b_c, yp_b_c_H)

colSums(is.na(Biz_Hotel))
Biz_Hotel[is.na(Biz_Hotel)] <- 0

##### User Data Pre-Processing #####
#### yelp_review.csv ####
names(yp_r)
yp_r_H <- yp_r[business_id %in% Biz_Hotel$business_id,]

# length(unique(yp_r_H$user_id)) 
# 183,005 User

# useful/funny/cool 변수들은 업체에 대한 긍정적인 값을 나타내므로 
# Sum 함으로써 likes 변수 생성 
yp_r_H$likes <- rowSums(yp_r_H[,c("useful","funny","cool")])


# 처음과 마지막 댓글달린 날짜 및 user들의 평균평점, 좋아요 합계, 댓글 수 Data-set 생성
first_review_date <- as.data.frame(yp_r_H[,.(first_review=min(date)),by=business_id])
first_review_date$first_year <- substr(first_review_date$first_review,1,4)
last_review_date <- as.data.frame(yp_r_H[,.(last_review=max(date)),by=business_id])
last_review_date$last_year <- substr(last_review_date$last_review,1,4)
Biz_Hotel_2 <- as.data.frame(yp_r_H[,.(stars_mean=mean(stars),likes_sum=sum(likes),review_cnt=.N),by=business_id])

Biz_Hotel_2 <- merge(Biz_Hotel_2, first_review_date, by="business_id", all=TRUE)
Biz_Hotel_2 <- merge(Biz_Hotel_2, last_review_date, by="business_id", all=TRUE)
rm(first_review_date, last_review_date)

#### yelp_user.csv ####
names(yp_u)
yp_u_H <- yp_u[user_id %in% yp_r_H$user_id]

# 변수 유형 변경(숫자형, 날짜형)
yp_u_H$review_count<-as.numeric(yp_u_H$review_count)
yp_u_H$average_stars<-as.numeric(yp_u_H$average_stars)
yp_u_H$useful<-as.numeric(yp_u_H$useful)
yp_u_H$funny<-as.numeric(yp_u_H$funny)
yp_u_H$cool<-as.numeric(yp_u_H$cool)
yp_u_H$fans<-as.numeric(yp_u_H$fans)
yp_u_H$yelping_since<-as.Date(yp_u_H$yelping_since)

# Elite 구분 variable 및 친구수 variable 생성
yp_u_H <- transform(yp_u_H, elite_user = ifelse(elite == "None", "General","Elite"),
                            friends_cnt = str_count(friends,",") + 1)

# 분석에 사용할 변수만 선정
yp_u_H <- yp_u_H[,c("user_id","name","review_count","yelping_since","friends","useful","funny","cool","fans","elite","average_stars","elite_user","friends_cnt")]


#### yelp_tip.csv ####
names(yp_t)
yp_t_H <- yp_t[business_id %in% Biz_Hotel$business_id,]


Biz_Hotel_3 <- merge(summaryBy(likes ~ business_id, data=yp_t_H, FUN=c(sum)),summaryBy(text ~ business_id, data=yp_t_H, FUN=c(length)), by="business_id")
names(Biz_Hotel_3) <- c("business_id","Tip_likes_sum","Tip_cnt")



#### Business Data Master ####
names(Biz_Hotel)
names(Biz_Hotel_2)
names(Biz_Hotel_3)

Biz_Hotel_Master <- merge(Biz_Hotel, Biz_Hotel_2, by="business_id", all=TRUE)
Biz_Hotel_Master <- merge(Biz_Hotel_Master, Biz_Hotel_3, by="business_id", all=TRUE)

# 최근성 반영 : 마지막 댓글 기간이 2015년 이전 업체는 분석 대상에서 제외
Biz_Hotel_Master <- Biz_Hotel_Master[Biz_Hotel_Master$last_year %in% c(2017,2016,2015),]

colSums(is.na(Biz_Hotel_Master))
Biz_Hotel_Master[is.na(Biz_Hotel_Master)] <- 0

# B_Hotel_MM <- merge(B_Hotel_MM, Tip_likes[,c("business_id","Tip.cnt")], by="business_id", all.x=TRUE)
# B_Hotel_MM[is.na(B_Hotel_MM)] <- 0
# 
# B_Hotel_MM <- merge(B_Hotel_MM, yp_b_a_H[,c("business_id","RestaurantsReservations")], all.x=TRUE)
# B_Hotel_MM[is.na(B_Hotel_MM$RestaurantsReservations)] <- "No information"
# colSums(is.na(B_Hotel_MM))
# table(B_Hotel_MM$RestaurantsReservations)


########## Data EDA ##########
##### EDA #####
names(Biz_Hotel_Master)
length(unique(Biz_Hotel_Master$business_id))

#### Hotel Location 지도 ####

table(Biz_Hotel_Master$state)
America <- get_googlemap(center=as.numeric(geocode("United States")), scale=2, zoom = 4)
Hotel_Map <- ggmap(America, extent="normal")
Hotel_Map <- Hotel_Map + geom_point(aes(x=longitude, y=latitude),data=Biz_Hotel_Master[Biz_Hotel_Master$longitude < -60 & Biz_Hotel_Master$latitude > 10,], col="red") + theme_bw() + labs(title="Hotel Location", x=NULL, y=NULL)

tmp <- Biz_Hotel_Master[Biz_Hotel_Master$longitude < -60 & Biz_Hotel_Master$latitude > 10,] %>%
  group_by(state) %>% summarise(Hotel_count=n(), lon=mean(longitude), lat=mean(latitude))

Hotel_Map + geom_point(aes(x=lon, y=lat, size=Hotel_count), data=tmp, col="Orange", alpha=0.4) +
  scale_size_area(max_size = 30, guide=FALSE)
 
  
tmp[tmp$state %in% c("AZ","NV","ON","OH"),] %>% 
  mutate(state=factor(state, levels=c("OH","ON","NV","AZ"))) %>%
  ggplot(aes(x=state, y=Hotel_count)) + geom_bar(stat="identity", fill="Orange") + coord_flip() +
  geom_text(aes(x=state, y=50, label=Hotel_count)) + theme_bw() +
  labs(title="Hotel 사업체가 가장 많은 지역", y=NULL) +
  theme(plot.title=element_text(face="bold",size=20))





#### 폐업한 업체 비중이 8% 밖에 안되므로, 전체데이터 가지고 분석 진행 ####
prop.table(table(Biz_Hotel$is_open))

#### Hotel 사업체별 별점 ####
hist(Biz_Hotel_Master$stars_mean, breaks=10, xlab="평점(1점 ~ 5점)", 
     ylab="Hotel Business count",main="Hotels 사업체들의 평점 Histogram", col="Gold", border="white")


#### review / checkin / Tip EDA ####
summary(Biz_Hotel_Master$review_count)
hist(Biz_Hotel_Master[Biz_Hotel_Master$review_count <= 30,]$review_count, breaks=30, freq=FALSE,
     xlab="Hotel Business Review Count", main="Hotels 사업체들의 리뷰 개수 Histogram",
     col="Orange", border="white")
lines(density(Biz_Hotel_Master[Biz_Hotel_Master$review_count <= 30,]$review_count),col="Dim Gray" ,lwd=2)

summary(Biz_Hotel_Master$checkins) # Median : 10
hist(Biz_Hotel_Master[Biz_Hotel_Master$checkins <= 30,]$checkins, breaks=30, freq=FALSE,
     xlab="Hotel Business Checkin Coount", main="Hotels 사업체들의 Checkin Histogram",
     col="Light Salmon", border="white")
lines(density(Biz_Hotel_Master[Biz_Hotel_Master$checkins <= 30,]$checkins),col="Dim Gray" ,lwd=2)

summary(Biz_Hotel_Master$Tip_cnt)
hist(Biz_Hotel_Master[Biz_Hotel_Master$Tip_cnt <= 10,]$Tip_cnt, breaks=30)
tmp <- data.frame(table(Biz_Hotel_Master$Tip_cnt))[1:10,]
tmp$Var1 <- factor(tmp$Var1, levels=c(9,8,7,6,5,4,3,2,1,0))
ggplot(tmp, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="Peach Puff") + coord_flip() +
  labs(title="Tip 개수별 Hotels 사업체 분포", x="Tip Count", y="Hotels Count") + theme_bw() +
  geom_text(aes(x=Var1, y=50,label=Freq)) +
  theme(plot.title=element_text(face="bold",size=15))


#### 연도별 사업체들의 댓글 분포  ####
 data.frame(table(substr(yp_r_H$date, 1,4))) %>%
   ggplot(aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="Gold") +
   labs(title="Hotel 사업체들의 연도별 리뷰 개수", x="연도",y="Review Count") +
   coord_flip() + geom_text(aes(x=Var1, y=1500, label=Freq)) + theme_bw() +
   theme(plot.title=element_text(face="bold",size=15))

tmp <- yp_r_H[,c("review_id","date")]
tmp <- transform(tmp, yy = substr(date, 1,4),
                      mm = substr(date, 6,7))
data.frame(table(tmp$yy, tmp$mm)) %>% filter(Var1 %in% c(2017,2016,2015)) %>%
  ggplot(aes(x=Var2, y=Freq, group=Var1)) + geom_line(col="Orange Red", lwd=1) + facet_grid(Var1~.) +
  geom_point(size=2, col="red") + theme_bw() +
  labs(title="Hotel 사업체들의 월별 리뷰 개수",x="Months", y="Review Count") +
  theme(plot.title=element_text(face="bold", size=15))
 






barplot(colSums(Biz_Hotel[,c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")]))








########## Hotel Business Segment ##########
##### Segment : Stars ##### 
# Stars(평점)을 기준으로 세분화
# Low / Middle / High level로 지정, 분포가 적절한 비중으로 퍼져있도록 평점 범위 조정
cor(Biz_Hotel_Master[,c("stars","stars_mean")])
View(Biz_Hotel_Master[,c("business_id","stars","stars_mean")])


Biz_Hotel_Master <- transform(Biz_Hotel_Master,stars_seg=ifelse(stars_mean <= 2.5,"Low",
                                                         ifelse(stars_mean <= 4.5,"Middle","High")))
Biz_Hotel_Master$stars_seg <- factor(Biz_Hotel_Master$stars_seg, levels=c("Low","Middle","High"))

data.frame(prop.table(table(Biz_Hotel_Master$stars_seg))) %>%
  ggplot(aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="orange red",col="white") +
  labs(x=NULL, y=NULL, title="Business Segmentation based on Stars") + theme_bw() +
  theme(plot.title=element_text(face="bold", size=20))
  
  

##### Segment : Period of reviews written #####
# 처음 댓글 달린 시점과, 마지막 댓글 달린 시점 기준으로 세분화
# period_low / period_middle / period_high / period_veryhigh level
# 분포가 적절한 비중으로 퍼져있도록 연도 범위 조정

Biz_Hotel_Master <- transform(Biz_Hotel_Master,period_review=ifelse(first_year %in% c(2017,2016,2015), "period_low",ifelse(first_year %in% c(2014,2013,2012), "period_middle", ifelse(first_year %in% c(2011,2010,2009), "period_high","period_very_high"))))
Biz_Hotel_Master$period_review <- factor(Biz_Hotel_Master$period_review, levels=c("period_low","period_middle","period_high","period_very_high")) 

data.frame(prop.table(table(Biz_Hotel_Master$period_review))) %>%
  ggplot(aes(x=Var1,y=Freq)) + geom_bar(stat="identity",fill="orange red", col="white") +
  labs(x=NULL, y=NULL, title="Business Segmentation based on review period") + theme_bw() +
  theme(plot.title=element_text(face="bold",size=20))

##### Segment : Review count #####
# 댓글 달린 갯수를 기준으로 세분화
# Low / Middle / High level로, 분포가 적절한 비중으로 퍼져있도록 댓글 수 범위 조정
cor(Biz_Hotel_Master[,c("review_count","review_cnt")])
summary(Biz_Hotel_Master$review_cnt)

Biz_Hotel_Master <- transform(Biz_Hotel_Master, review_seg=ifelse(review_cnt <= 10,"Low",ifelse(review_cnt <= 30,"Middle","High")))
Biz_Hotel_Master$review_seg <- factor(Biz_Hotel_Master$review_seg, levels=c("Low","Middle","High"))

prop.table(table(Biz_Hotel_Master$review_seg))

##### Segment : Recognition #####
# Review count variable 와 Tip count variable 가지고 recognition variable 생성
# Five level로, 분포가 적절한 비중으로 퍼져있도록 범위 조정
cor(Biz_Hotel_Master[,c("review_cnt","Tip_cnt")])
plot(Biz_Hotel_Master[,c("review_cnt","Tip_cnt")], cex=.8)

q1<-quantile(Biz_Hotel_Master$review_cnt,probs=c(0.5,0.9))
q2<-quantile(Biz_Hotel_Master$Tip_cnt,probs=c(0.5,0.9))

Biz_Hotel_Master$review_group <- ifelse(Biz_Hotel_Master$review_cnt<=q1[1],1, ifelse(Biz_Hotel_Master$review_cnt<=q1[2],2,3))
# table(Biz_Hotel_Master$review_group)

Biz_Hotel_Master$tip_group<-ifelse(Biz_Hotel_Master$Tip_cnt<=q2[1],1,ifelse(Biz_Hotel_Master$Tip_cnt<=q2[2],2,3))
# table(Biz_Hotel_Master$tip_group)


Biz_Hotel_Master$recog_score<-100*(Biz_Hotel_Master$review_group + Biz_Hotel_Master$tip_group)/6
Biz_Hotel_Master$recog_score<-ifelse(Biz_Hotel_Master$recog_score<34,1,
                                     ifelse(Biz_Hotel_Master$recog_score<51,2,
                                            ifelse(Biz_Hotel_Master$recog_score<67,3,
                                                   ifelse(Biz_Hotel_Master$recog_score<84,4,5))))
data.frame(prop.table(table(Biz_Hotel_Master$recog_score))) %>%
  ggplot(aes(x=Var1,y=Freq)) + geom_bar(stat="identity",fill="orange red", col="white") +
  labs(x=NULL, y=NULL, title="Business Segmentation based on recognization") + theme_bw() +
  theme(plot.title=element_text(face="bold",size=20))
# rm(q1,q2)






##### Final Hotel Business Segment #####
Segment_model <- summaryBy(likes_sum + checkins ~ period_review + stars_seg + recog_score, data=Biz_Hotel_Master, FUN=c(length, mean))

names(Segment_model)
Segment_model <- Segment_model[,c(1,2,3,4,6,7)]
names(Segment_model) <- c("period_review","stars_seg","recog_score","Business_cnt","likes","checkins")

########## Hotel Business Profiling ##########
##### yelp_business_attributes.csv #####

yp_b_a_H <- yp_b_a[business_id %in% Biz_Hotel_Master$business_id,] # 3,258
names(yp_b_a_H)

# 업체 Attributes를 나타내는 Data-set에서 
# 결측값의 비중이 낮은 변수 선정
# Tmp <- data.frame(summary(yp_b_a_H[,2:length(yp_b_a_H)]))
# write.csv(Tmp, "C:/Users/credif/Desktop/Tmp.csv", row.names=FALSE)

# BusinessParking_garage / BikeParking / RestaurantsReservations / BusinessAcceptsBitcoin
yp_b_a_H <- yp_b_a_H[,c("business_id","BusinessParking_garage","BikeParking","RestaurantsReservations","BusinessAcceptsBitcoin")]
table(yp_b_a_H$BusinessParking_garage)
table(yp_b_a_H$BikeParking)
table(yp_b_a_H$RestaurantsReservations)
table(yp_b_a_H$BusinessAcceptsBitcoin)


yp_b_a_H$BusinessParking_garage <- replace(yp_b_a_H$BusinessParking_garage, yp_b_a_H$BusinessParking_garage == "Na",NA) 
yp_b_a_H$BikeParking <- replace(yp_b_a_H$BikeParking, yp_b_a_H$BikeParking == "Na",NA)
yp_b_a_H$RestaurantsReservations <- replace(yp_b_a_H$RestaurantsReservations, yp_b_a_H$RestaurantsReservations == "Na",NA)
yp_b_a_H$BusinessAcceptsBitcoin <- replace(yp_b_a_H$BusinessAcceptsBitcoin, yp_b_a_H$BusinessAcceptsBitcoin == "Na",NA)

yp_b_a_H$BusinessParking_garage <- factor(yp_b_a_H$BusinessParking_garage)
yp_b_a_H$BikeParking <- factor(yp_b_a_H$BikeParking)
yp_b_a_H$RestaurantsReservations <- factor(yp_b_a_H$RestaurantsReservations)
yp_b_a_H$BusinessAcceptsBitcoin <- factor(yp_b_a_H$BusinessAcceptsBitcoin)


Biz_Hotel_Master <- merge(Biz_Hotel_Master, yp_b_a_H, by="business_id", all.x=TRUE)





##### Sub categories #####
# Hotels외 다른 카테고리에 따른 profiling
# sub_categories <- data.frame(str_split(Biz_Hotel_Master$categories, ";", simplify = TRUE))
sub_categories <- data.frame(table(unlist(str_split(Biz_Hotel_Master$categories, ";"))))
write.csv(catego, "C:/Users/credif/Desktop/catego.csv", row.names=FALSE)

tmp <- sub_categories[! sub_categories$Var1 %in% c("Hotels & Travel","Hotels"),] %>%
  arrange(Freq) %>% tail(15)
tmp$Var1 <- factor(tmp$Var1, levels=tmp$Var1)
ggplot(tmp, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="orange") + coord_flip() + theme_bw() + 
  geom_text(aes(x=Var1 ,y=100, label=Freq)) +
  labs(title="Hotel 사업체들의 Sub Category", x=NULL, y=NULL) +
  theme(plot.title = element_text(face="bold", size=15))



#### Casinos : Casinos 카테고리를 가지고 있는 업체 ####
Casinos <- Biz_Hotel_Master[grep("Casinos",Biz_Hotel_Master$categories),]

#### Meal    : Restaurants / Bed & Breakfast / Bars / Food / Breakfast & Brunch 카테고리를 가지고 있는 업체 ####
Restaurants <- Biz_Hotel_Master[grep("Restaurants",Biz_Hotel_Master$categories),]
Bed_Breakfast <- Biz_Hotel_Master[grep("Bed & Breakfast",Biz_Hotel_Master$categories),]
Bars <- Biz_Hotel_Master[grep("Bars",Biz_Hotel_Master$categories),]
Food <- Biz_Hotel_Master[grep("Food",Biz_Hotel_Master$categories),]
Breakfast_Brunch <- Biz_Hotel_Master[grep("Breakfast & Brunch",Biz_Hotel_Master$categories),]
Meal <- rbind(Restaurants,Bed_Breakfast,Bars,Food,Breakfast_Brunch)
Meal <- unique(Meal)

rm(Restaurants, Bed_Breakfast, Bars, Food, Breakfast_Brunch)

#### Event   : Event Planning & Services / Venues & Event Spaces / Party & Event Planning 카테고리를 가지고 있는 업체 ####
Event_1 <- Biz_Hotel_Master[grep("Event Planning & Services",Biz_Hotel_Master$categories),]
Event_2 <- Biz_Hotel_Master[grep("Venues & Event Spaces",Biz_Hotel_Master$categories),]
Event_3 <- Biz_Hotel_Master[grep("Party & Event Planning",Biz_Hotel_Master$categories),]

Event <- rbind(Event_1,Event_2,Event_3)
Event <- unique(Event)
rm(Event_1, Event_2, Event_3)

#### useful   : Limos / Airport Shuttles 카테고리를 가지고 있는 업체 ####
Limos <- Biz_Hotel_Master[grep("Limos",Biz_Hotel_Master$categories),]
Airport <- Biz_Hotel_Master[grep("Airport Shuttles",Biz_Hotel_Master$categories),]

useful <- rbind(Limos,Airport)
useful <- unique(useful)

rm(Limos, Airport)

#### Sub category Merge ####
Biz_Hotel_Master <- transform(Biz_Hotel_Master, Casinos=ifelse(business_id %in% Casinos$business_id,1,0),
                                                Meal=ifelse(business_id %in% Meal$business_id,1,0),
                                                Event=ifelse(business_id %in% Event$business_id,1,0),
                                                useful=ifelse(business_id %in% useful$business_id,1,0))

table(Biz_Hotel_Master$Casinos)
table(Biz_Hotel_Master$Meal)
table(Biz_Hotel_Master$Event)
table(Biz_Hotel_Master$useful)

rm(Casinos, Meal, Event, useful)


###### Profiling Model Merge #####
names(Segment_model)
names(Biz_Hotel_Master)

model_profile1 <- group_by(Biz_Hotel_Master, period_review,stars_seg,recog_score) %>% 
  summarise(Casino=sum(Casinos), Meal=sum(Meal), Event=sum(Event), useful=sum(useful))


model_profile2 <- dcast(Biz_Hotel_Master[Biz_Hotel_Master$state %in% c("AZ","NV","ON","OH"),],
                        period_review + stars_seg + recog_score ~ state, value.var="business_id",length)


Segment_model <- merge(Segment_model, model_profile1, by=c("period_review","stars_seg","recog_score"), all=TRUE)
Segment_model <- merge(Segment_model, model_profile2, by=c("period_review","stars_seg","recog_score"), all=TRUE)


write.csv(Segment_model, "E:/R/Study/Yelp_Project/Report/Hotel Business Segmentation.csv", row.names=FALSE)



##### (period_low & period_very_high) (Low & Middle) (1,5) Nevada #####
tmp <- filter(Biz_Hotel_Master, 
              period_review %in% c("period_low","period_very_high") &
              stars_seg %in% c("Low","Middle") & recog_score %in% c(1,5) & state == "NV")
tmp <- transform(tmp, fit_cluster = ifelse(period_review == "period_low" & stars_seg == "Low" & recog_score == 1,"운영기간이 짧고 평점 및 인지도가 낮은 Hotel","운영기간이 길고 평점 및 인지도가 높은 Hotel"))
summary(tmp$longitude)
summary(tmp$latitude)
Nevada <- get_googlemap(center=c(lon=mean(tmp$longitude), lat=mean(tmp$latitude)), scale=2, zoom = 11)
Nevada_Map <- ggmap(Nevada, extent="normal")
Nevada_Map + geom_point(data=tmp[tmp$longitude > -115.425 & tmp$longitude < -115 & tmp$latitude<36.3,], aes(x=longitude,y=latitude,col=as.factor(fit_cluster))) +
  theme_bw() + theme(legend.position = "bottom") + 
  labs(x=NULL, y=NULL, title="Hotel's location in Nevada", col=NULL) +
  theme(plot.title=element_text(face="bold", size=20))

##### (period_low & period_very_high) (Low & Middle) (1,5) Arizona #####
tmp <- filter(Biz_Hotel_Master, 
              period_review %in% c("period_low","period_very_high") &
                stars_seg %in% c("Low","Middle") & recog_score %in% c(1,5) & state == "AZ")
tmp <- transform(tmp, fit_cluster = ifelse(period_review == "period_low" & stars_seg == "Low" & recog_score == 1,"운영기간이 짧고 평점 및 인지도가 낮은 Hotel","운영기간이 길고 평점 및 인지도가 높은 Hotel"))
summary(tmp$longitude)
summary(tmp$latitude)
Arizona <- get_googlemap(center=c(lon=mean(tmp$longitude), lat=mean(tmp$latitude)), scale=2, zoom = 11)
Arizona_Map <- ggmap(Arizona, extent="normal")
Arizona_Map + geom_point(data=tmp[tmp$longitude > -112.2 & tmp$longitude < -111.8 & tmp$latitude<33.65 & tmp$latitude > 33.3,], aes(x=longitude,y=latitude,col=as.factor(fit_cluster))) +
  theme_bw() + theme(legend.position = "bottom") + 
  labs(x=NULL, y=NULL, title="Hotel's location in Arizona", col=NULL) +
  theme(plot.title=element_text(face="bold", size=20))

##### (period_low & period_very_high) (Low & Middle) (1,5) Nevads & Arizona #####
tmp <- filter(Biz_Hotel_Master, 
              period_review %in% c("period_low","period_very_high") &
                stars_seg %in% c("Low","Middle") & recog_score %in% c(1,5) & state %in% c("AZ","NV"))
tmp <- transform(tmp, fit_cluster = ifelse(period_review == "period_low" & stars_seg == "Low" & recog_score == 1,"운영기간이 짧고 평점 및 인지도가 낮은 Hotel","운영기간이 길고 평점 및 인지도가 높은 Hotel"))

########## WordCloud ##########

wordcloud_review <- function(x){
  rst <- VCorpus(VectorSource(x))
  rst <- tm_map(rst, content_transformer(tolower))
  rst <- tm_map(rst, removeNumbers)
  rst <- tm_map(rst, removeWords, stopwords())
  rst <- tm_map(rst, removePunctuation)
  rst <- tm_map(rst, stemDocument)
  rst <- tm_map(rst, stripWhitespace)
  return(rst)
}

yp_t_H_final <- yp_t_H[business_id %in% tmp$business_id,c("text","business_id","likes","date")]
yp_t_H_final <- left_join(yp_t_H_final, tmp[,c("business_id","fit_cluster","state")],
                          by="business_id")

AZ_low_Hotel <- yp_t_H_final[yp_t_H_final$state == "AZ" & yp_t_H_final$fit_cluster == "운영기간이 짧고 평점 및 인지도가 낮은 Hotel",]
NV_low_Hotel <- yp_t_H_final[yp_t_H_final$state == "NV" & yp_t_H_final$fit_cluster == "운영기간이 짧고 평점 및 인지도가 낮은 Hotel",]

AZ_high_Hotel <- yp_t_H_final[yp_t_H_final$state == "AZ" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel",]
NV_high_Hotel <- yp_t_H_final[yp_t_H_final$state == "NV" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel",]

##### AZ : 운영기간이 짧고 평점 및 인지도가 낮은  Hotel WordCloud #####
AZ_low_corpus <- wordcloud_review(AZ_low_Hotel$text)
wordcloud(AZ_low_corpus, min.freq=100, random.order=FALSE, colors=brewer.pal(8,"Accent"))


##### AZ : 운영기간이 길고 평점 및 인지도가 높은  Hotel WordCloud #####
AZ_high_corpus <- VCorpus(VectorSource(AZ_high_Hotel$text))
AZ_high_corpus <- tm_map(AZ_high_corpus, content_transformer(tolower))
AZ_high_corpus <- tm_map(AZ_high_corpus, removeNumbers)
AZ_high_corpus <- tm_map(AZ_high_corpus, removeWords, stopwords())
AZ_high_corpus <- tm_map(AZ_high_corpus, removePunctuation)
AZ_high_corpus <- tm_map(AZ_high_corpus, stemDocument)
AZ_high_corpus <- tm_map(AZ_high_corpus, stripWhitespace)

wordcloud(AZ_high_corpus, min.freq=100, random.order=FALSE, colors=brewer.pal(8,"Dark2"))


##### NV : 운영기간이 짧고 평점 및 인지도가 낮은  Hotel WordCloud #####
NV_low_corpus <- wordcloud_review(NV_low_Hotel$text)
wordcloud(NV_low_corpus, min.freq=100, random.order=FALSE, colors=brewer.pal(8,"Accent"))


##### NV : 운영기간이 길고 평점 및 인지도가 높은  Hotel WordCloud #####
NV_high_corpus <- VCorpus(VectorSource(NV_high_Hotel$text))
NV_high_corpus <- tm_map(NV_high_corpus, content_transformer(tolower))
NV_high_corpus <- tm_map(NV_high_corpus, removeNumbers)
NV_high_corpus <- tm_map(NV_high_corpus, removeWords, stopwords())
NV_high_corpus <- tm_map(NV_high_corpus, removePunctuation)
NV_high_corpus <- tm_map(NV_high_corpus, stemDocument)
NV_high_corpus <- tm_map(NV_high_corpus, stripWhitespace)
wordcloud(NV_high_corpus, min.freq=100, random.order=FALSE, colors=brewer.pal(8,"Dark2"))







#######################################################################
yp_t_H_final$text_clean <- removeNumbers(yp_t_H_final$text)
yp_t_H_final$text_clean <- removeWords(yp_t_H_final$text_clean, stopwords())
yp_t_H_final$text_clean <- removePunctuation(yp_t_H_final$text_clean)
yp_t_H_final$text_clean <- tolower(yp_t_H_final$text_clean)
# 이상치 제거 
yp_t_H_final <- yp_t_H_final[-30885,]
yp_t_H_final$text_clean <- stemDocument(yp_t_H_final$text_clean)
yp_t_H_final$text_clean <- stripWhitespace(yp_t_H_final$text_clean)


write.csv(NV_low_Hotel, "E:/1. R/Study/Yelp Project/Report/NV_low_Hotel.csv", row.names=FALSE)
write.csv(AZ_low_Hotel, "E:/1. R/Study/Yelp Project/Report/AZ_low_Hotel.csv", row.names=FALSE)


# AZ high hotel : great, home, airport
# NV high hotel : room, vega, hotel

AZ_high_tip_1 <- yp_t_H_final[yp_t_H_final$state == "AZ" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("great"),]
AZ_high_tip_2 <- yp_t_H_final[yp_t_H_final$state == "AZ" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("home"),]
AZ_high_tip_3 <- yp_t_H_final[yp_t_H_final$state == "AZ" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("airport"),]
AZ_high_tip <- rbind(AZ_high_tip_1,AZ_high_tip_2,AZ_high_tip_3)
rm(AZ_high_tip_1,AZ_high_tip_2,AZ_high_tip_3)
AZ_high_tip <- AZ_high_tip %>%
  mutate(str_cnt = str_count(text, " "), Year=substr(date,1,4)) %>%
  filter(str_cnt > 20 & Year %in% c("2014","2015","2016","2017"))



NV_high_tip_1 <- yp_t_H_final[yp_t_H_final$state == "NV" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("room"),]
NV_high_tip_2 <- yp_t_H_final[yp_t_H_final$state == "NV" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("vega"),]
NV_high_tip_3 <- yp_t_H_final[yp_t_H_final$state == "NV" & yp_t_H_final$fit_cluster == "운영기간이 길고 평점 및 인지도가 높은 Hotel" & yp_t_H_final$text_clean %like% c("hotel"),]
NV_high_tip <- rbind(NV_high_tip_1,NV_high_tip_2,NV_high_tip_3)
rm(NV_high_tip_1,NV_high_tip_2,NV_high_tip_3)
NV_high_tip <- NV_high_tip %>%
  mutate(str_cnt = str_count(text, " "), Year=substr(date,1,4)) %>%
  filter(str_cnt > 20 & Year %in% c("2017"))

write.csv(NV_high_tip, "E:/1. R/Study/Yelp Project/Report/NV_high_tip.csv", row.names=FALSE)
write.csv(AZ_high_tip, "F:/1. R/Study/Yelp Project/Report/AZ_high_tip.csv", row.names=FALSE)



