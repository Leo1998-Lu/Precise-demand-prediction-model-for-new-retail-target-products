options(scipen=200)
sale_info<-read.csv("附件1：sale_info.csv",header = T,sep = ",",encoding = 'UTF-8')
head(sale_info)
names(sale_info)[1]<-"skc"
str(sale_info)

######问题1######
##选定对应时间轴 2018-07-01 —— 2018-10-01
sale_info$date_num<-as.numeric(as.Date(sale_info$date_rcd))
start_Q1<-as.numeric(as.Date("2018-07-01"));start_Q1
end_Q1<-as.numeric(as.Date("2018-10-01"));end_Q1

##各skc累计销售与累计花费
library(dplyr)
skc_sale<-sale_info%>%
  group_by(skc)%>%
  filter(date_num>=start_Q1 & date_num<=end_Q1)%>%
  summarise(total_sale=sum(s),total_cost=sum(real_cost))%>%
  top_n(50,total_sale)
skc_sale#target skc for Q1
skc_cost<-arrange(skc_sale,skc_sale$total_cost)
skc_cost$rank<-factor(nrow(skc_cost):1)

##销售额top10 skc
library(ggplot2)
ggplot(skc_cost%>%top_n(10,total_cost),aes(rank,total_cost,fill=rank))+geom_bar(stat="identity")+
  geom_text(aes(label=paste0("skc:",skc)),vjust=1,size=3.5,colour='white')+
  guides(fill=F)+theme_minimal()+ylab("累计销售额")+xlab("排名")

##选取目标skc
target_skc<-NA
for(i in 1:50) target_skc<-rbind(target_skc,sale_info[sale_info$skc==skc_sale$skc[i],])
target_skc<-target_skc[-1,]
head(target_skc)
#write.csv(target_skc,"目标skc.csv")

##top50 skc时间轴均值变化
mean_top50skc<-target_skc%>%
  group_by(date_rcd)%>%
  summarise(mean_sale=mean(s),mean_cost=mean(real_cost))
head(mean_top50skc)
#write.csv(mean_top50skc,"目标skc的平均销售量.csv")
library(xts)
timeline_top50skc<-as.xts(mean_top50skc$mean_cost,as.Date(mean_top50skc$date_rcd))
plot(timeline_top50skc,main="目标SKC平均销售额情况时序图")


p1=ggplot(mean_top50skc, aes(x=as.Date(date_rcd))) + 
  geom_line(aes(y=mean_sale)) + 
  labs(title="目标SKC平均销售量情况时序图", 
       subtitle="目标skc为销售时间处于2018-07-01至2018-10-01内累计销售额排名前50的skc", 
       x="时间线",
       y="目标skc平均销售量")+theme_minimal() # title and caption
p1

##高亮节日时间段
#元旦 2018-12-30 —— 2019-01-01
#国庆 2018-10-01 —— 2018-10-07
#双11 2018-11-11
#双12 2018-12-12

double11<-which(mean_top50skc$date_rcd=="2018-11-11")
double12<-which(mean_top50skc$date_rcd=="2018-12-12")
nation_start<-which(mean_top50skc$date_rcd=="2018-10-01")
nation_end<-which(mean_top50skc$date_rcd=="2018-10-07")
NYD_start<-which(mean_top50skc$date_rcd=="2018-12-30")
NYD_end<-which(mean_top50skc$date_rcd=="2019-01-01")



p1+geom_point(aes(as.Date(date_rcd[double11]),
                  mean_sale[double11]),col="red",size=1.5)+
  geom_text(aes(as.Date(date_rcd[double11]),
                mean_sale[double11]+0.5,label="双十一"),size=3,col=2)+
  geom_point(aes(as.Date(date_rcd[double12]),
                 mean_sale[double11]),col="red",size=1.5)+
  geom_text(aes(as.Date(date_rcd[double12]),
                mean_sale[double11]-0.5,label="双十二"),size=3,col=2)+
  annotate("rect", fill = "red", alpha = 0.2, xmin = as.Date(c("2018-10-01")), xmax = as.Date(c("2018-10-07")),ymin = -Inf, ymax = Inf)+
  geom_text(aes(as.Date(date_rcd[nation_start]),
                mean_sale[nation_start],label="国庆"),size=3,col=2)+
  annotate("rect", fill = "red", alpha = 0.2, xmin = as.Date(c("2018-12-30")), xmax = as.Date(c("2019-01-01")),ymin = -Inf, ymax = Inf)+
  geom_text(aes(as.Date(date_rcd[NYD_end]),
                mean_sale[NYD_end],label="元旦"),size=3,col=2)

##前5高销售额日期
data.frame(arrange(mean_top50skc%>%
                     top_n(5,mean_cost),mean_cost)[5:1,])


####特征提取####
library(readxl)
#1.节假日效应
holiday_info<-read_xlsx("holiday_info.xlsx")
head(holiday_info)

holiday_num<-NA
for(i in 1:nrow(holiday_info)) holiday_num<-c(holiday_num,
                                              as.numeric(as.Date(holiday_info$date_fest_start[i])):as.numeric(as.Date(holiday_info$date_fest_end[i])))
holiday_num<-holiday_num[-1]

is_holiday<-ifelse(target_skc$date_num %in% holiday_num,1,0)
target_skc$is_holiday<-is_holiday
target_skc

#2.折扣影响
prod_info<-read.csv("附件2：prod_info.csv",header = T)
head(prod_info)
names(prod_info)[1]<-"skc"
dim(target_skc)
dim(prod_info)
final_skc_Q1<-merge(target_skc,prod_info,by = c("skc"))
head(final_skc_Q1)
discount<-(final_skc_Q1$tag_price-final_skc_Q1$real_cost/final_skc_Q1$s)/final_skc_Q1$tag_price
is_discount<-ifelse(discount>0,1,0)
sum(is_discount)/length(discount)#打折时间比率
final_skc_Q1$discount<-discount
final_skc_Q1$is_discount<-is_discount

#3.库存影响
inv_info<-read.csv("附件3：inv_info.csv",header = T)
head(inv_info)
names(inv_info)[1]<-"skc"
dim(final_skc_Q1)
dim(inv)
inv<-inv_info[(inv_info$skc %in% as.numeric(names(table(target_skc$skc)))),]

Q1_final_skc<-merge(final_skc_Q1,inv,by=c("skc","date_rcd"))
head(Q1_final_skc)
#write.csv(Q1_final_skc,"问题1目标skc最终数据")


##四个节日对应数据
four_holiday<-mean_top50skc[c(double11,double12,nation_start:nation_end,NYD_start:NYD_end),]
four_holiday

holiday_period<-mean_top50skc[(nation_start-7):(NYD_end+7),]
p2=ggplot(holiday_period, aes(x=as.Date(date_rcd))) + 
  geom_line(aes(y=mean_cost)) + 
  labs(title="目标SKC在四个节日期间平均销售额情况时序图", 
       subtitle="目标skc为销售时间处于2018-07-01至2018-10-01内累计销售额排名前50的skc", 
       x="时间线",
       y="目标skc平均销售额")+theme_minimal() # title and caption
p2+geom_point(aes(as.Date(c("2018-11-11")),
                  mean_cost[date_rcd=="2018-11-11"]),col="red",size=1.5)+
  geom_text(aes(as.Date(c("2018-11-11")),
                mean_cost[date_rcd=="2018-11-11"]+50,label="双十一"),size=3,col=2)+
  geom_point(aes(as.Date(c("2018-12-12")),
                 mean_cost[date_rcd=="2018-12-12"]),col="red",size=1.5)+
  geom_text(aes(as.Date(c("2018-12-12")),
                mean_cost[date_rcd=="2018-12-12"]+50,label="双十二"),size=3,col=2)+
  annotate("rect", fill = "red", alpha = 0.2, xmin = as.Date(c("2018-10-01")), xmax = as.Date(c("2018-10-07")),ymin = -Inf, ymax = Inf)+
  geom_text(aes(as.Date(c("2018-10-05")),
                mean_cost[date_rcd=="2018-10-05"],label="国庆"),size=3,col=2)+
  annotate("rect", fill = "red", alpha = 0.2, xmin = as.Date(c("2018-12-30")), xmax = as.Date(c("2019-01-01")),ymin = -Inf, ymax = Inf)+
  geom_text(aes(as.Date(c("2018-12-31")),
                mean_cost[date_rcd=="2018-12-31"],label="元旦"),size=3,col=2)




#######变量为：skc+is_holiday+year_id+tiny_class_code+tag_price+discount **无销售量作为自变量
####决策树模型####
library(rpart)
str(Q1_final_skc)
Q1_final_skc$tiny_class_code<-factor(Q1_final_skc$tiny_class_code)
Q1_final_skc$is_holiday<-factor(Q1_final_skc$is_holiday)
Q1_final_skc$is_discount<-factor(Q1_final_skc$is_discount)
rt<-rpart(real_cost~skc+is_holiday+year_id+tiny_class_code+tag_price+discount, data = Q1_final_skc)
library(rpart.plot)
rt.bestcp = rt$cptable[which.min(rt$cptable[,"xerror"]), "CP"]
rt.ptree = prune(rt, cp = rt.bestcp)
prp(rt.ptree, type = 2, fallen.leaves = T, main="回归决策树模型（CART）",cex=0.6,split.cex=1)


model<-rpart(real_cost~skc+is_holiday+year_id+tiny_class_code+tag_price+discount, data = Q1_final_skc, method = "anova")
printcp(model)
plotcp(model)
pmodel<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
rpart.plot(pmodel,type=1,digits=3,fallen.leaves = T,cex=0.8,split.cex=1)


pred<-predict(model,Q1_final_skc)
APE<-abs(pred-Q1_final_skc$real_cost)/Q1_final_skc$real_cost
mean(APE)
MAPE<-sum(APE)/length(pred);MAPE

##KNN
library(kknn)
knn.model<-kknn(real_cost~skc+is_holiday+year_id+tiny_class_code+tag_price+discount, 
                train = Q1_final_skc,test = Q1_final_skc, k=10)
pred.knn<-knn.model$fitted.values
APE2<-abs(pred.knn-Q1_final_skc$real_cost)/Q1_final_skc$real_cost
mean(APE2)
MAPE2<-sum(APE2)/length(pred.knn);MAPE2

##随机森林
library(randomForest)
forest<-randomForest(real_cost~skc+is_holiday+year_id+tiny_class_code+tag_price+discount, data = Q1_final_skc)
forest
pred.forest<-predict(forest,Q1_final_skc)
APE3<-abs(pred.forest-Q1_final_skc$real_cost)/Q1_final_skc$real_cost
mean(APE3)
MAPE3<-sum(APE3)/length(pred.lm);MAPE3

##模型对比
plot(Q1_final_skc$real_cost,type="l",ylab="销售额",xlab = "时间线")
lines(pred,col=2)
legend("topleft",col=1:2,lty=1,legend = c("实际值","决策树模型预测值"),cex=0.66)
text(13000,10000,paste0("MAPE:",MAPE))

plot(Q1_final_skc$real_cost,type="l",ylab="销售额",xlab = "时间线")
lines(pred.forest,col='skyblue')
legend("topleft",col=c("black",'skyblue'),lty=1,legend = c("实际值","随机森林模型预测值"),cex=0.66)
text(13000,10000,paste0("MAPE:",MAPE3))

plot(Q1_final_skc$real_cost,type="l",ylab="销售额",xlab = "时间线")
lines(pred.knn,col=3)
legend("topleft",col=c(1,3),lty=1,legend = c("实际值","KNN模型预测值"),cex=0.66)
text(13000,10000,paste0("MAPE:",MAPE2))


##打折影响对比
discount_effect<-Q1_final_skc%>%
  group_by(is_discount)%>%
  summarise(mean_cost=mean(real_cost))
discount_effect$is_discount<-factor(discount_effect$is_discount,levels = c(1,0),labels = c("是","否"))
discount_effect

ggplot(discount_effect,aes(is_discount,mean_cost))+geom_bar(stat="identity",fill=c('skyblue','red'))+
  ylab("平均销售额")+xlab("是否打折")+coord_flip()+theme_minimal()+
  geom_text(aes(label=paste0(round(mean_cost,3),"￥")),hjust=1,size=5,colour='white')


##节日影响对比
holiday_effect<-Q1_final_skc%>%
  group_by(is_holiday)%>%
  summarise(mean_cost=mean(real_cost))
holiday_effect$is_holiday<-factor(holiday_effect$is_holiday,levels = c(1,0),labels = c("是","否"))
holiday_effect
ggplot(holiday_effect,aes(is_holiday,mean_cost))+geom_bar(stat="identity",fill=c('green','red'))+
  ylab("平均销售额")+xlab("是否节日")+coord_flip()+theme_minimal()+
  geom_text(aes(label=paste0(round(mean_cost,3),"￥")),hjust=1,size=5,colour='white')

##库存影响
library(car)
scatterplot(Q1_final_skc$ie,Q1_final_skc$real_cost,xlab="库存",ylab="销售额",col="skyblue")
ie_model<-lm(real_cost~ie,data=Q1_final_skc)
summary(ie_model)
##打折 节日 库存综合影响
ggplot(Q1_final_skc,aes(ie,real_cost,shape=is_discount,col=is_holiday))+geom_point()+
  ylab("实际销售额")+xlab("库存")+theme_minimal()+labs(col="是否节日",shape="是否打折")+
  scale_colour_discrete(labels=c("否", "是"))+ scale_shape_discrete(labels=c("否", "是"))+
  scale_color_manual(values=c("#0073C2FF", "red"))

Q1_final_skc%>%
  filter(is_discount==0)%>%
  summarise(max(real_cost))
sum(Q1_final_skc$real_cost>=2782.5)/nrow(Q1_final_skc)
####加入上周销售量变量####
cost_last_week<-cumsum(mean_top50skc$mean_cost[1:6])
for(i in 1:(nrow(mean_top50skc)-6)){
  ma7<-sum(mean_top50skc$mean_cost[i:(i+6)])
  cost_last_week<-append(cost_last_week,ma7)
}
cost_last_week

top50skc_cost<-cbind(mean_top50skc,cost_last_week)
head(top50skc_cost)

#基于上周销售量预测当日销售量
liner.model<-glm(mean_cost~cost_last_week,top50skc_cost,family = "gaussian")
summary(liner.model)
mean(abs(liner.model$residuals)/top50skc_cost$mean_cost)#MAPE
top50skc_cost$pre_cost<-liner.model$fitted.values

p3=ggplot(top50skc_cost, aes(x=as.Date(date_rcd))) + 
  geom_line(aes(y=cost_last_week)) + 
  labs(title="目标SKC销售额情况时序图", 
       subtitle="黑色实线为目标SKC上周累计销售量，红色实线为目标SKC当日销售量,
       蓝色实线为目标SKC根据上周累计销售量预测的当日销售量", 
       x="时间线",
       y="销售量")+theme_minimal() # title and caption
p3.2=p3+geom_line(aes(y=mean_cost),col="red")+
  geom_label(aes(label = "上周累计销售量",x=as.Date("2019-12-30"),y=3000),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  geom_label(aes(label = "当日销售量预测值",x=as.Date("2019-12-30"),y=1000,col="red"),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  guides(col=F)
p3.2+geom_line(aes(y=pre_cost),col="blue")+
  geom_label(aes(label = "当日销售量",x=as.Date("2019-05-30"),y=1000,col="blue"),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  geom_label(aes(label = paste0("MAPE:",mean(abs(liner.model$residuals)/mean_cost)),
                 x=as.Date("2019-06-30"),y=6000),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)
names(Q1_final_skc)
#write.csv(Q1_final_skc,"问题1目标skc最终数据.csv")
#####问题2#####
#2019-6-1——2019-10-1
Q2_class<-merge(sale_info,prod_info,by="skc")
train_start<-as.numeric(as.Date("2019-06-01"))
train_end<-as.numeric(as.Date("2019-10-1"))
test_start<-as.numeric(as.Date("2019-10-01"))
test_end<-as.numeric(as.Date("2019-12-30"))
Q2_final_class<-Q2_class%>%
  filter(date_num>=train_start & date_num<=test_end)%>%
  group_by(tiny_class_code)%>%
  summarise(total_cost=sum(real_cost))%>%
  top_n(10,total_cost)
Q2_final_class

##销售额top10 class
Q2_final_class<-arrange(Q2_final_class,total_cost)
Q2_final_class$rank<-factor(nrow(Q2_final_class):1)
ggplot(Q2_final_class,aes(rank,total_cost,fill=rank))+geom_bar(stat="identity")+
  geom_text(aes(label=paste0("小类编码:",tiny_class_code)),vjust=1,size=3.5,colour='black')+
  guides(fill=F)+theme_minimal()+ylab("累计销售额")+xlab("排名")+coord_flip()

target_class<-NA
for(i in 1:10) target_class<-rbind(target_class,Q2_class[Q2_class$tiny_class_code==Q2_final_class$tiny_class_code[i],])
target_class<-target_class[-1,]
dim(target_class)
target_class$yearmon<-as.yearmon(target_class$date_rcd)

yearmon_target<-target_class%>%
  group_by(yearmon)%>%
  summarise(month_cost=sum(real_cost))


####特征提取####
#1.节假日效应
is_holiday<-ifelse(target_class$date_num %in% holiday_num,1,0)
target_class$is_holiday<-is_holiday
sum(target_class$is_holiday==1)/nrow(target_class)

#2.折扣影响
target_class$discount<-(target_class$tag_price-target_class$real_cost/target_class$s)/target_class$tag_price
target_class$discount<-ifelse(target_class$discount>0,1,0)
sum(target_class$discount)/length(target_class$discount)#打折时间比率

#3.上周销售量变量
target_class_train<-target_class%>%
  filter(date_num>=train_start & date_num<=train_end)%>%
  group_by(tiny_class_code)%>%arrange(date_num)
target_class_train

mean_target_class<-target_class_train%>%
  group_by(date_rcd)%>%
  summarise(mean_cost=mean(real_cost))
mean_target_class

last_week_cost<-cumsum(mean_target_class$mean_cost[1:6])
for(i in 1:(nrow(mean_target_class)-6)){
  ma7<-sum(mean_target_class$mean_cost[i:(i+6)])
  last_week_cost<-append(last_week_cost,ma7)
}
last_week_cost

top10class_cost<-cbind(mean_target_class,last_week_cost)
head(top10class_cost)

#基于上周销售量预测当日销售量
liner.model2<-glm(mean_cost~last_week_cost,top10class_cost,family = "gaussian")
summary(liner.model2)

target_class_test<-target_class%>%
  filter(date_num>=test_start & date_num<=test_end)%>%
  group_by(tiny_class_code)%>%arrange(date_num)

mean_target_class_test<-target_class_test%>%
  group_by(date_rcd)%>%
  summarise(mean_cost=mean(real_cost))
mean_target_class_test

last_week_cost<-cumsum(mean_target_class_test$mean_cost[1:6])
for(i in 1:(nrow(mean_target_class_test)-6)){
  ma7<-sum(mean_target_class_test$mean_cost[i:(i+6)])
  last_week_cost<-append(last_week_cost,ma7)
}
last_week_cost

top10class_cost_test<-cbind(mean_target_class_test,last_week_cost)
head(top10class_cost_test)
#write.csv(target_class_train,"top10目标小类数据.csv")
##模型预测
top10class_cost_test$pre_cost<-predict(liner.model2,top10class_cost_test)


p4=ggplot(top10class_cost_test, aes(x=as.Date(date_rcd))) + 
  geom_line(aes(y=last_week_cost)) + 
  labs(title="目标小类平均销售额情况时序图", 
       subtitle="黑色实线为目标小类上周平均累计销售量，
       红色实线为目标小类平均当日销售量，
       蓝色实线为目标小类根据上周平均累计销售量预测的当日销售量", 
       x="时间线",
       y="销售量")+theme_minimal() # title and caption

p4.2=p4+geom_line(aes(y=mean_cost),col="red")+
  geom_label(aes(label = "上周累计销售量",x=as.Date("2019-10-01"),y=1000),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  geom_label(aes(label = "当日销售量预测值",x=as.Date("2019-12-01"),y=250,col="red"),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  guides(col=F)
p4.2+geom_line(aes(y=pre_cost),col="blue")+
  geom_label(aes(label = "当日销售量",x=as.Date("2019-12-30"),y=250,col="blue"),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)+
  geom_label(aes(label = paste0("MAPE:",mean(abs(pre_cost-mean_cost)/mean_cost)),
                 x=as.Date("2019-12-30"),y=1000),position=position_dodge(width=0.5),vjust='inward',hjust='inward',angle=45,size=4)


###月销售额计算
top10class_cost_test$month<-substring(as.character(as.yearmon(top10class_cost_test$date_rcd)),1,2)
month_cost_test_pre<-top10class_cost_test%>%
  group_by(month)%>%
  summarise(month_cost=sum(mean_cost),month_cost_pre=sum(pre_cost))
data.frame(month_cost_test_pre)
month_MAPE<-mean(abs(as.numeric(month_cost_test_pre$month_cost_pre-month_cost_test_pre$month_cost))/as.numeric(month_cost_test_pre$month_cost_pre))
month_MAPE
#write.csv(top10class_cost,"目标小类的平均销售额.csv")
####提取各小类####
names(target_class_train)
class_num<-as.numeric(names(table(factor(target_class$tiny_class_code))))
class_1_train<-target_class_train[target_class_train$tiny_class_code==class_num[1],]
class_1_train

class_1_test<-target_class_test[target_class_test$tiny_class_code==class_num[1],]
class_1_test


#计算上周销量
last_week_cost<-cumsum(class_1_train$real_cost[1:6])
for(i in 1:(nrow(class_1_train)-6)){
  ma7<-sum(class_1_train$real_cost[i:(i+6)])
  last_week_cost<-append(last_week_cost,ma7)
}
class_1_train$last_week_cost<-last_week_cost
class_1_train

last_week_cost<-cumsum(class_1_test$real_cost[1:6])
for(i in 1:(nrow(class_1_test)-6)){
  ma7<-sum(class_1_test$real_cost[i:(i+6)])
  last_week_cost<-append(last_week_cost,ma7)
}
class_1_test$last_week_cost<-last_week_cost
class_1_test

class_1_train$pred<-predict(lm.model2,class_1_train)##模型1：仅基于上周销售
class_1_test$pred<-predict(lm.model2,class_1_test)

####优化模型
names(class_1)
lm.class1<-lm(real_cost~last_week_cost+skc+tag_price+is_holiday+discount+yearmon,data=class_1_train)
summary(lm.class1)

class_1_train$pred2<-lm.class1$fitted.values
class_1_test$pred2<-predict(lm.class1,class_1_test)
train_month_info<-class_1_train%>%group_by(yearmon)%>%summarise(total_sale=sum(s),month_cost=sum(real_cost),
                                                                holiday_ratio=mean(is_holiday),discount_days_ratio=mean(is_discount),
                                                                mean_discount=mean(discount),
                                                                pre_month_cost=sum(pred),
                                                                pre_month_cost2=sum(pred2))
train_month_info$APE<-abs(train_month_info$month_cost-train_month_info$pre_month_cost)/train_month_info$month_cost
train_month_info$APE2<-abs(train_month_info$month_cost-train_month_info$pre_month_cost2)/train_month_info$month_cost
data.frame(train_month_info)
MAPE_1<-apply(train_month_info[,(ncol(train_month_info)-1):ncol(train_month_info)],2,mean)
MAPE_1
class_num[1]

test_month_info<-class_1_test%>%group_by(yearmon)%>%summarise(total_sale=sum(s),month_cost=sum(real_cost),
                                                              holiday_ratio=mean(is_holiday),discount_days_ratio=mean(is_discount),
                                                              mean_discount=mean(discount),
                                                              pre_month_cost=sum(pred),
                                                              pre_month_cost2=sum(pred2))

test_month_info$APE<-abs(test_month_info$month_cost-test_month_info$pre_month_cost)/test_month_info$month_cost
test_month_info$APE2<-abs(test_month_info$month_cost-test_month_info$pre_month_cost2)/test_month_info$month_cost
data.frame(test_month_info)
MAPE_2<-apply(test_month_info[,(ncol(test_month_info)-1):ncol(test_month_info)],2,mean)
MAPE_2
class_num[1]


summary(lm.model2)
summary(lm.class1)
######问题3######
plot(class_1_train$last_week_cost,type="l")
class_1_test$pred2


#######目标小类预测
target_class_week_info<-class_1_test%>%
  group_by(date_rcd)%>%
  summarise(mean_cost=mean(real_cost))
target_class_week_info

target_class_week_info_pre<-class_1_test%>%
  group_by(date_rcd)%>%
  summarise(mean_cost_pre=mean(pred2))
target_class_week_info_pre

plot(target_class_week_info$mean_cost,type="l",ylab="销售额",xlab="目标小类2019-10-01日后的每日销售情况")
lines(target_class_week_info_pre$mean_cost_pre,type="l",col=2,lty=2)
legend("topleft",col=1:2,lty=1:2,legend = c("实际销售额","销售额预测值"),cex=0.8)
text(70,150,paste0("MAPE:",mean(abs(target_class_week_info$mean_cost-target_class_week_info_pre$mean_cost_pre)/target_class_week_info$mean_cost)))

#计算下周销量
next_week_cost<-sum(target_class_week_info$mean_cost[1:6])
next_week_cost

for(i in 1:11){
  ma7<-sum(target_class_week_info$mean_cost[(7*i):(7*(i+1))])
  next_week_cost<-append(next_week_cost,ma7)
}

#write.csv(class_1_train,"目标27050401小类最终数据.csv")

next_week_cost_pre<-sum(target_class_week_info_pre$mean_cost_pre[1:6])
next_week_cost_pre

for(i in 1:11){
  ma7<-sum(target_class_week_info_pre$mean_cost_pre[(7*i):(7*(i+1))])
  next_week_cost_pre<-append(next_week_cost_pre,ma7)
}
plot(next_week_cost,type="l",ylab="销售额",xlab="目标小类2019-10-01日后的每周销售情况",ylim=c(min(next_week_cost),max(next_week_cost_pre)))
lines(next_week_cost_pre,type="l",col="blue",lty=3)
legend("topleft",col=c("red","blue"),lty=c(1,3),legend = c("实际销售额","销售额预测值"),cex=0.8)
text(8,1400,paste0("MAPE:",mean(abs(next_week_cost-next_week_cost_pre)/next_week_cost)))