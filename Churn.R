#读入Churn数据集
churn <- read.csv(file="D:/gitProjects/R/DMPA_data_sets/Data sets/churn.txt",stringsAsFactors = TRUE)
#显示前10条记录
churn[1:10,]

#总结客户流失变量
sum.churn <- summary(churn$Churn)
sum.churn

#计算客户流失比例
prop.churn <-sum(churn$Churn. == "True.") /length(churn$Churn.)
prop.churn

#流失变量的条形图
barplot(sum.churn,ylim = c(0,3000),main="Bar Garph of Churn and Non-churners",
        col = "lightblue")
box(which = "plot", lty = "solid" ,col="black")

#为客户流失和国际套餐的计数建表
counts <- table(churn$Churn, churn$Int.l.Plan,dnn = c("Churn","International Plan"))
counts

#叠加柱状图
barplot(counts,legend=rownames(counts),col = c("blue","red"), ylim = c(0,3300),xlab = "国际套餐",ylab = "计数", 
        main = "Comparsion Bar Chart:Churn Proportions by Internatiional Plan")
box(which = "plot", lty = "solid" ,col="black")

#创建两个变量的汇总表
sumtable <- addmargins(counts,FUN = sum)
sumtable

#创建分行比例表
row.margin <- round(prop.table(counts,margin = 1),4)*100
row.margin

#创建分列比例表
col.margin <- round(prop.table(counts,margin = 2),4)*100
col.margin

#带有图例的聚类条形图
barplot(t(counts),col = c("blue","green"), ylim = c(0,3300), ylab = "Counts",xlab = "Churn", main="International Plan Count by Churn",beside = TRUE)
legend("topright",c(rownames(counts)),col = c("blue","green"), pch = 15, title = "Int'l Plan")
box(which = "plot", lty = "solid" ,col="black")


#客户服务呼叫的非覆盖直方图
hist(churn$CustServ.Calls,xlim = c(0,10), xlab = "客户服务电话量", ylab="计数",col="lightblue",main="histogram of Customer service Calls")

#下载并安装R包ggplot2
install.packages("ggplot2")
library(ggplot2)

#覆盖条形图
ggplot() +
   geom_bar(data = churn, aes(x = factor(churn$CustServ.Calls),fill= factor(churn$Churn.)),position = "stack")+
   scale_x_discrete("customer Service Calls")+
   scale_y_continuous("parcent")+
   guides(fill=guide_legend(title = "Churn"))+
   scale_fill_manual(values = c("blue","red"))

ggplot()+
  geom_bar(data = churn,aes(x= factor(churn$CustServ.Calls),fill= factor(churn$Churn.)),position = "fill")+
  scale_x_discrete("customer Service Calls")+
  scale_y_continuous("parcent")+
  guides(fill=guide_legend(title = "Churn"))+
  scale_fill_manual(values = c("blue","red"))

#t-检验和国际电话的两个例子
churn.false <- subset(churn,churn$Churn. =="False.")
churn.true <- subset(churn, churn$Churn.=="True.")
t.test(churn.false$Intl.Calls,churn.true$Intl.Calls)

#傍晚使用时长和白天使用时长的散点图，将客户流失着色
plot(churn$Eve.Mins,churn$Day.Mins,xlim=c(0,400), ylim = c(0,400), xlab="Evening minutes", ylab="Day minutes", main="Scatterplot of day and Evening minutes by Churn",
    col=ifelse(churn$Churn.=="True.","red","blue")) 
legend("topright", c("True","False"), col = c("red","blue"),pch = 1, title = "Churn")

#u