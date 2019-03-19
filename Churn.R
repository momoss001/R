#读入churn数据集
churn <- read.csv(file="D:/project/R/DMPA_data_sets/Data sets/churn.txt",stringsAsFactors = TRUE)
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

#为客户和国际套餐流失量建表
counts <- table(churn$Churn, churn$Int.l.Plan,dnn = c("Churn","International Plan"))
counts

