#读入房屋数据集，准备数据
houses <- read.csv("D:/project/R/DMPA_data_sets/Data sets/houses/houses.txt",stringsAsFactors = FALSE,header = FALSE,sep =""  )

names(houses) <- c("MVAL","MINC","HAGE","ROOMS","BEDRMS","POPN","HHLDS","LAT","LONG")

#标准化变量
houses$MINC_Z <- (houses$MINC-mean(houses$MINC)/sd(houses$MINC))
houses$HAGE_Z <- (houses$HAGE-mean(houses$HAGE)/sd(houses$HAGE))

#同样操作作用月其他变量
#随机选取90%用于测试数据集
choose <-runif(dim(houses)[1],0,1)
test.house <- houses[which(choose<.1),]
train.house <- houses[which(choose>=.1),]
  
#主成分分析
install.packages("psych")
library(psych)

#主成分
pcal <- principal(train.house[,c(1:9)], nfactors = 9, rotate = "none",scores = TRUE)
#特征值
pcal$values
#解释变异
pcal$loadings

#坡度图
plot(pcal$values,type = "b",main ="ScreePlotfor housesData")

#坡度图因子得分
pairs(~train.house$MINC+train.house$HAGE+pcal$scores[,3],labels=c("Median Income ","housing Median Age","Component 3scores"))

pairs(~train.house$MINC+train.house$HAGE+pcal$scores[,4],labels=c("Median Income ","housing Median Age","Component 4scores"))


#计算共性
comm3 <- loadings(pcal)[2,1]^2+loadings(pcal)[2,2]^2+loadings(pcal)[2,3]^2

comm4 <- loadings(pcal)[2,1]^2+loadings(pcal)[2,2]^2+loadings(pcal)[2,3]^2+loadings(pcal)[2,4]^2
comm3;comm4

#主成分验证
pca2 <- principal(test.house[,c(1:9)], nfactors = 9, rotate = "none",scores = TRUE)
pca2$loadings



# 读入，准备数据用于因子分析
adult <- read.csv("D:/project/R/DMPA_data_sets/Data sets/adult/Clem3Training",stringsAsFactors = FALSE,header = FALSE )

