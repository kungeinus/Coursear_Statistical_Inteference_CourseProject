library(datasets)
library(ggplot2)
data<-ToothGrowth
data$dose<-as.factor(data$dose)

data_summary<-aggregate(data=data,len~supp+dose,FUN = mean)
names(data_summary)<-c("Supplement Type","Dose","Len_Mean")
data_summary$Len_Median<-aggregate(data=data,len~supp+dose,FUN = median)$len
data_summary$Len_Std<-aggregate(data=data,len~supp+dose,FUN = sd)$len
data_summary$Len_Var<-aggregate(data=data,len~supp+dose,FUN = var)$len
data_summary


figure <- ggplot(data, aes(x = dose, y = len, 
                             fill = supp))
figure <- figure + geom_boxplot()
figure <- figure + facet_grid(. ~ supp)
figure<-figure+theme_bw(base_size = 20)+xlab("Dose(mg/day)")+ylab('Tooth Length')
figure<-figure+guides(fill=guide_legend(title="Supplement type"))
print(figure)

t.test(len~supp,data = subset(data,dose==0.5))
t.test(len~supp,data = subset(data,dose==1))
t.test(len~supp,data = subset(data,dose==2))
t.test(len~dose,data = subset(data,dose %in% c(0.5,1) & supp=="OJ"))
t.test(len~dose,data = subset(data,dose %in% c(0.5,2) & supp=="OJ"))
t.test(len~dose,data = subset(data,dose %in% c(1,2) & supp=="OJ"))
t.test(len~dose,data = subset(data,dose %in% c(0.5,1) & supp=="VC"))
t.test(len~dose,data = subset(data,dose %in% c(0.5,2) & supp=="VC"))
t.test(len~dose,data = subset(data,dose %in% c(1,2) & supp=="VC"))