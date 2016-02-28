library(datasets)
library(ggplot2)
data<-ToothGrowth

figure <- ggplot(data, aes(x = as.factor(dose), y = len, 
                             fill = supp))
figure <- figure + geom_bar(stat = "identity")
figure <- figure + facet_grid(. ~ supp)
figure<-figure+theme_bw(base_size = 20)+xlab("Dose")+ylab('Length of Tooth')
print(figure)