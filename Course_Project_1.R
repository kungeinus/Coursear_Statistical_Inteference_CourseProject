library(ggplot2)
set.seed(9527)
nsamples<-40
nsim<-1000
lambda<-0.2
sim_data<-matrix(data=0,nrow=nsim,ncol=nsamples)
for (i in 1:nsim)
        sim_data[i,]<-rexp(nsamples,lambda)
exp_mean<-apply(sim_data,1,mean)
sample_mean<-mean(exp_mean)
sample_sd<-sd(exp_mean)
theoretical_mean<-1/lambda
theoretical_sd<-1/lambda/sqrt(nsamples)

mean_text<-paste(paste("Simulated Mean is",round(sample_mean,3)),paste("Comparing to Theoretical Mean is",round(theoretical_mean,3)))
mean_var<-paste(paste("Simulated Var is",round(sample_sd^2,3)),paste("Comparing to Theoretical Var is",round(theoretical_sd^2,3)))
mean_text
mean_var
conf_sim<-sample_mean+c(-1,1)*qnorm(0.975,0,sample_sd)
conf_the<-theoretical_mean+c(-1,1)*qnorm(0.975,0,theoretical_sd)
conf_text_low<-paste(paste("Simulated 95% Confidence Interval Lower Bound is",round(conf_sim,3)[1]),
                 paste("Comparing to Theoretical 95% Confidence Interval Lower Bound",round(conf_the,3)[1]))
conf_text_high<-paste(paste("Simulated 95% Confidence Interval Higher Bound is",round(conf_sim,3)[2]),
                     paste("Comparing to Theoretical 95% Confidence Interval Higher Bound",round(conf_the,3)[2]))
conf_text_low
conf_text_high

density_f<-density(exp_mean)
sample <- seq(min(density_f$x), max(density_f$x), length=length(exp_mean))
sim_density<-approx(density_f$x,density_f$y,sample)
theoretical_density <- dnorm(sample, mean=1/lambda, sd=(1/lambda/sqrt(nsamples)))
plotdata<-data.frame(sim=exp_mean)
plotdata$x<-sample
plotdata$sim_y<-sim_density$y
plotdata$the_y<-theoretical_density



figure<-ggplot(plotdata)
figure<-figure+geom_histogram(aes(x=sim,y=..density..), bins=30, colour="black",fill = "green")
figure<-figure+geom_vline(aes(xintercept = mean(sim)), colour="magenta",size=1,linetype=1)
figure<-figure+geom_text(aes(x=mean(sim)+1.6,label=paste("Simulated Sample Mean =",round(sd(sim),3))),y=0.53,size=7)
figure<-figure+geom_line(aes(x=x,y=sim_y,color='sim'),linetype=1,size=1)
figure<-figure+geom_line(aes(x=x,y=the_y,color='the'),linetype=1,size=1)
figure<-figure+scale_colour_manual(name="Legend",values=c("sim"="red", "the"="blue"),labels=c("Simulated","Theoretical"))
figure<-figure+theme_bw(base_size = 20)+xlab("Mean Value")+ylab('Density')
figure<-figure+annotate("segment", x = conf_the[1], xend = conf_the[1], y = 0, yend = 0.1,
                        colour = "blue",size=1)
figure<-figure+annotate("segment", x = conf_the[2], xend = conf_the[2], y = 0, yend = 0.1,
                        colour = "blue",size=1)
figure<-figure+annotate("segment", x = conf_sim[1], xend = conf_sim[1], y = 0, yend = 0.1,
                        colour = "red",size=1)
figure<-figure+annotate("segment", x = conf_sim[2], xend = conf_sim[2], y = 0, yend = 0.1,
                        colour = "red",size=1)
figure<-figure+geom_text(x = conf_sim[2]+1.1,y=0.12,size=7,label="95% Confidence Interval")
print(figure)

qqnorm(exp_mean)
qqline(exp_mean)



