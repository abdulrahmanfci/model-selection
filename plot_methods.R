library(ggplot2)

ssize = c(100,200,300,400,500,800,1000,1500,2000,4000,6000)
smodel = c('a','b','c','d','e','f')
msedata = matrix(0,length(ssize)*6, 5)

j <- 1
k <- 1
for(i in 1:(length(ssize)*6)){
  if(j > 6){
    j <- 1
    k <- k+1
  }
  
  msedata[i,1] <- ssize[k]
  msedata[i,2] <- j
  avgdata <- mean(mseall[i,])
  msedata[i,3] <- avgdata
  error <- (sd(mseall[i,])/sqrt(5)*1.96)
  msedata[i,4] <- (avgdata-error)
  msedata[i,5] <- (avgdata+error)
  j <- j+1
  
}

df1 <- data.frame(msedata[25:66,])
colnames(df1) <- c("samplesize","method","value","lwr","uppr")
df1$method <- as.character(df1$method)

df1 <- data.frame(msedata[1:24,])
colnames(df1) <- c("samplesize","method","value","lwr","uppr")
df1$method <- as.character(df1$method)

labels1 <- c("direct estimation",expression(y==beta[0]+beta[1]*x[1]+beta[2]*x[2], 
                                            y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3]*x[1]*x[2], 
                                            y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3]*x[1]*x[2]+beta[4]*x[1]^{2},
                                            y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3]*x[1]*x[2]+beta[4]*x[1]^{2}+beta[5]*x[2]^{2},
                                            y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3]*x[1]*x[2]+beta[4]*x[1]^{2}+beta[5]*x[2]^{2}+beta[6]*x[1]^{3}
))


ggplot(df1, aes(factor(samplesize), value, group=method, color=method)) +
  geom_line() +
  geom_point(aes(shape=method))+
 
  theme(legend.position = c(0.7, 0.8), legend.text.align = 0, legend.background = element_blank(), 
        legend.text = element_text(size=12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12)) +
  ylab("MSE")+ xlab("Sample size") + #lims(x=df$rows)+
  scale_color_discrete(name="Model",labels = labels1)+
  scale_shape_discrete(name="Model",labels = labels1)+
  geom_ribbon(aes(ymin=lwr, ymax=uppr), linetype=2, alpha=0.1)
