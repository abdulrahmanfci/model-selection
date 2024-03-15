xls=list()
x2ls=list()
yls=list()

lmlist=list()
lmlist[[1]] <-  reformulate(c("xls", "x2ls"), response = "yls")
lmlist[[2]] <- reformulate(c("xls", "x2ls","xls*x2ls"), response = "yls")
lmlist[[3]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)"), response = "yls")
lmlist[[4]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)","I(x2ls^2)"), response = "yls")
lmlist[[5]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)","I(x2ls^2)", "I(x2ls^3)"), response = "yls")
lmlist[[6]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)","I(x2ls^2)","I(xls^3)", "I(x2ls^3)"), response = "yls")
lmlist[[7]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)","I(x2ls^2)","I(xls^3)", "I(x2ls^3)","I(xls^4)"), response = "yls")
lmlist[[8]] <-  reformulate(c("xls", "x2ls", "xls*x2ls", "I(xls^2)","I(x2ls^2)","I(xls^3)", "I(x2ls^3)","I(xls^4)","I(xls^5)",
                              "I(x2ls^5)","I(xls^6)","I(x2ls^6)","I(xls^7)","I(xls^8)",
                              "I(x2ls^8)","I(xls^9)","I(x2ls^9)","I(xls^10)","I(x2ls^10)",
                              "I(xls^11)","I(x2ls^11)","I(xls^12)","I(x2ls^12)","I(xls^13)",
                              "I(xls^13)","I(x2ls^13)"), response = "yls")

ssize = c(100,200,300,400,500,800,1000,1500,2000,4000,6000)#,800,1000,1500,2000)
ssize = c(7000)
nsamples = 5

mnames <- c("m0","m1","m2","m3","m4","m5")
mseres = matrix(0,length(ssize),length(lmlist)+1)

mseall = matrix(0,length(ssize)*(length(lmlist)+1), nsamples)
xls_origin=list()
x2ls_origin=list()
yls_origin=list()

intervention=25
osize = 2000
cat("\nsize: ",as.character(Sys.time()),"\n")
for(i in 1:intervention){
  j <- 0
  for(j in 1:osize){
    xls_origin<-c(xls_origin,list(x1[i]))
    x2ls_origin<-c(x2ls_origin,x2[i])
    yls_origin<-c(yls_origin,data[j,i])
  }
}  

xls_origin=unlist(xls_origin);x2ls_origin=unlist(x2ls_origin); yls_origin=unlist(yls_origin)


counter = 0
for(m in 1:length(ssize)){
  
  cat("\nsize: ",ssize[m]," ",as.character(Sys.time()),"\n")
  print(mseres)
  
  
  
  for(k in 1:(length(lmlist)+1)){
    counter <- counter+1
    lsmse = list()
    lsmse2 = list()
    for(l in 1:nsamples){
      
      intervention=25#length(x1)
      #default 500
      
      xls=list()
      x2ls=list()
      yls=list()
      
      #cat("\n",as.character(Sys.time()))
      i <- 0
      j <- 0
      
      iteration=ssize[m]
      for(i in 1:intervention){
        j <- 0
        start <- (l-1)*iteration+1
        end <- min(30000,l*iteration+1)
        for(j in start:end){
          xls<-c(xls,list(x1[i]))
          x2ls<-c(x2ls,x2[i])
          yls<-c(yls,data[j,i])
        }
        
        #cat("\nprint j:",j,l,iteration,"\n")
      }
      
      #cat("\n",as.character(Sys.time()))
      
      #start = (l-1)*iteration
      #end = l*iteration
      #cat("\n start:",start," ",end," ",iteration,"\n")
      
      xls=unlist(xls);x2ls=unlist(x2ls); yls=unlist(yls)
      #cat("\n yls",length(yls)," ",tail(yls))
      
      if(k == 1){
        #was sum
        mse=mean( (yls-mean(yls))^2 )
        lsmse = c(lsmse,mse)
        
        sumtc =0
        for(i in 1:intervention){
          for(j in 1:osize){
            sumtc <- sumtc+(yls_origin[((i-1)*osize+j)]-mean(yls[((i-1)*iteration):(i*iteration)]))^2
          }
          
          #cat("\nprint j:",i,j,sumtc,"\n")
        }
        
        mse2 = sumtc/(intervention*osize)
        lsmse2 = c(lsmse2,mse2)
        # if(m>=7){
        # cat("mse in 1:",m," ",k," ",l," ",mse,mean(yls)," ",sum(yls-mean(yls)),"\n")
        # }
        
      }
      else{
        df <- data.frame(xls,x2ls,yls,stringsAsFactors = FALSE)
       
        fit <- lm(lmlist[[k-1]],data=df)
        p=predict(fit,newdata=data.frame(xls=x1,x2ls=x2),interval='confidence')
        sumtc =0
        
        for(i in 1:intervention){
          intvar = list()
          for(j in 1:osize){
           sumtc <- sumtc+(yls_origin[((i-1)*osize+j)]-p[i])^2
           intvar = c(intvar,(yls_origin[((i-1)*osize+j)]-p[i])^2)
          }
          #cat("\n",mean(unlist(intvar))," ",sd(unlist(intvar)) )
          #cat("\nprint j:",i,j,sumtc,"\n")
        }
        #was sum
        mse=mean( (fit$residuals^2))
        lsmse = c(lsmse,mse)
        #print(mse)
        #print(p)
        mse2 = sumtc/(intervention*osize)
        lsmse2 = c(lsmse2,mse2)
        
        }
      }
    cat("\nlsmse: ",k," ",unlist(lsmse),"\nlsmse2",unlist(lsmse2))
    #print(yls)
    avg = mean(unlist(lsmse))
    avg = mean(unlist(lsmse2))
    mseres[m,k] = avg
    mseall[counter,] = unlist(lsmse2)
  }
}

library(ggplot2)
library(reshape2)
mseres21 <- mseres[1:4,]
mseres22 <- mseres[5:11,]
df <- data.frame(mseres22)
rownames(df) <- c(100,200,300,400)#
rowname <- c(500,800,1000,1500,2000,4000,6000)
rownames(df) <- c(500,800,1000,1500,2000,4000,6000)
colnames(df) <- mnames

library(data.table)
library(tidyverse)
library(directlabels)


df2 <- t(df) #transpose(df)

# get row and colnames in order
#colnames(df2) <- rownames(df)
#rownames(df2) <- colnames(df)
df2 <- data.frame(df2)
#colnames(df2) <- c(rownames(df)[1:5])
colnames = c('x100','x200','x300','x400','x500','x800','x1000','x2000')
df2['model'] <- colnames(df)

colnames(df2) <- colnames
df2 <- df2[,-c(6)]


#df2 <-df2 %>% pivot_longer(cols = c(X100, X200, X300,X400,X500),
#                                       names_to = "SampleSize")
df <- df[,-c(1)]
#colnames(df) <- c('m1','m2','m3','m4','m5')

#df['rows'] <- c('s1_100','s2_200','s3_300','s4_400')
#df['rows'] <- c('s5_500','s7_800','s8_1000','s9_1500', 'ss_2000')
#colnames(df)[7] <- 'm8'
df['rows'] <- rownames(df)
df <-df %>% pivot_longer(cols = c(m0, m1,m2, m3, m4,m5),
                         names_to = "model")

#df.m <- melt(df)
#color-blind palette
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#previously it was geom_point(aes(shape=model)) without scale_color_discrete
ggplot(df, aes(rows, value, col=model, group=model)) +
  geom_line() +  #geom_line(aes(linetype=model))
  geom_point(aes(shape=model))+
  #scale_linetype_manual( values = unique(model))+
  #scale_fill_manual() +
  theme(legend.position = c(0.8, 0.8),legend.title=element_text(size=14)) +
  #scale_x_discrete(limits=df$rows)+
  ylab("MSE")+ xlab("Sample size") + lims(x=df$rows)+
  scale_color_discrete(name="Model",labels = c(expression(y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3], 
                                                          y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3], 
                                                          y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                          y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                          y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                          y==beta[0])))+
scale_shape_discrete(name="Model",labels = c(expression(y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3], 
                                                        y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3], 
                                                        y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                        y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                        y==beta[0]+beta[1]*x[1]+beta[2]*x[2]+beta[3],
                                                        y==beta[0])))

#theme(axis.text.x = element_text(angle = 90, 
#                                 hjust = 1,
#                                 vjust = 0.5))


mseresavg <- mseres
mseallavg <- mseall
mseresavgm7 <- mseres
mseallavgm7 <- mseall
for(i in 1:length(mseall)){
  cat("\nhello: ",i," ",sd(mseall[i,]))
}

mseresold <- mseres
mseallold <- mseall
mseres2
