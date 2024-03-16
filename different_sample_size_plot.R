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
      
     
      xls=unlist(xls);x2ls=unlist(x2ls); yls=unlist(yls)      
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
         
        }
        #was sum
        mse=mean( (fit$residuals^2))
        lsmse = c(lsmse,mse)
        
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
