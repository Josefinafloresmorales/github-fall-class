#JFM ps9 demtechh Spring 2017
# clear
rm(list = ls())

#getting data
  library(readxl)
  tab <- read.csv("~/Downloads/New Folder With Items/ps9.csv")
  View(tab)
  n_max <- length(tab)
#ages 25-55. used for a

#a
  tab$lx_S[22]<-0
#find q(x)S
  
  for(i in 2:22) {
    tab$qS[i]<-(tab$lx_S[i-1]-tab$lx_S[i])/tab$lx_S[i-1]
  }
tab$qS[1]<-0
#find YS(x) 

  for(i in 1:22) {
    tab$YSx[i]<-(1/2)*(log(tab$qS[i]/(1-tab$qS[i])))
  }
  print(tab$YSx)
##What to do wtih the open interval ? 

#find q(x)
  tab$lx[1]<-10000
  tab$lx[2]<-0
  
  tab$lx[3]<-0
  
  tab$lx[4]<-0
  
  tab$lx[5]<-0
  tab$lx[6]<-0
  tab$lx[14]<-0
  tab$lx[15]<-0
  tab$lx[16]<-0
  tab$lx[17]<-0
  tab$lx[18]<-0
  tab$lx[19]<-0
  tab$lx[20]<-0
  tab$lx[21]<-0
  tab$lx[22]<-0
  tab$b<-0
  
  for(i in 7:12) {
    
    tab$b[i]<-tab$lx[i+1]/tab$lx[i]   
    
  }
  
  tab$b[6]<-0
  tab$b[6]<- tab$lx[7]/10000
  
  tab$qx<-0
  
  for(i in 7:13) {
    tab$qx[i]<-(tab$lx[i-1]-tab$lx[i])/tab$lx[i-1]
  }


#find Y(x)

  for(i in 1:22) {
    tab$Yx[i]<-(1/2)*(log(tab$qx[i]/(1-tab$qx[i])))
  }
  print(tab$Yx)



#OLS regression of Y(x) on YS(x)
tab$include<-1

  for(i in 1:7) {
    tab$include[i]<-0
  }
  for(i in 13:22){
    tab$include[i]<-0
  }
  for(i in 8:13){
    tab$include[i]<-1
  }
  out<- lm(Yx~ YSx, data=subset(tab,include==1))
  summary(out)
#intercept is the level of mortality of Panama males population in (year) is higher (slightly) than the standard. 
#The mortality schedules of Panama males population in year is more concentrated at older ages compared to the standard. 

#once we get the formula, we can get Yx for every value of YSx? Yx would be estimated values from our 
  *#regression. 
    
    #intercept: 0.5876 
    # beta: 1.7396 
# Yx = 0.5876 + 1.7396(YSx)
# Yhat 
  tab$Yhat<- 0.5876 + 1.7396*(tab$YSx)
    #now that we have our Yhat values, we can derive 

    tab$qhat <- (exp(2*tab$Yhat))/(1+exp(2*tab$Yhat))
    #then start with the first one: 
    tab$phat<- 1- tab$qhat
    tab$phat[22]<- 0
    
     tab$lxhat<-0
    tab$lxhat[1]<-10000
    for (i in 2:21){
    tab$lxhat[i]<- tab$lxhat[i-1] - (tab$qhat[i]*tab$lxhat[i-1])
    }
  
    
    library(ggplot2)
    
    plot(tab$age[7:13], tab$lx[7:13], type="l", col="blue", xlab="Age", ylab="Number alive at age x")
    
    lines(tab$age[7:13], tab$lxhat[7:13], col="green")
    
##calculalte nqx in both ways : 
    #p53 in book 
      qx <- ((10000-tab$lxhat[3]) / 10000)
      
      qxb<- (exp(2*tab$Yhat[3]))/ (1+exp(2*tab$Yhat[2]))
                
                print(qx) 
                #0.1890643 if the nqx though I wonder why not equal ? 
                print(qxb)
                print(tab$lxhat)
