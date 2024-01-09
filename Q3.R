################################################################################
#      Question 3: Partially Hedged Strategy and Potential Future Exposure     #
################################################################################



##################### 3.1
install.packages('pandas')

Fto = Ft[,-1]
View(Fto)
kstar = 0.0005457074
Delta = 0.25
cs = 0.0105
bas = 0.0005



nSimu = 50000
Seq = 1
Seqx = N/M

x = 0
NST = 20*nSimu
Hedge = 0

CF=0
CF = array(dim=c(nSimu))
CF = as.data.frame(CF)
View(CF)

for (u in 1:100) {
  Count = 0
  for (j in 1:20) {
    
    for (i in 1:nSimu) {
      CF = 0
      x = u*M
      
      CF = (kstar+bas)*Delta*x + cs*Delta*N  + (N-x)*Delta*Fto[i,j]
      CF
      
      
      if(CF>=65000){Count = Count +1}
      
    } 
    
    
  }
  
  if(Count <= 0.05*20*nSimu){print(x)}
  
}


##################### 3.2

Hedge_Ratio = 3300000/N
Hedge_Ratio
