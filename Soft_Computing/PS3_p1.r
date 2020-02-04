w<-0.7
c1<-0.2
c2<-0.6
n<-20
E<-100
oper<-10
r1<-runif(n)
r2<-runif(n)
CP<-list()
V<-list()
for(i in c(1:n)){
CP[[i]]<-runif(oper,min=-500,max =500)
V[[i]]<-runif(oper,min=-500,max=500)
}
sum_t<-c()
iter<-c()
pbest<-CP
pbest_fit<-c()
fitness_function<- function(n,s) 
{
  if(s=='MAX'){
    return(50-sum(n))
  }
  else if(s=='MIN'){
    if (sum(n)==0){
      return(1/(1+abs(50-sum(n))))
    }
    else{
      return(1/(abs(50-sum(n))))
      
    }
  }
  
}  
temp_cp<-list()

for(i in c(1:E)){
  r1<-runif(n)
  r2<-runif(n)
  print(r1,r2)
  F_x<-c()
  
  for(j in c(1:n)){
    
    F_x<-c(F_x,fitness_function(CP[[j]],'MIN'))
    if(i==1){
      flag<-1
      pbest[[j]]<-CP[[j]]
      pbest_fit[j]<-F_x[j]
      next
    }
    if(F_x[j]>pbest_fit[j]){
      pbest[[j]]<-CP[[j]]
      pbest_fit[j]<-F_x[j]
    }
    
  }
  
  gbest_ind<-which.max(F_x)
  print(gbest_ind)
  gbest_pos<-CP[[gbest_ind]]
  gbest_fit<-F_x[gbest_ind]
  temp_cp<-CP
  for(j in c(1:n)){
    V[[j]]<-w*V[[j]]+c1*r1[j]*(pbest[[j]]-CP[[j]])+c2*r2[j]*(gbest_pos-CP[[j]])
    CP[[j]]<-CP[[j]]+V[[j]]
    
  }
  print('Iteration')
  print(i)
  iter<-c(iter,i)
  sum_t<-c(sum_t,sum(gbest_pos))
  print(sum(gbest_pos))
  print(gbest_fit)
}
plot(iter,sum_t)