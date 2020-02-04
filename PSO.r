w<-0.7
c1<-0.2
c2<-0.6
n<-5
E<-2
r1<-runif(n)
r2<-runif(n)
r1<-c(0.4657,0.8956,0.3877,0.4902,0.5039)
r2<-c(0.5319,0.8185,.8331,.7677,.1708)
CP<-runif(n,min=-2,max = 4)
CP<-c(-0.3425,3.9558,-1.1228,-0.0981,-0.0385)
V<-runif(n,min=-1,max=1)
V<-c(0.0319,0.3185,0.3331,0.2677,-0.3291)
pbest<-CP
fitness_function<- function(n,s) 
{
  if(s=='MAX'){
    return(1+2*n-n^2)
  }
  else if(s=='MIN'){
    if(n^2==0){
      return(1/(1+1+2*n-n^2))
      
    }
    else{
      return(1/(1+2*n-n^2))
    }
  }
  
}  
temp_cp<-c()
print(pbest)

for(i in c(1:E)){
  F_x<-c()
  
  for(j in c(1:n)){
    
    F_x<-c(F_x,fitness_function(CP[j],'MAX'))
    print(F_x)
    if(i==1){
      flag<-1
      pbest[j]<-CP[j]
    }
    else{
      flag<-which.max(c(temp[j],F_x[j]))
      if(flag==1){
        print(flag)
        pbest[j]<-temp_cp[j]
      }
      else if(flag==2){
        
        pbest[j]<-CP[j]
       
      }
    }
    
    
    
  }
  temp<-F_x
  print(pbest)
  
  gbest_ind<-which.max(F_x)
  gbest_pos<-CP[gbest_ind]
  gbest_fit<-F_x[gbest_ind]
  temp_cp<-CP
  for(j in c(1:n)){
    V[j]<-w*V[j]+c1*r1[j]*(pbest[j]-CP[j])+c2*r2[j]*(gbest_pos-CP[j])
    CP[j]<-CP[j]+V[j]
  }
  print('Iteration')
  print(i)
  
  print(gbest_pos)
  print(gbest_fit)
}
