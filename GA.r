library(plotly)
n = 5
GA<-expand.grid(replicate(n, 0:1, simplify = FALSE))
select=8
selected_chromo<-GA[1:select+1,]
xl<-0
xu<-28
pc<-0.95
pm=0.05
xiter<-c()
y<-c()
fitness_function<- function(n,s) 
{
  if(s=='MAX'){
    return(n^2)
  }
  else if(s=='MIN'){
    if(n^2==0){
      return(1/(1+n^2))
      
    }
    else{
      return(1/n^2)
    }
  }
  
}  

compare_chromo<- function(r,cp,cn){
  for(i in c(1:select)){
    
    if(r<=cp[i]){
      return(cn[i])
    }
    
  }
  
  
}

for(k in c(1:100)){
  rand_num<-runif(select)
  co_rand_num<-runif(select/2)
  mut_rand_num<-runif(select)
  
  decimal<-c()
  scaled_x_value<-c()
  F_x<-c()
  P_x<-c()
  for( i in c(1:select)){
    sum<-0
    
    val=selected_chromo[i,]
    
    for(j in c(1:n)){
      sum<-sum+selected_chromo[i,j]*(2^(n-j))
      
    }
    
    decimal<-c(decimal,sum)
    x<-xl+((xu-xl)*sum)/((2^n)-1)
    scaled_x_value<-c(scaled_x_value,x)
    F_x<-c(F_x,fitness_function(x,'MAX'))
    
    
  }
  plot_ly(scaled_x_value,F_x,type='contour')
  
  cum_P_x<-c()
  sum<-0
 
  chromo_num<-c()
  selection<-c()
  for(i in c(1:select)){
    P_x<-c(P_x,F_x[i]/sum(F_x))
    sum<-sum+P_x[i]
    cum_P_x<-c(cum_P_x,sum)
    chromo_num<-c(chromo_num,i)
  }
  
  
  for(i in c(1:select)){
    
    selection<-c(selection,compare_chromo(rand_num[i],cum_P_x,chromo_num))
    
  }
  
  
  
  
  co_site<-c(4,2,1,3)
  mut_site<-c(2,1,4,5,3,2,3,2)
  counter<-1
  CO_chromo<-c()
  CO_chromo<-selected_chromo
  
  for(i in seq(1,select,2)){
    if(co_rand_num[counter]<=pc){
      cos<-co_site[counter]+1
      
      temp<-CO_chromo[selection[i],]
      
      
      for(j in c(cos:n)){
        CO_chromo[selection[i],j]<-CO_chromo[selection[i+1],j]
        CO_chromo[selection[i+1],j]<-temp[j]
      }
      
    }
    counter<-counter+1
  }
 
  mut_chromo<-CO_chromo
 
  for(i in c(1:select)){
    if(mut_rand_num[i]<=pm){
      mut_chromo[i,mut_site[i]]<-as.integer(!mut_chromo[i,mut_site[i]])
    }
    
  }
  print(k)
  print('Iteration')
  print(selected_chromo)
  print(CO_chromo)
  print(mut_chromo)
  
  print(sum(F_x)/select)
  selected_chromo<-mut_chromo
  xiter<-c(xiter,k)
  y<-c(y,sum(F_x)/select)
}
plot(xiter,y)