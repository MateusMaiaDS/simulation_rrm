# See article: Regression Random Machines

#Simulation Scenario 1
n<-1000
model_one<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  y=x1^2 + exp(-x2^2)+rnorm(n,sd = 0.1)
  
  data<-data.frame(x1,x2,y)
  return(data)
  
}

model_two<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  x7<-til_matrix[,7]
  x8<-til_matrix[,8]
  x6<-til_matrix[,6]
  x10<-til_matrix[,10]
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<-x1*x2+x3^2-x4*x7+x8*x10-x6^2+noise
  
  data<-data.frame(x1,x2,x3,x4,x6,x7,x8,x10,y)
  return(data)      
}

model_three<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<- -sin(2*x1)+x2^2+x3-exp(-x4)+noise
  
  data<-data.frame(x1,x2,x3,x4,y)
  return(data)      
}

model_four<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<- -x1 +(2*x2-1)^2+sin(2*pi*x3)/(2-sin(2*pi*x3))+sin(2*pi*x4)+2*cos(2*pi*x4)+3*(sin(2*pi*x4))^2+4*(cos(2*pi*x4))^2 +noise
  
  data<-data.frame(x1,x2,x3,x4,y)
  return(data)      
}

model_five<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  x5<-til_matrix[,5]
  x9<-til_matrix[,9]
  x8<-til_matrix[,8]
  x6<-til_matrix[,6]
  x10<-til_matrix[,10]
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<-ifelse(x1>0,1,0)+x2^3+ifelse(x4+x6-x8-x9>1+x10,1,0)+exp(-x2^2)+noise
  
  data<-data.frame(x1,x2,x3,x4,x5,x6,x8,x9,x10,y)
  return(data)      
}

model_six<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  x6<-til_matrix[,6]
  x8<-til_matrix[,8]
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<-x1^2+(x2^2)*x3*exp(-abs(x4))+x6-x8+noise
  
  data<-data.frame(x1,x2,x3,x4,x6,x8,y)
  return(data)      
}

# Corespond to model six from the article (Befoe it was model eight/seven)
model_seven<-function(n,d,seed=NULL){
  if(!is.null(set.seed)){set.seed(seed)}
  matrix<-replicate(n = d,expr = runif(n),simplify = 'matrix')
  til_matrix<-2*(matrix-0.5)
  x1<-til_matrix[,1]
  x2<-til_matrix[,2]
  x3<-til_matrix[,3]
  x4<-til_matrix[,4]
  x6<-til_matrix[,6]
  x8<-til_matrix[,8]
  noise<-rnorm(n,mean = 0,sd=0.5)
  y<-x1 +3*x2^2 -2*exp(-x3) + x4 +noise
  
  data<-data.frame(x1,x2,x3,x4,y)
  return(data)      
}



# Correspond 
model_eigth<-function(n=100,m=1,mean=0,sd=1,seed_sim=NULL,p=0){
  #p is contamination
  if(!is.null(seed_sim)){
    set.seed(seed_sim)
  }
  
  mu<-rep(mean,6)
  cov<-diag(sd,6)
  
  #Too se the equation check "Forest-type Regression with General Losses And Robust Forest"
  X<-MASS::mvrnorm(n=n,mu = mu,Sigma = cov,empirical = TRUE)
  
  Y<-apply(X,1,function(x){
    m*(x[1]+0.707*x[2]^2+2*ifelse(x[3]>0,1,0)+0.873*log(abs(x[1]))*x[3]+0.894*x[2]*x[4]+2*ifelse(x[5]>0,1,0)+0.464*exp(x[6]))
  })
  #Adiciona-se the noise
  erro<-ifelse(runif(n)>p,rnorm(1,mean = 0,sd = 1),rnorm(1,mean = 0,sd = 5))
  Y<-Y+erro
  
  sim_data<-data.frame(X=X,y=Y)  
  
}

