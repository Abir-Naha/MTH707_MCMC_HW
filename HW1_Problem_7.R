#our target distribution is F = Multivariate Normal with mean vector 0 and dispersion matrix is 100x100 identity matrix

set.seed(201257)#seed set to my roll number

mnormal_den<-function(x)
{
  return(exp(-sum(x^2)/2))
}

normal_mh<-function(N = 1e3, p = 100, h)
{
  out<-matrix(0,nrow = N,ncol = p)
  acc.prob<-0
  out[1,]<-0
  
  for (t in 2:N) 
  {
    prop<-rnorm(p, mean = out[t-1,], sd = sqrt(h))  
    
    alpha<-mnormal_den(prop)/mnormal_den(out[t-1,])
    
    u<-runif(1)
    
    if(u<=alpha)
    {
      out[t,] = prop
      acc.prob = acc.prob+1
    }
    else
    {
      out[t,] = out[t-1,]
    }
  }
  print(acc.prob/N)
  return(out)
}

chain<-normal_mh(h = .05)
plot.ts(chain[,1])