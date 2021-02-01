# our target distribution is F = N(0,1)
# our proposal distribution is Q(x,.) = N(x,h)
# starting value Xo = 0 and n = 1000 and h's are 1,5,10 respectively
# we have to construct a Metropolis Hasting algorithm to generate samples from F

#------------------------------------------------------------------#

#set.seed(201257) # seed set to my roll number 
N = 1e3
iid.samples<- rnorm(n = N, mean = 0, sd = 1) # iid standard normal samples

normal_den <- function(x)
{
  return(exp(-x^2/2))
}

normal_mh <- function(N = 1e3, h) # h is the proposal variance of Normal Distribution
{
  out <- numeric(length = N) 
  acc.prob <- 0  # acceptance probability
  out[1] <- 0 # starting value
  
  for(t in 2:N)
  {
    prop <- rnorm(1, mean = out[t-1], sd = sqrt(h))
    alpha <- normal_den(x = prop) / normal_den(x = out[t-1])
    U <- runif(1)
    if(U <= alpha)  # to decide whether to accept or reject
    {
      out[t] <- prop
      acc.prob <- acc.prob + 1
    } 
    else
    {
      out[t] <- out[t-1]
    }
  }
  print(acc.prob/N)
  return(out)
}


# CASE 1: h=1

#making a markov chain and obtaining samples from standard normal distn
chain <- normal_mh(N = N, h = 1)

#plotting the density of iid standard normal samples
plot(density(iid.samples),main = 'Estimated Density Curve',
     col='darkgreen')
# overlaying density of Marov Chain simulated samples
lines(density(chain),col='red') 
legend('topright',legend = c('iid samples','MC samples'),
       col = c('darkgreen','red'),lty = 1,lwd = 2,cex=.7)

#trace plot
plot.ts(iid.samples,col = 'green',ylim = c(-4,4),
        ylab = 'Sample Values')
lines(chain,col = 'red')
legend("topright", legend=c("iid samples", "MC Samples"),
       col=c("green", "red"),lty=1,lwd = 2, cex=.6)


# CASE 2: h=5

#making a markov chain and obtaining samples from standard normal distn
chain <- normal_mh(N = N, h = 5)

#plotting the density of iid standard normal samples
plot(density(iid.samples),main = 'Estimated Density Curve',
     col='darkgreen')
# overlaying density of Marov Chain simulated samples
lines(density(chain),col='red') 
legend('topright',legend = c('iid samples','MC samples'),
       col = c('darkgreen','red'),lty = 1,lwd = 2,cex=.7)

#trace plot
plot.ts(iid.samples,col = 'green',ylim = c(-4,4),
        ylab = 'Sample Values')
lines(chain,col = 'red')
legend("topright", legend=c("iid samples", "MC Samples"),
       col=c("green", "red"),lty=1,lwd = 2, cex=.6)


# CASE 3: h=10

#making a markov chain and obtaining samples from standard normal distn
chain <- normal_mh(N = N, h = 10)

#plotting the density of iid standard normal samples
plot(density(iid.samples),main = 'Estimated Density Curve',
     col='darkgreen')
# overlaying density of Marov Chain simulated samples
lines(density(chain),col='red') 
legend('topright',legend = c('iid samples','MC samples'),
       col = c('darkgreen','red'),lty = 1,lwd = 2,cex=.7)

#trace plot
plot.ts(iid.samples,col = 'green',ylim = c(-4,4),
        ylab = 'Sample Values')
lines(chain,col = 'red')
legend("topright", legend=c("iid samples", "MC Samples"),
       col=c("green", "red"),lty=1,lwd = 2, cex=.6)

##CONCLUSION for different h :
#for h=1 : acceptance probability=0.771 
#for h=5 : acceptance probability=0.452
#for h=10: acceptance probability=0.35
# the more the value of h becomes the more the markov chain is generating more diverse
# sample that is replicating better the original distribution. But with greater value
# of h, the acceptance probability is reducing but replicating a better the original distribution.
# So, h=10 seems best for trace plot.

