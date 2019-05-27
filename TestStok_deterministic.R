#  Test -  find stochastic period (from note)
par(pty = 's')
plot(c(0,1),c(0,1), t='n', asp=1, xlab='x_n',ylab  ='x_{n+1}')
numits = 20000 

P = 3 # period to look for: don't use "T" because that means "True"
N= 3
gam = 0.066
#gam = 0
noise = 0
#noise   = .01
#noise =  .13
base =  4
#base = 3.88
#base = 3.3

#set.seed(47)
x=runif(1)
#x = .2
orbit = c()
for (i in 1:(N*P)){
  xnext= (base-noise + noise*runif(1))*x*(1-x)
  orbit  = c(orbit,xnext)
  x = xnext
}

ajs = c()
for (j in 1:N){
  # Fejer coefficients
   ajs[j]=(2/N)*(1  -  (2*j-1)/(2*N))
  # From arxiv paper
  # ajs[j]= (2*(N-j)+1)/(N^2)
  # Dirichlet coefficients
  # ajs[j]=1/N
}

# # From "Cycle" paper
# eps2 = 1/9
# eps1 = 4/9
# ajs[3]=eps2
# ajs[2]  = eps1-eps2
# ajs[1] = 1-(ajs[2]+ajs[3])

## NOTE: we need to reverse the order of the weight vector 
## because we put the averaged vector in increasing index order
ajs = rev(ajs)

kvals = c()
for (k in (N*P):(numits+N*P)){
  #kvals  = c(kvals,k)
  S1 = seq((k-(N-1)*P),k,by = P) # which previous points to average over, part 1
  S2 = seq((k -(N*P)+1),(k-P+1),by = P)  # which points to average, part 2
  xnext = (1-gam)*sum(ajs*(base-noise + noise*runif(N))*orbit[S1]*(1-orbit[S1]))
          + gam*(1/N)*sum((base-noise + noise*runif(N))*orbit[S2]*(1-orbit[S2]))
   orbit = c(orbit,xnext)       
}
range = 1000:20000

points(orbit[range],orbit[(range+1)],xlim=c(0,1),ylim=c(0,1), col=N, cex=.1+N/30)
lines(c(0,1),c(0,1),col = 'green')
# plot a graph of the Pth iterate of the deterministic map on top
a1 = base-noise
a2 = base

x0vals  = seq(0,1,by = .01)
x1vals = x0vals
x2vals = x0vals
for (j in 1:P){
 x1vals = a1*x1vals*(1-x1vals)
 x2vals = a2*x2vals*(1-x2vals)
}
lines(x0vals,x1vals,col = 'purple')
lines(x0vals,x2vals,col = 'lavender')

# calculate period 2 orbit
h   = 4
p1 =  (1+h -   sqrt(h^2-2*h-3))/(2*h)
p2 =  (1+h +   sqrt(h^2-2*h-3))/(2*h)
# plot on  top  of graph# 
# points(c(p1,p2),c(p2,p1),pch = 16,col='red')

# for the time series plot
range = 3900:4000
par(pty =  'm')
plot(range,orbit[range],type ='b',pch=18)
#plot(range,orbit[range],type ='l')

# look at  the distribution of orbit points
hist(orbit[range],breaks = 100, freq = F)
#  plot period 2 points on horizontal axis
points(c(p1,p2),c(0,0),pch=16,col='red')
