# #  This first version, while having cool properties (?) is not correct
# plot(c(0,1),c(0,1), t='n')
# numits = 1000 
# 
# for (N in 2:20) {
#   x=.2
#   orbit = x
#   for (i in 1:N){
#     xnext = (3.87 + .13*runif(1))*x*(1-x)
#     orbit  = c(orbit,xnext)
#     x = xnext
#   }
#   k=length(orbit)
#   for (i in 1:numits){
#     xnext = 1/N*sum((3.87 + .13*runif(N))*orbit[(k-(N-1)):k]*(1-orbit[(k-(N-1)):k]))
#     orbit  = c(orbit,xnext)
#     x = xnext
#   }
# points(orbit[N:(numits+N)],orbit[(N+1):(numits+N+1)],xlim=c(0,1),ylim=c(0,1), col=N, cex=.1+N/5)
# }

#  Test -  find stochastic period (from note)
par(pty = 's')
plot(c(0,1),c(0,1), t='n', asp=1)
numits = 20000 

P = 3 # period to look for: don't use "T" because that means "True"
N=6
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
orbit = x
for (i in 1:(N*P)){
  xnext= (base-noise + noise*runif(1))*x*(1-x)
  orbit  = c(orbit,xnext)
  x = xnext
}
for (k in (N*P):(numits+N*P)){
  S1 = seq((k-(N-1)*P),k,by = P) # which previous points to average over, part 1
  S2 = seq((k -(N*P)+1),(k-P+1),by = P)  # which points to average, part 2
  xnext = (1-gam)*(1/N)*sum((base-noise + noise*runif(N))*orbit[S1]*(1-orbit[S1]))
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

# for the time series plot
range = 3900:4000
par(pty =  'm')
plot(range,orbit[range],type ='l')

# look at  the distribution of orbit points
hist(orbit,breaks = 100, freq = F)
