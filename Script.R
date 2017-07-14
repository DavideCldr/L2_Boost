set.seed(123)

n = 100
x=runif(n)*2*pi
y=sin(x)+rnorm(n, sd=0.5)

plot(y~x)
curve(sin(x), min(x),max(x), col="red", add=T)

library(rpart)

# step 0
B = 50
nu = 0.1
d = 1

# step 1
hatf0=mean(y)

# step 2
r = y - hatf0

# step 3
hatg1 = rpart(r~x, control=rpart.control(maxdepth = d))

# step 4
hatfb = matrix(NA, nrow=n, ncol=B)
hatfb[,1] = hatf0 + nu*predict(hatg1)

# step 5
for (b in 2:B){
# step 2
r = y - hatfb[,b-1]
# step 3
hatgb = rpart(r~x, control=rpart.control(maxdepth = d))
# step 4
hatfb[,b] = hatfb[,b-1] + nu*predict(hatgb) 
}

# estimated f at step bstop
bstop = 40
curve(sin(x), min(x),max(x), col="red", ylab="y")
ix = sort(x,index.return=TRUE)$ix
lines(x[ix],hatfb[ix,bstop], type="s", col="blue")
