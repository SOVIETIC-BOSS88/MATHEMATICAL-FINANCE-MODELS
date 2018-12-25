##Here we do our first simulations

##1.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we export our W and Z to separate txt files and we plot them
write.table(W, "c:/W.txt", sep="\t")
write.table(Z, "c:/Z.txt", sep="\t")
plot(W,type='l',xlab="time")
plot(Z,type='l',xlab="time")

##Here we save the plot images
dev.copy(jpeg,filename="W.jpg");
dev.copy(jpeg,filename="B.jpg");
dev.off ();

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##Here we export our B to a txt file and we plot it
write.table(B, "c:/B.txt", sep="\t")
plot(B,type='l',xlab="time")
dev.copy(jpeg,filename="Z.jpg");


##1.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W )
plot(S1, type='l',xlab="time")

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T)* B)
plot(S2, type='l',xlab="time")

##Here we export our S1 and S2 to separate txt files
write.table(S1, "c:/S1.txt", sep="\t")
write.table(S2, "c:/S2.txt", sep="\t")

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - matrix_K, 0)
mean(S3)
S3

################################################################################


##2)Here we replicate our process 100 times

##Here we increase total allocation limit 
memory.limit()
1535.875
memory.limit(size=4000)

##Here we replicate our process above, 100 times
X <- replicate(100,{
##Here we repeat the above process 100 times

##2.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##2.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=100;
S1 = S0_1 * exp( ( r -(sigma1^2)/2) * T + sigma1 * sqrt(T) * W )

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=100;
S2 = S0_2 * exp( ( r -(sigma2^2)/2) * T + sigma2 * sqrt(T)* B )

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - matrix_K, 0)
##S3
format(round(S3, 5), nsmall = 5)
S3mean = mean(S3)
S3mean})

X
format(round(X, 5), nsmall = 5)

histinfo<-hist(X)
hist(X, 
     main="Histogram", xlab="X", 
      border="blue", col="green", 
	xlim=c(7.24, 7.45), ylim=c(0,10),
     las=1, 
     breaks=50)

plot(X)
sd(X, na.rm = FALSE)


################################################################################


##3) Here we simulate the 1.1 and 1.2, but using different S0 values and also using the Margrabe's formula

##3.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##3.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W )

##Here we introduce the inputs for S2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - matrix_K, 0)
##S3
format(round(S3, 5), nsmall = 5)
S3mean = mean(S3)
S3mean

##3.3)Here we use the Margrabe formula when K = 0

a = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 + (sigma2)^2 )
d_plus = log(S0_1/S0_2)/a + a/2
d_minus = log(S0_1/S0_2)/a - a/2 

## Now we use this formula 110*NormalDist(d+)-90*NormalDist(d-)
N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)

S3_Margrabe = S0_1 * N_d1 - S0_2 * N_d2
S3_Margrabe


################################################################################

##4) Here we simulate the S3, but using different K values

##4.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##4.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_K0mean = mean(S3)
S3_K0mean

##Here we put K to 5 and we create a matrix
K = 5
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 5, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K5mean = mean(S3)
S3_K5mean

##Here we put K to 10 and we create a matrix
K = 10
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 10, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K10mean = mean(S3)
S3_K10mean

##Here we put K to 15 and we create a matrix
K = 15
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 15, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K15mean = mean(S3)
S3_K15mean

##Here we put K to -5 and we create a matrix
K = -5
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 5, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K_NEG5mean = mean(S3)
S3_K_NEG5mean

##Here we put K to -10 and we create a matrix
K = -10
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 10, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K_NEG10mean = mean(S3)
S3_K_NEG10mean

##Here we put K to -15 and we create a matrix
K = -15
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 15, 0)
S3 = pmax(S1 - S2 -  K, 0)
S3_K_NEG15mean = mean(S3)
S3_K_NEG15mean

S3_K0mean
S3_K5mean
S3_K10mean
S3_K15mean

S3_K_NEG5mean
S3_K_NEG10mean
S3_K_NEG15mean

MC_S0_K = matrix( 
c(S3_K0mean, S3_K5mean, S3_K10mean, S3_K15mean, 
S3_K_NEG5mean, S3_K_NEG10mean, S3_K_NEG15mean), 
nrow=7, 
ncol=1
) 

MC_S0_K


################################################################################

##5) Here we simulate the S3 but using anthitetic variables

##5.1)Here we invert our W and B values

invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##5.2)Here we simulate our spreads
##We use the two formulas respectively

##invS^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*invW)
##invS^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*invB)

##Here we introduce the inputs for invS1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_K0mean = mean(invS3)
invS3_K0mean

S3_total_mean = (S3_K0mean + invS3_K0mean) / 2
S3_total_mean


################################################################################

##6)Here we replicate 4 and 5, 100 times

X <- replicate(100, 
{
##6.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##6.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_K0mean = mean(S3)
S3_K0mean
})
X 
sd(X, na.rm = FALSE)

Y <- replicate(100, 
{
##6.1)Here we simulate our Brownians
##Here we simulate our W and our Z
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.8

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##6.2)Here we simulate our spreads
##We use the two formulas respectively

##S^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*W)
##S^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*B)

##Here we introduce the inputs for S1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_K0mean = mean(S3)
S3_K0mean

##6.3)Here we invert our W and B values

invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##6.4)Here we simulate our spreads
##We use the two formulas respectively

##invS^1(T)=S(0)exp((r - (sigma1^2)/2)T+\sigma1*arrel(T)*invW)
##invS^2(T)=S(0)exp((r - (sigma2^2)/2)T+\sigma2*arrel(T)*invB)

##Here we introduce the inputs for invS1
##Watch that we have changed S0_1 to S0_1 = 110
##inputs:
r=0; sigma1=0.3; T=1; n=5000000; S0_1=110;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##Watch that we have changed S0_2 to S0_2 = 90
##inputs:
r=0; sigma2=0.2; T=1; n=5000000; S0_2=90;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##Here we put K to 0 and we create a matrix
K = 0
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_K0mean = mean(invS3)
invS3_K0mean

S3_total_mean = (S3_K0mean + invS3_K0mean) / 2
S3_total_mean
})
Y
sd(Y, na.rm = FALSE)

################################################################################

##7)Here we will add Kirk's approximation 
##and the Modified Kirk's approximation for spread option prices

##7.1)Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, B)
##rho = 0.8
rho = 0.999

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##7.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1 =0.3; T=0.5; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2 =0.2; T=0.5; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we put K to 5 and we create a matrix
K = 5
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_K5mean = mean(S3)
S3_K5mean

## Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##Here we simulate our spreads

##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##Here we put K to 5 and we create a matrix
K = 5
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_K5mean = mean(invS3)
invS3_K5mean

S3_total_mean = (S3_K5mean + invS3_K5mean) / 2
S3_total_mean


##7.3)Here we add Kirk's approximation for spread option prices
##Here we calculate our volatility

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))

d1 = (log(S, base = exp(1)) + 1/2 * (a_kirk^2) * T) / ( a_kirk * (sqrt(T)) )
d2 = d1 - a_kirk * sqrt(T)

##Other possibility
##d1 = ( log(S, base = exp(1)) + r*T)/ (a_kirk* sqrt(T)) + ((a_kirk* sqrt(T))/2)  
##d2 = ( log(S, base = exp(1)) + r*T)/ (a_kirk* sqrt(T)) - ((a_kirk* sqrt(T))/2)  

##Here we use the above calculations to approximate the call spread using Kirk's formula

N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)

C_kirk = (exp(-r*T)) * ( (S0_1*N_d1) - ((S0_2 + K) * N_d2) )
C_kirk


##7.4)Here we add Modified Kirk's approximation for spread option prices

##Here we calculate our volatility

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

X_t = log(S0_1, base = exp(1))
x_aster = log((S0_2 + K), base = exp(1))

I_t = sqrt(a_kirk^2) + 1/2 * (( (sigma2 * S0_2/(S0_2 + K)) - rho* sigma1)^2 ) * ( 1 / ((sqrt(a_kirk^2))^3) ) *
			 (sigma2^2) * ( (S0_2 * K) / ((S0_2 + K)^2) ) * (X_t - x_aster)


##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))
d1_modif = (log(S, base = exp(1)) + 1/2 * (I_t^2) * T) / (I_t * (sqrt(T)) )
d2_modif = d1_modif - I_t * sqrt(T)


##Here we use the above calculations to approximate the call spread using Modified Kirk's formula
N_d1_modif = pnorm(d1_modif,mean=0,sd=1)
N_d2_modif = pnorm(d2_modif,mean=0,sd=1)

C_kirk_modif = (exp(-r*T)) * ( (S0_1*N_d1_modif) - ((S0_2 + K) * N_d2_modif) )
C_kirk_modif


S3_total_mean
C_kirk
C_kirk_modif

##7.5)Here we calculate the errors of the Kirk and Modified Kirk's formulas

error_Kirk = ((C_kirk * 100) / S3_total_mean) - 100
error_Kirk

error_Kirk_modif = ((C_kirk_modif * 100) / S3_total_mean) - 100
error_Kirk_modif


################################################################################

##8)Here we will add Kirk's approximation 
##and the Modified Kirk's approximation for spread option prices, but with a different K

##8.1)Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
##rho = 0.8
rho = 0.999

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##8.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here put K to 10 and we create a matrix
K = 10
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_K10mean = mean(S3)
S3_K10mean

## Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##Here we simulate our spreads
##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##Here we put K to 10 and we create a matrix
K = 10
matrix_K =  matrix( rep(K, len=1), nrow = (5000000))    

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_K10mean = mean(invS3)
invS3_K10mean

S3_total_mean = (S3_K10mean + invS3_K10mean) / 2
S3_total_mean


##8.3)Here we add Kirk's approximation for spread option prices
##Here we calculate our volatility

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))

d1 = (log(S, base = exp(1)) + 1/2 * (a_kirk^2) * T) / ( a_kirk * (sqrt(T)) )
d2 = d1 - a_kirk * sqrt(T)

##Other possibility
##d1 = ( log(S, base = exp(1)) + r*T)/ (a_kirk* sqrt(T)) + ((a_kirk* sqrt(T))/2)  
##d2 = ( log(S, base = exp(1)) + r*T)/ (a_kirk* sqrt(T)) - ((a_kirk* sqrt(T))/2)  

##Here we use the above calculations to approximate the call spread using Kirk's formula

N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)

C_kirk = (exp(-r*T)) * ( (S0_1*N_d1) - ((S0_2 + K) * N_d2) )
C_kirk


##8.4)Here we add Modified Kirk's approximation for spread option prices

##Here we calculate our volatility

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

X_t = log(S0_1, base = exp(1))
x_aster = log((S0_2 + K), base = exp(1))

I_t = sqrt(a_kirk^2) + 1/2 * (( (sigma2 * S0_2/(S0_2 + K)) - rho* sigma1)^2 ) * ( 1 / ((sqrt(a_kirk^2))^3) ) *
			 (sigma2^2) * ( (S0_2 * K) / ((S0_2 + K)^2) ) * (X_t - x_aster)

S = (S0_1 / (S0_2 + K))
d1_modif = (log(S, base = exp(1)) + 1/2 * (I_t^2) * T) / (I_t * (sqrt(T)) )
d2_modif = d1_modif - I_t * sqrt(T)

N_d1_modif = pnorm(d1_modif,mean=0,sd=1)
N_d2_modif = pnorm(d2_modif,mean=0,sd=1)

C_kirk_modif = (exp(-r*T)) * ( (S0_1*N_d1_modif) - ((S0_2 + K) * N_d2_modif) )
C_kirk_modif


S3_total_mean
C_kirk
C_kirk_modif

##8.5) Here we calculate the errors of the Kirk and modified Kirk's formulas

error_Kirk = ((C_kirk * 100) / S3_total_mean) - 100
error_Kirk

error_Kirk_modif = ((C_kirk_modif * 100) / S3_total_mean) - 100
error_Kirk_modif

################################################################################

##9)Here we will add Kirk's approximation 
##and the Modified Kirk's approximation for spread option prices, but with a different K and rho

##9.1)Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho = 0.999

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##9.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1 =0.3; T=0.5; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2 =0.2; T=0.5; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

## Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##Here we simulate our spreads
##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)


##9.3)Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K_rho_0.999 <- matrix(c(0:20), nrow = 21, ncol = 1)

##Here we create a function that will evaluate our formula
function_S3_rho_0.999<-function(K){

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_mean = mean(S3)
S3_mean

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_mean = mean(invS3)
invS3_mean

S3_total_mean = (S3_mean + invS3_mean) / 2
S3_total_mean
} 

##Here we apply the matrix_K_rho_0.999 values to our function_S3_rho_0.999 to be evaluated
results_S3_rho_0.999 = apply(matrix_K_rho_0.999, 1, function(x) function_S3_rho_0.999(x) )


##9.4)Here we add Kirk's approximation for spread option prices

##Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K_rho_0.999 <- matrix(c(0:20), nrow = 21, ncol = 1)

##Here we create a function that will evaluate our formula

function_Kirk_rho_0.999<-function(K){
##Here we calculate our volatility
a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))

d1 = (log(S, base = exp(1)) + 1/2 * (a_kirk^2) * T) / ( a_kirk * (sqrt(T)) )
d2 = d1 - a_kirk * sqrt(T)

##Here we use the above calculations to approximate the call spread  using Kirk's formula

N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)

C_kirk = (exp(-r*T)) * ( (S0_1*N_d1) - ((S0_2 + K) * N_d2) )
C_kirk
} 

##Here we apply the matrix_K_rho_0.999 values to our function_Kirk_rho_0.999 to be evaluated
results_Kirk_rho_0.999 = apply(matrix_K_rho_0.999, 1, function(x) function_Kirk_rho_0.999(x) )


##9.5)Here we add Modified Kirk's approximation for spread option prices

##Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K_rho_0.999 <- matrix(c(0:20), nrow = 21, ncol = 1)

##Here we create a function that will evaluate our formula

function_Kirk_modif_rho_0.999<-function(K){

##Here we calculate our volatility

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

X_t = log(S0_1, base = exp(1))
x_aster = log((S0_2 + K), base = exp(1))

I_t = sqrt(a_kirk^2) + 1/2 * (( (sigma2 * S0_2/(S0_2 + K)) - rho* sigma1)^2 ) * ( 1 / ((sqrt(a_kirk^2))^3) ) *
			 (sigma2^2) * ( (S0_2 * K) / ((S0_2 + K)^2) ) * (X_t - x_aster)

S = (S0_1 / (S0_2 + K))
d1_modif = (log(S, base = exp(1)) + 1/2 * (I_t^2) * T) / (I_t * (sqrt(T)) )
d2_modif = d1_modif - I_t * sqrt(T)

N_d1_modif = pnorm(d1_modif,mean=0,sd=1)
N_d2_modif = pnorm(d2_modif,mean=0,sd=1)

C_kirk_modif = (exp(-r*T)) * ( (S0_1*N_d1_modif) - ((S0_2 + K) * N_d2_modif) )
C_kirk_modif
} 

##Here we apply the matrix_K_rho_0.999 values to our function_Kirk_rho_0.999 to be evaluated
results_Kirk_modif_rho_0.999 = apply(matrix_K_rho_0.999, 1, function(x) function_Kirk_modif_rho_0.999(x) )

results_S3_rho_0.999 
results_Kirk_rho_0.999 
results_Kirk_modif_rho_0.999 


##9.6) Here we calculate the errors of the Kirk and modified Kirk's formulas

error_Kirk_rho_0.999 = ((results_Kirk_rho_0.999 * 100) / results_S3_rho_0.999) - 100
error_Kirk_rho_0.999

error_Kirk_modif_rho_0.999 = ((results_Kirk_modif_rho_0.999 * 100) / results_S3_rho_0.999) - 100
error_Kirk_modif_rho_0.999


################################################################################

##10)Here we will add Kirk's approximation 
##and the Modified Kirk's approximation for spread option prices, but with a different K and rho

##10.1)Here we create a matrix for rho, with 21 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)

##Here we create a function that will evaluate our formula

function_S3_K_rho<-function(rho){
##Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
##rho = 0.999

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##10.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

## Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##Here we simulate our spreads
##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##10.3)Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K <- matrix(c(0:20), nrow = 21, ncol = 1)

##Here we create a function that will evaluate our formula
function_S3_K <- function(K){

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_mean = mean(S3)
S3_mean

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_mean = mean(invS3)
invS3_mean

S3_total_mean = (S3_mean + invS3_mean) / 2
S3_total_mean
} 

##Here we apply the matrix_K_rho values to our function_total_S3 to be evaluated
results_function_S3_K = apply(matrix_K, 1, function(x) function_S3_K(x) )
}
results_function_S3_K_rho = apply(matrix_rho, 1, function(x) function_S3_K_rho(x) )
results_function_S3_K_rho


##10.4)Here we add Kirk's approximation for spread option prices
##Here we create a matrix for rho, with 21 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
##Here we create a function that will evaluate our formula
function_Kirk_K_rho<-function(rho){
##Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K <- matrix(c(0:20), nrow = 21, ncol = 1)
##Here we create a function that will evaluate our formula
function_Kirk_K<-function(K){

##Here we calculate our volatility
##inputs:
r=0; sigma1=0.3; T=0.5; S0_1=100;
r=0; sigma2=0.2; T=0.5; S0_2=100;

a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))
d1 = (log(S, base = exp(1)) + 1/2 * (a_kirk^2) * T) / ( a_kirk * (sqrt(T)) )
d2 = d1 - a_kirk * sqrt(T)

##Here we use the above calculations to approximate the call spread using Kirk's formula
N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)

C_kirk = (exp(-r*T)) * ( (S0_1*N_d1) - ((S0_2 + K) * N_d2) )
C_kirk
} 

##Here we apply the matrix_K_rho to our function_Kirk to be evaluated
results_function_Kirk_K = apply(matrix_K, 1, function(x) function_Kirk_K(x) )
}
results_function_Kirk_K_rho = apply(matrix_rho, 1, function(x) function_Kirk_K_rho(x) )
results_function_Kirk_K_rho


##10.5)Here we add Modified Kirk's approximation for spread option prices
##Here we create a matrix for rho, with 21 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)

##Here we create a function that will evaluate our formula
function_Kirk_modif_K_rho<-function(rho){

##Here we create a matrix for K, with 21 rows, 1 column and values from 0 to 20
matrix_K <- matrix(c(0:20), nrow = 21, ncol = 1)

##Here we create a function that will evaluate our formula
function_Kirk_modif_K <- function(K){

##Here we calculate our volatility
##inputs:
r=0; sigma1=0.3; T=0.5; S0_1=100;
r=0; sigma2=0.2; T=0.5; S0_2=100;
a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

X_t = log(S0_1, base = exp(1))
x_aster = log((S0_2 + K), base = exp(1))

I_t = sqrt(a_kirk^2) + 1/2 * (( (sigma2 * S0_2/(S0_2 + K)) - rho* sigma1)^2 ) * ( 1 / ((sqrt(a_kirk^2))^3) ) *
			 (sigma2^2) * ( (S0_2 * K) / ((S0_2 + K)^2) ) * (X_t - x_aster)

S = (S0_1 / (S0_2 + K))
d1_modif = (log(S, base = exp(1)) + 1/2 * (I_t^2) * T) / (I_t * (sqrt(T)) )
d2_modif = d1_modif - I_t * sqrt(T)

N_d1_modif = pnorm(d1_modif,mean=0,sd=1)
N_d2_modif = pnorm(d2_modif,mean=0,sd=1)

C_kirk_modif = (exp(-r*T)) * ( (S0_1*N_d1_modif) - ((S0_2 + K) * N_d2_modif) )
C_kirk_modif
} 
##Here we apply the matrix_K_rho values to our function_Kirk_modif to be evaluated
results_Kirk_modif_K = apply(matrix_K, 1, function(x) function_Kirk_modif_K(x) )
}
results_function_Kirk_modif_K_rho = apply(matrix_rho, 1, function(x) function_Kirk_modif_K_rho(x) )
results_function_Kirk_modif_K_rho

results_function_S3_K_rho
results_function_Kirk_K_rho
results_function_Kirk_modif_K_rho


##Here we calculate the error produced by the Kirk formula for each rho
error_Kirk_K_rho = ((results_function_Kirk_K_rho * 100) / results_function_S3_K_rho) - 100
colnames(error_Kirk_K_rho) <- c('rho_0.8', 'rho_0.85','rho_0.9','rho_0.95', 'rho_0.999')
rownames(error_Kirk_K_rho) <- c('K0', 'K1', 'K2','K3','K4','K5','K6','K7','K8','K9',
'K10','K11','K12','K13','K14','K15','K16','K17','K18','K19','K20')
error_Kirk_K_rho

persp(error_Kirk_K_rho, theta = 40)

##Here we set and retrieve the current directory 
setwd("~/")
getwd()

##Here we save the plot images for the Kirk formula
plot(error_Kirk_K_rho[,'rho_0.8'], type= 'l', main = 'error_Kirk_K_rho_0.8', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_rho_0.8.pdf');

plot(error_Kirk_K_rho[,'rho_0.85'], type= 'l', main = 'error_Kirk_K_rho_0.85', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_K_rho_0.85.pdf');

plot(error_Kirk_K_rho[,'rho_0.9'], type= 'l', main = 'error_Kirk_K_rho_0.9', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_K_rho_0.9.pdf');

plot(error_Kirk_K_rho[,'rho_0.95'], type= 'l', main = 'error_Kirk_K_rho_0.95', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_K_rho_0.95.pdf');

plot(error_Kirk_K_rho[,'rho_0.999'], type= 'l', main = 'error_Kirk_K_rho_0.999', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_K_rho_0.999.pdf');
dev.off ();


##Here we calculate the error produced by the modified Kirk formula for each rho
error_Kirk_modif_K_rho = ((results_function_Kirk_modif_K_rho * 100) /  results_function_S3_K_rho) - 100
colnames(error_Kirk_modif_K_rho) <- c('rho_0.8', 'rho_0.85','rho_0.9','rho_0.95','rho_0.999')
rownames(error_Kirk_modif_K_rho) <- c('K0', 'K1', 'K2','K3','K4','K5','K6','K7','K8','K9',
'K10','K11','K12','K13','K14','K15','K16','K17','K18','K19','K20')
error_Kirk_modif_K_rho

##Here we save the plot images for the modified Kirk formula
plot(error_Kirk_modif_K_rho[,'rho_0.8'], type= 'l', main = 'error_Kirk_modif_K_rho_0.8', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_modif_K_rho_0.8.pdf');

plot(error_Kirk_modif_K_rho[,'rho_0.85'], type= 'l', main = 'error_Kirk_modif_K_rho_0.85', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_modif_K_rho_0.85.pdf');

plot(error_Kirk_modif_K_rho[,'rho_0.9'], type= 'l', main = 'error_Kirk_modif_K_rho_0.9', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_modif_K_rho_0.9.pdf');

plot(error_Kirk_modif_K_rho[,'rho_0.95'], type= 'l', main = 'error_Kirk_modif_K_rho_0.95', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_modif_K_rho_0.95.pdf');

plot(error_Kirk_modif_K_rho[,'rho_0.999'], type= 'l', main = 'error_Kirk_modif_K_rho_0.999', xlab="K", ylab='errors')
dev.print(pdf, 'error_Kirk_modif_K_rho_0.999.pdf');
dev.off ();


##Here we superpose the plot images and we save them
plot(error_Kirk_K_rho[,'rho_0.8'],type="l", main = 'error_Kirk_K_rho_0.8_and_error_Kirk_modif_K_rho_0.8', 
xlab="K", ylab='errors', col="red", ylim=c(-0.1, 0.5))
lines(error_Kirk_modif_K_rho[,'rho_0.8'],col="green", ylim=c(-0.5, 0.5))
legend('topright', legend=c("Kirk errors", "Modified Kirk errors"), col=c("red", "green"), lty=1, cex=0.8)
dev.print(pdf, 'error_Kirk_K_rho_and_error_Kirk_modif_K_rho_0.8.pdf');

plot(error_Kirk_K_rho[,'rho_0.85'],type="l", main = 'error_Kirk_K_rho_0.85_and_error_Kirk_modif_K_rho_0.85', 
xlab="K", ylab='errors', col="red", ylim=c(-0.2, 0.8))
lines(error_Kirk_modif_K_rho[,'rho_0.85'],col="green", ylim=c(-0.2, 0.8))
legend('topright', legend=c("Kirk errors", "Modified Kirk errors"), col=c("red", "green"), lty=1, cex=0.8)
dev.print(pdf, 'error_Kirk_K_rho_and_error_Kirk_modif_K_rho_0.85.pdf');

plot(error_Kirk_K_rho[,'rho_0.9'],type="l", main = 'error_Kirk_K_rho_0.9_and_error_Kirk_modif_K_rho_0.9', 
xlab="K", ylab='errors', col="red", ylim=c(-0.2, 0.7))
lines(error_Kirk_modif_K_rho[,'rho_0.9'],col="green", ylim=c(-0.2, 0.7))
legend('topright', legend=c("Kirk errors", "Modified Kirk errors"), col=c("red", "green"), lty=1, cex=0.8)
dev.print(pdf, 'error_Kirk_K_rho_and_error_Kirk_modif_K_rho_0.9.pdf');

plot(error_Kirk_K_rho[,'rho_0.95'],type="l", main = 'error_Kirk_K_rho_0.95_and_error_Kirk_modif_K_rho_0.95', 
xlab="K", ylab='errors', col="red", ylim=c(-0.4, 7))
lines(error_Kirk_modif_K_rho[,'rho_0.95'],col="green", ylim=c(-0.4, 7))
legend('topright', legend=c("Kirk errors", "Modified Kirk errors"), col=c("red", "green"), lty=1, cex=0.8)
dev.print(pdf, 'error_Kirk_K_rho_and_error_Kirk_modif_K_rho_0.95.pdf');

plot(error_Kirk_K_rho[,'rho_0.999'],type="l", main = 'error_Kirk_K_rho_0.999_and_error_Kirk_modif_K_rho_0.999', 
xlab="K", ylab='errors', col="red", ylim=c(-0.2, 5))
lines(error_Kirk_modif_K_rho[,'rho_0.999'],col="green", ylim=c(-0.2, 5))
legend('topright', legend=c("Kirk errors", "Modified Kirk errors"), col=c("red", "green"), lty=1, cex=0.8)
dev.print(pdf, 'error_Kirk_K_rho_and_error_Kirk_modif_K_rho_0.999.pdf');
dev.off ();

################################################################################


##11)Here we will calculate the intervals of confidence for K = 5 and K = 10,
##and also for rho = 0,9 and rho = 0,999 for our 5000000 simulations

##11.1)Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
rho_0.9 = 0.9
rho_0.999 = 0.999

##Here we calculate B, so W and B can have same correlation
B_rho_0.9 = (rho_0.9 * W + (sqrt(1-(rho_0.9^2))) * Z)
B_rho_0.999 = (rho_0.999 * W + (sqrt(1-(rho_0.999^2))) * Z)

##11.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)

##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
S2_B_rho_0.9 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B_rho_0.9)
S2_B_rho_0.999 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B_rho_0.999)


##11.3) Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB_rho_0.9 = (rho_0.9 * invW + (sqrt(1-(rho_0.9^2))) * invZ)
invB_rho_0.999 = (rho_0.999 * invW + (sqrt(1-(rho_0.999^2))) * invZ)


##Here we simulate our spreads
##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.3; T=0.5; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.2; T=0.5; n=5000000; S0_2=100;
invS2_invB_rho_0.9 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB_rho_0.9)
invS2_invB_rho_0.999 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB_rho_0.999)

K_5 = 5
K_10 = 10

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3_B_rho_0.9_K_5 = pmax(S1 - S2_B_rho_0.9 - K_5, 0)
invS3_invB_rho_0.9_K_5 = pmax(invS1 - invS2_invB_rho_0.9 - K_5, 0)

##Here we create two matrixes from the S3 and invS3 data frames, so we will be able to operate on the cells individually
mat1_S3_B_rho_0.9_K_5 = data.matrix(S3_B_rho_0.9_K_5, rownames.force = NA)
mat2_invS3_invB_rho_0.9_K_5 = data.matrix(invS3_invB_rho_0.9_K_5, rownames.force = NA)
mat3_rho_0.9_K_5 = (mat1_S3_B_rho_0.9_K_5 + mat2_invS3_invB_rho_0.9_K_5)/2

##Here we calculate the mean and the standard deviation
mean_mat3_rho_0.9_K_5 =  mean(mat3_rho_0.9_K_5)
std_dev_mat3_rho_0.9_K_5 = sd(mat3_rho_0.9_K_5)

##Now we calculate thir confidence intervals
conf_inter_rho_0.9_K_5_minus = mean_mat3_rho_0.9_K_5 - 1.96 * (std_dev_mat3_rho_0.9_K_5)/sqrt(n)
conf_inter_rho_0.9_K_5_minus
conf_inter_rho_0.9_K_5_plus = mean_mat3_rho_0.9_K_5 + 1.96 * (std_dev_mat3_rho_0.9_K_5)/sqrt(n)
conf_inter_rho_0.9_K_5_plus


##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3_B_rho_0.9_K_10 = pmax(S1 - S2_B_rho_0.9 - K_10, 0)
invS3_invB_rho_0.9_K_10 = pmax(invS1 - invS2_invB_rho_0.9 - K_10, 0)

##Here we create two matrixes from the S3 and invS3 data frames, so we will be able to operate on the cells individually
mat1_S3_B_rho_0.9_K_10 = data.matrix(S3_B_rho_0.9_K_10, rownames.force = NA)
mat2_invS3_invB_rho_0.9_K_10 = data.matrix(invS3_invB_rho_0.9_K_10, rownames.force = NA)
mat3_rho_0.9_K_10 = (mat1_S3_B_rho_0.9_K_10 + mat2_invS3_invB_rho_0.9_K_10)/2

##Here we calculate the mean and the standard deviation
mean_mat3_rho_0.9_K_10 =  mean(mat3_rho_0.9_K_10)
std_dev_mat3_rho_0.9_K_10 = sd(mat3_rho_0.9_K_10)

##Now we calculate thir confidence intervals
conf_inter_rho_0.9_K_10_minus = mean_mat3_rho_0.9_K_10 - 1.96 * (std_dev_mat3_rho_0.9_K_10)/sqrt(n)
conf_inter_rho_0.9_K_10_minus
conf_inter_rho_0.9_K_10_plus = mean_mat3_rho_0.9_K_10 + 1.96 * (std_dev_mat3_rho_0.9_K_10)/sqrt(n)
conf_inter_rho_0.9_K_10_plus


##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3_B_rho_0.999_K_5 = pmax(S1 - S2_B_rho_0.999 - K_5, 0)
invS3_invB_rho_0.999_K_5 = pmax(invS1 - invS2_invB_rho_0.999 - K_5, 0)

##Here we create two matrixes from the S3 and invS3 data frames, so we will be able to operate on the cells individually
mat1_S3_B_rho_0.999_K_5 = data.matrix(S3_B_rho_0.999_K_5, rownames.force = NA)
mat2_invS3_invB_rho_0.999_K_5 = data.matrix(invS3_invB_rho_0.999_K_5, rownames.force = NA)
mat3_rho_0.999_K_5 = (mat1_S3_B_rho_0.999_K_5 + mat2_invS3_invB_rho_0.999_K_5)/2

##Here we calculate the mean and the standard deviation
mean_mat3_rho_0.999_K_5 =  mean(mat3_rho_0.999_K_5)
std_dev_mat3_rho_0.999_K_5 = sd(mat3_rho_0.999_K_5)

##Now we calculate thir confidence intervals
conf_inter_rho_0.999_K_5_minus = mean_mat3_rho_0.999_K_5 - 1.96 * (std_dev_mat3_rho_0.999_K_5)/sqrt(n)
conf_inter_rho_0.999_K_5_minus
conf_inter_rho_0.999_K_5_plus = mean_mat3_rho_0.999_K_5 + 1.96 * (std_dev_mat3_rho_0.999_K_5)/sqrt(n)
conf_inter_rho_0.999_K_5_plus


##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3_B_rho_0.999_K_10 = pmax(S1 - S2_B_rho_0.999 - K_10, 0)
invS3_invB_rho_0.999_K_10 = pmax(invS1 - invS2_invB_rho_0.999 - K_10, 0)

##Here we create two matrixes from the S3 and invS3 data frames, so we will be able to operate on the cells individually
mat1_S3_B_rho_0.999_K_10 = data.matrix(S3_B_rho_0.999_K_10, rownames.force = NA)
mat2_invS3_invB_rho_0.999_K_10 = data.matrix(invS3_invB_rho_0.999_K_10, rownames.force = NA)
mat3_rho_0.999_K_10 = (mat1_S3_B_rho_0.999_K_10 + mat2_invS3_invB_rho_0.999_K_10)/2

##Here we calculate the mean and the standard deviation
mean_mat3_rho_0.999_K_10 =  mean(mat3_rho_0.999_K_10)
std_dev_mat3_rho_0.999_K_10 = sd(mat3_rho_0.999_K_10)

##Now we calculate thir confidence intervals
conf_inter_rho_0.999_K_10_minus = mean_mat3_rho_0.999_K_10 - 1.96 * (std_dev_mat3_rho_0.999_K_10)/sqrt(n)
conf_inter_rho_0.999_K_10_minus
conf_inter_rho_0.999_K_10_plus = mean_mat3_rho_0.999_K_10 + 1.96 * (std_dev_mat3_rho_0.999_K_10)/sqrt(n)
conf_inter_rho_0.999_K_10_plus


################################################################################


##12)Here we will add Kirk's approximation 
##and the Modified Kirk's approximation for spread option prices, but with a different K, rho and T

##12.1)Here we create a matrix for T, with 5 columns, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_T <-matrix(c(0.1, 0.2, 0.3, 0.4, 0.5), nrow = 5, ncol = 1)
function_S3_K_rho_T<-function(T){

##Here we create a matrix for rho, with 5 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)

##Here we create a function that will evaluate our formula
function_S3_K_rho<-function(rho){
##Here we simulate our Brownians
n = (5000000)
W = rnorm(n,mean=0,sd=1)
Z = rnorm(n,mean=0,sd=1)

##Here we find the correlation between W and Z
##rho = cor(W, Z)
##rho = 0.999

##Here we calculate B, so W and B can have same correlation
B = (rho * W + (sqrt(1-(rho^2))) * Z)

##12.2)Here we simulate our spreads

##Here we introduce the inputs for S1
##inputs:
r=0; sigma1=0.6; n=5000000; S0_1=100;
S1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * W)
##Here we introduce the inputs for S2
##inputs:
r=0; sigma2=0.5; n=5000000; S0_2=100;
S2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * B)

##Here we invert our W and B values
invW = W * (-1)
invZ = Z * (-1)
invB = (rho * invW + (sqrt(1-(rho^2))) * invZ)

##Here we simulate our spreads
##Here we introduce the inputs for invS1
##inputs:
r=0; sigma1=0.6; n=5000000; S0_1=100;
invS1 = S0_1 * exp((r -(sigma1^2)/2)  * T + sigma1 * sqrt(T) * invW)

##Here we introduce the inputs for invS2
##inputs:
r=0; sigma2=0.5; n=5000000; S0_2=100;
invS2 = S0_2 * exp((r -(sigma2^2)/2) * T + sigma2 * sqrt(T) * invB)

##12.3)Here we create a matrix for K, with 11 rows, 1 column and values from 0 to 11
matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)

##Here we create a function that will evaluate our formula
function_S3_K <- function(K){

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
S3 = pmax(S1 - S2 - K, 0)
S3_mean = mean(S3)
S3_mean

##Here we calculate max(S^1(T) - S^2(T) - 0, 0)
invS3 = pmax(invS1 - invS2 - K, 0)
invS3_mean = mean(invS3)
invS3_mean
S3_total_mean = (S3_mean + invS3_mean) / 2
S3_total_mean
} 
##Here we apply the matrix_K_rho values to our function_total_S3 to be evaluated
results_function_S3_K = apply(matrix_K, 1, function(x) function_S3_K(x) )
}
results_function_S3_K_rho = apply(matrix_rho, 1, function(x) function_S3_K_rho(x) )
results_function_S3_K_rho
}
results_function_S3_K_rho_T = apply(matrix_T, 1, function(x) function_S3_K_rho_T(x) )
results_function_S3_K_rho_T


##12.4)Here we add Kirk's approximation for spread option prices
##Here we create a matrix for T, with 5 columns, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_T <-matrix(c(0.1, 0.2, 0.3, 0.4, 0.5), nrow = 5, ncol = 1)
function_Kirk_K_rho_T<-function(T){

##Here we create a matrix for rho, with 5 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
##Here we create a function that will evaluate our formula
function_Kirk_K_rho<-function(rho){

##Here we create a matrix for K, with 11 rows, 1 column and values from 0 to 11
matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
##Here we create a function that will evaluate our formula
function_Kirk_K<-function(K){

##Here we calculate our volatility
##inputs:
r=0; sigma1=0.6; S0_1=100;
r=0; sigma2=0.5; S0_2=100;
a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

##Here we calculate our d1 and d2
S = (S0_1 / (S0_2 + K))
d1 = (log(S, base = exp(1)) + 1/2 * (a_kirk^2) * T) / ( a_kirk * (sqrt(T)) )
d2 = d1 - a_kirk * sqrt(T)

##Here we use the above calculations to approximate the call spread using Kirk's formula
N_d1 = pnorm(d1,mean=0,sd=1)
N_d2 = pnorm(d2,mean=0,sd=1)
C_kirk = (exp(-r*T)) * ( (S0_1*N_d1) - ((S0_2 + K) * N_d2) )
C_kirk
}
##Here we apply the matrix_K_rho to our function_Kirk to be evaluated
results_function_Kirk_K = apply(matrix_K, 1, function(x) function_Kirk_K(x) )
}
results_function_Kirk_K_rho = apply(matrix_rho, 1, function(x) function_Kirk_K_rho(x) )
results_function_Kirk_K_rho
}
results_function_Kirk_K_rho_T = apply(matrix_T, 1, function(x) function_Kirk_K_rho_T(x) )
results_function_Kirk_K_rho_T


##12.5)Here we add Modified Kirk's approximation for spread option prices
##Here we create a matrix for T, with 5 columns, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_T <-matrix(c(0.1, 0.2, 0.3, 0.4, 0.5), nrow = 5, ncol = 1)
function_Kirk_modif_K_rho_T <- function(T){

##Here we create a matrix for rho, with 5 rows, 1 column and 0.80, 0.85, 0.90, 0.95, 0.999 values
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)

##Here we create a function that will evaluate our formula
function_Kirk_modif_K_rho<-function(rho){

##Here we create a matrix for K, with 11 rows, 1 column and values from 0 to 11
matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)

##Here we create a function that will evaluate our formula
function_Kirk_modif_K <- function(K){

##Here we calculate our volatility
##inputs:
r=0; sigma1=0.6; S0_1=100;
r=0; sigma2=0.5; S0_2=100;
a_kirk = sqrt( (sigma1)^2 - 2*rho*sigma1*sigma2 * (S0_2/(S0_2 + K)) + (sigma2)^2 * ((S0_2/(S0_2 + K))^2) )

X_t = log(S0_1, base = exp(1))
x_aster = log((S0_2 + K), base = exp(1))

I_t = sqrt(a_kirk^2) + 1/2 * (( (sigma2 * S0_2/(S0_2 + K)) - rho* sigma1)^2 ) * ( 1 / ((sqrt(a_kirk^2))^3) ) *
			 (sigma2^2) * ( (S0_2 * K) / ((S0_2 + K)^2) ) * (X_t - x_aster)

S = (S0_1 / (S0_2 + K))
d1_modif = (log(S, base = exp(1)) + 1/2 * (I_t^2) * T) / (I_t * (sqrt(T)) )
d2_modif = d1_modif - I_t * sqrt(T)

N_d1_modif = pnorm(d1_modif,mean=0,sd=1)
N_d2_modif = pnorm(d2_modif,mean=0,sd=1)

C_kirk_modif = (exp(-r*T)) * ( (S0_1*N_d1_modif) - ((S0_2 + K) * N_d2_modif) )
C_kirk_modif
} 
##Here we apply the matrix_K_rho values to our function_Kirk_modif to be evaluated
results_Kirk_modif_K = apply(matrix_K, 1, function(x) function_Kirk_modif_K(x) )
}
results_function_Kirk_modif_K_rho = apply(matrix_rho, 1, function(x) function_Kirk_modif_K_rho(x) )
results_function_Kirk_modif_K_rho
}
results_function_Kirk_modif_K_rho_T = apply(matrix_T, 1, function(x) function_Kirk_modif_K_rho_T(x) )
results_function_Kirk_modif_K_rho_T


results_function_S3_K_rho_T
results_function_Kirk_K_rho_T
results_function_Kirk_modif_K_rho_T


##Here we calculate the error produced by the Kirk formula for each rho and T
error_Kirk_K_rho_T = ((results_function_Kirk_K_rho_T * 100) / results_function_S3_K_rho_T) - 100
colnames(error_Kirk_K_rho_T) <- c('T_0.1', 'T_0.2','T_0.3','T_0.4', 'T_0.5')
error_Kirk_K_rho_T

##Here we organize our T matrixes for the Kirk formula, 
##with K being the rows and rho being the columns
error_Kirk_K_rho_0.80_T_0.1 = data.matrix(error_Kirk_K_rho_T[1:11, 1], rownames.force = NA)
error_Kirk_K_rho_0.85_T_0.1 = data.matrix(error_Kirk_K_rho_T[12:22, 1], rownames.force = NA)
error_Kirk_K_rho_0.90_T_0.1 = data.matrix(error_Kirk_K_rho_T[23:33, 1], rownames.force = NA)
error_Kirk_K_rho_0.95_T_0.1 = data.matrix(error_Kirk_K_rho_T[34:44, 1], rownames.force = NA)
error_Kirk_K_rho_0.999_T_0.1 = data.matrix(error_Kirk_K_rho_T[45:55, 1], rownames.force = NA)

error_Kirk_K_rho_T_0.1 = cbind(error_Kirk_K_rho_0.80_T_0.1, error_Kirk_K_rho_0.85_T_0.1, 
error_Kirk_K_rho_0.90_T_0.1, error_Kirk_K_rho_0.95_T_0.1, error_Kirk_K_rho_0.999_T_0.1)
error_Kirk_K_rho_T_0.1

error_Kirk_K_rho_0.80_T_0.2 = data.matrix(error_Kirk_K_rho_T[1:11, 2], rownames.force = NA)
error_Kirk_K_rho_0.85_T_0.2 = data.matrix(error_Kirk_K_rho_T[12:22, 2], rownames.force = NA)
error_Kirk_K_rho_0.90_T_0.2 = data.matrix(error_Kirk_K_rho_T[23:33, 2], rownames.force = NA)
error_Kirk_K_rho_0.95_T_0.2 = data.matrix(error_Kirk_K_rho_T[34:44, 2], rownames.force = NA)
error_Kirk_K_rho_0.999_T_0.2 = data.matrix(error_Kirk_K_rho_T[45:55, 2], rownames.force = NA)

error_Kirk_K_rho_T_0.2 = cbind(error_Kirk_K_rho_0.80_T_0.2, error_Kirk_K_rho_0.85_T_0.2, 
error_Kirk_K_rho_0.90_T_0.2, error_Kirk_K_rho_0.95_T_0.2, error_Kirk_K_rho_0.999_T_0.2)
error_Kirk_K_rho_T_0.2

error_Kirk_K_rho_0.80_T_0.3 = data.matrix(error_Kirk_K_rho_T[1:11, 3], rownames.force = NA)
error_Kirk_K_rho_0.85_T_0.3 = data.matrix(error_Kirk_K_rho_T[12:22, 3], rownames.force = NA)
error_Kirk_K_rho_0.90_T_0.3 = data.matrix(error_Kirk_K_rho_T[23:33, 3], rownames.force = NA)
error_Kirk_K_rho_0.95_T_0.3 = data.matrix(error_Kirk_K_rho_T[34:44, 3], rownames.force = NA)
error_Kirk_K_rho_0.999_T_0.3 = data.matrix(error_Kirk_K_rho_T[45:55, 3], rownames.force = NA)

error_Kirk_K_rho_T_0.3 = cbind(error_Kirk_K_rho_0.80_T_0.3, error_Kirk_K_rho_0.85_T_0.3, 
error_Kirk_K_rho_0.90_T_0.3, error_Kirk_K_rho_0.95_T_0.3, error_Kirk_K_rho_0.999_T_0.3)
error_Kirk_K_rho_T_0.3

error_Kirk_K_rho_0.80_T_0.4 = data.matrix(error_Kirk_K_rho_T[1:11, 4], rownames.force = NA)
error_Kirk_K_rho_0.85_T_0.4 = data.matrix(error_Kirk_K_rho_T[12:22, 4], rownames.force = NA)
error_Kirk_K_rho_0.90_T_0.4 = data.matrix(error_Kirk_K_rho_T[23:33, 4], rownames.force = NA)
error_Kirk_K_rho_0.95_T_0.4 = data.matrix(error_Kirk_K_rho_T[34:44, 4], rownames.force = NA)
error_Kirk_K_rho_0.999_T_0.4 = data.matrix(error_Kirk_K_rho_T[45:55, 4], rownames.force = NA)

error_Kirk_K_rho_T_0.4 = cbind(error_Kirk_K_rho_0.80_T_0.4, error_Kirk_K_rho_0.85_T_0.4, 
error_Kirk_K_rho_0.90_T_0.4, error_Kirk_K_rho_0.95_T_0.4 ,error_Kirk_K_rho_0.999_T_0.4)
error_Kirk_K_rho_T_0.4

error_Kirk_K_rho_0.80_T_0.5 = data.matrix(error_Kirk_K_rho_T[1:11, 5], rownames.force = NA)
error_Kirk_K_rho_0.85_T_0.5 = data.matrix(error_Kirk_K_rho_T[12:22, 5], rownames.force = NA)
error_Kirk_K_rho_0.90_T_0.5 = data.matrix(error_Kirk_K_rho_T[23:33, 5], rownames.force = NA)
error_Kirk_K_rho_0.95_T_0.5 = data.matrix(error_Kirk_K_rho_T[34:44, 5], rownames.force = NA)
error_Kirk_K_rho_0.999_T_0.5 = data.matrix(error_Kirk_K_rho_T[45:55, 5], rownames.force = NA)

error_Kirk_K_rho_T_0.5 = cbind(error_Kirk_K_rho_0.80_T_0.5, error_Kirk_K_rho_0.85_T_0.5, 
error_Kirk_K_rho_0.90_T_0.5, error_Kirk_K_rho_0.95_T_0.5, error_Kirk_K_rho_0.999_T_0.5)
error_Kirk_K_rho_T_0.5


##Here we calculate the error produced by the modified Kirk formula for each rho and T
error_Kirk_modif_K_rho_T = ((results_function_Kirk_modif_K_rho_T * 100) /  results_function_S3_K_rho_T) - 100
colnames(error_Kirk_modif_K_rho_T) <- c('T_0.1', 'T_0.2','T_0.3','T_0.4','T_0.5')
error_Kirk_modif_K_rho_T

##Here we organize our T matrixes for the modified Kirk formula, 
##with K being the rows and rho being the columns
error_Kirk_modif_K_rho_0.80_T_0.1 = data.matrix(error_Kirk_modif_K_rho_T[1:11, 1], rownames.force = NA)
error_Kirk_modif_K_rho_0.85_T_0.1 = data.matrix(error_Kirk_modif_K_rho_T[12:22, 1], rownames.force = NA)
error_Kirk_modif_K_rho_0.90_T_0.1 = data.matrix(error_Kirk_modif_K_rho_T[23:33, 1], rownames.force = NA)
error_Kirk_modif_K_rho_0.95_T_0.1 = data.matrix(error_Kirk_modif_K_rho_T[34:44, 1], rownames.force = NA)
error_Kirk_modif_K_rho_0.999_T_0.1 = data.matrix(error_Kirk_modif_K_rho_T[45:55, 1], rownames.force = NA)

error_Kirk_modif_K_rho_T_0.1 = cbind(error_Kirk_modif_K_rho_0.80_T_0.1, error_Kirk_modif_K_rho_0.85_T_0.1, 
error_Kirk_modif_K_rho_0.90_T_0.1, error_Kirk_modif_K_rho_0.95_T_0.1, error_Kirk_modif_K_rho_0.999_T_0.1)
error_Kirk_modif_K_rho_T_0.1

error_Kirk_modif_K_rho_0.80_T_0.2 = data.matrix(error_Kirk_modif_K_rho_T[1:11, 2], rownames.force = NA)
error_Kirk_modif_K_rho_0.85_T_0.2 = data.matrix(error_Kirk_modif_K_rho_T[12:22, 2], rownames.force = NA)
error_Kirk_modif_K_rho_0.90_T_0.2 = data.matrix(error_Kirk_modif_K_rho_T[23:33, 2], rownames.force = NA)
error_Kirk_modif_K_rho_0.95_T_0.2 = data.matrix(error_Kirk_modif_K_rho_T[34:44, 2], rownames.force = NA)
error_Kirk_modif_K_rho_0.999_T_0.2 = data.matrix(error_Kirk_modif_K_rho_T[45:55, 2], rownames.force = NA)

error_Kirk_modif_K_rho_T_0.2 = cbind(error_Kirk_modif_K_rho_0.80_T_0.2, error_Kirk_modif_K_rho_0.85_T_0.2, 
error_Kirk_modif_K_rho_0.90_T_0.2, error_Kirk_modif_K_rho_0.95_T_0.2, error_Kirk_modif_K_rho_0.999_T_0.2)
error_Kirk_modif_K_rho_T_0.2

error_Kirk_modif_K_rho_0.80_T_0.3 = data.matrix(error_Kirk_modif_K_rho_T[1:11, 3], rownames.force = NA)
error_Kirk_modif_K_rho_0.85_T_0.3 = data.matrix(error_Kirk_modif_K_rho_T[12:22, 3], rownames.force = NA)
error_Kirk_modif_K_rho_0.90_T_0.3 = data.matrix(error_Kirk_modif_K_rho_T[23:33, 3], rownames.force = NA)
error_Kirk_modif_K_rho_0.95_T_0.3 = data.matrix(error_Kirk_modif_K_rho_T[34:44, 3], rownames.force = NA)
error_Kirk_modif_K_rho_0.999_T_0.3 = data.matrix(error_Kirk_modif_K_rho_T[45:55, 3], rownames.force = NA)

error_Kirk_modif_K_rho_T_0.3 = cbind(error_Kirk_modif_K_rho_0.80_T_0.3, error_Kirk_modif_K_rho_0.85_T_0.3, 
error_Kirk_modif_K_rho_0.90_T_0.3, error_Kirk_modif_K_rho_0.95_T_0.3, error_Kirk_modif_K_rho_0.999_T_0.3)
error_Kirk_modif_K_rho_T_0.3

error_Kirk_modif_K_rho_0.80_T_0.4 = data.matrix(error_Kirk_modif_K_rho_T[1:11, 4], rownames.force = NA)
error_Kirk_modif_K_rho_0.85_T_0.4 = data.matrix(error_Kirk_modif_K_rho_T[12:22, 4], rownames.force = NA)
error_Kirk_modif_K_rho_0.90_T_0.4 = data.matrix(error_Kirk_modif_K_rho_T[23:33, 4], rownames.force = NA)
error_Kirk_modif_K_rho_0.95_T_0.4 = data.matrix(error_Kirk_modif_K_rho_T[34:44, 4], rownames.force = NA)
error_Kirk_modif_K_rho_0.999_T_0.4 = data.matrix(error_Kirk_modif_K_rho_T[45:55, 4], rownames.force = NA)

error_Kirk_modif_K_rho_T_0.4 = cbind(error_Kirk_modif_K_rho_0.80_T_0.4, error_Kirk_modif_K_rho_0.85_T_0.4, 
error_Kirk_modif_K_rho_0.90_T_0.4, error_Kirk_modif_K_rho_0.95_T_0.4, error_Kirk_modif_K_rho_0.999_T_0.4)
error_Kirk_modif_K_rho_T_0.4

error_Kirk_modif_K_rho_0.80_T_0.5 = data.matrix(error_Kirk_modif_K_rho_T[1:11, 5], rownames.force = NA)
error_Kirk_modif_K_rho_0.85_T_0.5 = data.matrix(error_Kirk_modif_K_rho_T[12:22, 5], rownames.force = NA)
error_Kirk_modif_K_rho_0.90_T_0.5 = data.matrix(error_Kirk_modif_K_rho_T[23:33, 5], rownames.force = NA)
error_Kirk_modif_K_rho_0.95_T_0.5 = data.matrix(error_Kirk_modif_K_rho_T[34:44, 5], rownames.force = NA)
error_Kirk_modif_K_rho_0.999_T_0.5 = data.matrix(error_Kirk_modif_K_rho_T[45:55, 5], rownames.force = NA)

error_Kirk_modif_K_rho_T_0.5 = cbind(error_Kirk_modif_K_rho_0.80_T_0.5, error_Kirk_modif_K_rho_0.85_T_0.5, 
error_Kirk_modif_K_rho_0.90_T_0.5, error_Kirk_modif_K_rho_0.95_T_0.5, error_Kirk_modif_K_rho_0.999_T_0.5)
error_Kirk_modif_K_rho_T_0.5


##Here we save the suface plot images for the Kirk formula
persp(error_Kirk_K_rho_T_0.1, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='red', main = 'error_Kirk_K_rho_T_0.1', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_K_rho_T_0.1.pdf');
persp(error_Kirk_K_rho_T_0.2, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='red', main = 'error_Kirk_K_rho_T_0.2', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_K_rho_T_0.2.pdf');
persp(error_Kirk_K_rho_T_0.3, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='red', main = 'error_Kirk_K_rho_T_0.3', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_K_rho_T_0.3.pdf');
persp(error_Kirk_K_rho_T_0.4, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='red', main = 'error_Kirk_K_rho_T_0.4', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_K_rho_T_0.4.pdf');
persp(error_Kirk_K_rho_T_0.5, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='red', main = 'error_Kirk_K_rho_T_0.5', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_K_rho_T_0.5.pdf');


##Here we save the suface plot images for the modified Kirk formula
persp(error_Kirk_modif_K_rho_T_0.1, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='green', main = 'error_Kirk_modif_K_rho_T_0.1', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_modif_K_rho_T_0.1.pdf');
persp(error_Kirk_modif_K_rho_T_0.2, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='green', main = 'error_Kirk_modif_K_rho_T_0.2', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_modif_K_rho_T_0.2.pdf');
persp(error_Kirk_modif_K_rho_T_0.3, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='green', main = 'error_Kirk_modif_K_rho_T_0.3', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_modif_K_rho_T_0.3.pdf');
persp(error_Kirk_modif_K_rho_T_0.4, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='green', main = 'error_Kirk_modif_K_rho_T_0.4', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_modif_K_rho_T_0.4.pdf');
persp(error_Kirk_modif_K_rho_T_0.5, scale = TRUE, theta = 40, phi = 5, ticktype = 'detailed', col='green', main = 'error_Kirk_modif_K_rho_T_0.5', xlab='K', ylab='rho', zlab='errors' )
dev.print(pdf, 'error_Kirk_modif_K_rho_T_0.5.pdf');


options(digits=15)
error_Kirk_K_rho_T
error_Kirk_modif_K_rho_T

##Here we superpose the surface plot images and we save them
matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
persp(matrix_K, matrix_rho, error_Kirk_K_rho_T_0.1, scale = TRUE, theta = 40, phi = 20,
ticktype = 'detailed', col='red', main = 'Red Surface: Kirk Errors / Green Surface: Modified Kirk Errors',
xlab='K', ylab='rho', zlab='errors', ylim = c(0.8, 1), cex.lab = 0.8, cex.axis = 0.8)
par(new = TRUE)
persp(error_Kirk_modif_K_rho_T_0.1, scale = TRUE, theta = 40, phi = 20, 
col='green', xlab='', ylab='', zlab='', ticktype = 'simple', zlim = c(-0.6097, 34.64) )
dev.print(pdf, 'error_Kirk_K_rho_T_0.1_and_error_Kirk_modif_K_rho_T_0.1.pdf');


matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
persp(matrix_K, matrix_rho, error_Kirk_K_rho_T_0.2, scale = TRUE, theta = 40, phi = 20,
ticktype = 'detailed', col='red', main = 'Red Surface: Kirk Errors / Green Surface: Modified Kirk Errors',
xlab='K', ylab='rho', zlab='errors', ylim = c(0.8, 1), cex.lab = 0.8, cex.axis = 0.8)
par(new = TRUE)
persp(error_Kirk_modif_K_rho_T_0.2, scale = TRUE, theta = 40, phi = 20, 
col='green', xlab='', ylab='', zlab='', ticktype = 'simple', zlim = c(-0.241, 21.76) )
dev.print(pdf, 'error_Kirk_K_rho_T_0.2_and_error_Kirk_modif_K_rho_T_0.2.pdf');


matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
persp(matrix_K, matrix_rho, error_Kirk_K_rho_T_0.3, scale = TRUE, theta = 40, phi = 20,
ticktype = 'detailed', col='red', main = 'Red Surface: Kirk Errors / Green Surface: Modified Kirk Errors',
xlab='K', ylab='rho', zlab='errors', ylim = c(0.8, 1), cex.lab = 0.8, cex.axis = 0.8)
par(new = TRUE)
persp(error_Kirk_modif_K_rho_T_0.3, scale = TRUE, theta = 40, phi = 20, 
col='green', xlab='', ylab='', zlab='', ticktype = 'simple', zlim = c(-0.127, 17.33))
dev.print(pdf, 'error_Kirk_K_rho_T_0.3_and_error_Kirk_modif_K_rho_T_0.3.pdf');


matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
persp(matrix_K, matrix_rho, error_Kirk_K_rho_T_0.4, scale = TRUE, theta = 40, phi = 20,
ticktype = 'detailed', col='red', main = 'Red Surface: Kirk Errors / Green Surface: Modified Kirk Errors',
xlab='K', ylab='rho', zlab='errors', ylim = c(0.8, 1), cex.lab = 0.8, cex.axis = 0.8)
par(new = TRUE)
persp(error_Kirk_modif_K_rho_T_0.4, scale = TRUE, theta = 40, phi = 20, 
col='green', xlab='', ylab='', zlab='', ticktype = 'simple', zlim = c(-0.089, 15.34))
dev.print(pdf, 'error_Kirk_K_rho_T_0.4_and_error_Kirk_modif_K_rho_T_0.4.pdf');


matrix_K <- matrix(c(0:10), nrow = 11, ncol = 1)
matrix_rho <- matrix(c(0.80, 0.85, 0.90, 0.95, 0.999), nrow = 5, ncol = 1)
persp(matrix_K, matrix_rho, error_Kirk_K_rho_T_0.5, scale = TRUE, theta = 40, phi = 20,
ticktype = 'detailed', col='red', main = 'Red Surface: Kirk Errors / Green Surface: Modified Kirk Errors',
xlab='K', ylab='rho', zlab='errors', ylim = c(0.8, 1), cex.lab = 0.8, cex.axis = 0.8)
par(new = TRUE)
persp(error_Kirk_modif_K_rho_T_0.5, scale = TRUE, theta = 40, phi = 20, 
col='green', xlab='', ylab='', zlab='', ticktype = 'simple', zlim = c(-0.072, 14.28))
dev.print(pdf, 'error_Kirk_K_rho_T_0.5_and_error_Kirk_modif_K_rho_T_0.5.pdf');






##Some useful tools:
##Convert data field into a matrix: matrixS1 = data.matrix(S1, rownames.force = NA)
##Join two matrixes: matrixS1andS2 = cbind(S1, S2)
##Mean of two rows: rowMeans <- apply(matrixS1S2,1,mean) 
##Mean of two rows: matrixrowMeans = data.matrix(rowMeans, rownames.force = NA)
##Mean of two rows: matrixS1minusS2$mean <- rowMeans(matrixS1S2, na.rm = TRUE)
##Calculate max(S^1(T) - S^2(T), K): rowMax <- apply(matrixS1andS2,1,max) 
##Format S3 to take onl 5 decimals: format(round(S3, 5), nsmall = 5)
##plot( error_Kirk[,'rho_0.8'], type="l", col="red", ylim=c(0.0, 0.2) )
##par(new=TRUE)
##plot( error_Kirk_modif[,'rho_0.8'], type="l", col="green" )
##To go to other lines of axis: 
##i = 4
##ii = 44
##mtext(side=1,text=paste0("Line", ii),line=i, family = "serif")
## Use outer to compute faster 2 variable functions: z <- outer(K, rho, function_Kirk_K)
