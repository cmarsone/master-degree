
setwd("Mon répertoire") # à personnaliser
rm(list=objects())
graphics.off()

n=4 # nombre de lignes
p=3 # nombre de colonnes
vec=1:(n*p)
matrix(vec,nrow=n,ncol=p)

## ---- 
A=rbind(c(4,-1,1),
        c(3,10,3))
B=cbind(2:4,4:2)
A[,2]
B[3,2]
A*B
A[,1:2]*B[2:3,]
A%*%B
solve(A%*%B)%*%A%*%B

## ---- 
rm(list=objects())
df = read.table("trempe.csv",sep=";",dec=",",header=TRUE)

# on peut s'aider du bouton "import data set" de l'onglet "Environnement"
df = read.csv2("trempe.csv")

## ---- 
head(df)
dim(df)
str(df)


## ---- 
summary(df)
pairs(df)
round(cor(df),3)

library(corrplot)
corrplot(cor(df))
 
library(GGally)
ggpairs(df)

## ---- 
X = as.matrix(cbind(1,df[-ncol(df)]))
Y = as.matrix(df$y)
theta.est = solve(t(X)%*%X)%*% t(X)%*%Y
n = length(Y)
p = ncol(X)
sigma.est = sqrt(sum( (X%*%theta.est -Y)^2   )/(n-p)) 
V = solve(t(X)%*%X) *sigma.est^2
stddev = sqrt(diag(V))

## ---- 
## lm(y~soaktime + soakpct + difftime + diffpct,data=df)
res=lm(y~.,data=df)
summary(res)
names(res)     # le nom des informations sorties par summary
res$coef       # accès à l'une de ces informations
## à compléter !


