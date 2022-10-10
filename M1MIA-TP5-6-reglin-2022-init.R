
setwd("~/Documents") # à personnaliser
rm(list=objects())
graphics.off()

n=4 # nombre de lignes
p=3 # nombre de colonnes
vec=1:(n*p)
matrix(vec,nrow=n,ncol=p)

## ---- 
A=rbind(c(4,-1,1),
        c(3,10,3)) # 2 lignes * 3 colonnes
B=cbind(2:4,4:2) # 3 lignes * 2 colonnes
A[,2] # deuxième colonne
B[3,2] # troisième ligne deuxième colonne
A*B # erreur car les opérations sont faites selon chaque composante
A[,1:2]*B[2:3,] # ok
A%*%B # produit matriciel
solve(A%*%B)%*%A%*%B # vérification

## ---- 
rm(list=objects())
df = read.table("trempe.csv",sep=";",dec=",",header=TRUE)

# on peut s'aider du bouton "import data set" de l'onglet "Environnement"
df = read.csv2("trempe.csv")

## ---- 
head(df)
dim(df)
str(df)

df[, 2] # vecteur
df$soakpct # vecteur
df["soakpct"] # data frame

## ---- 
summary(df)
pairs(df)
round(cor(df), 3)

install.packages("corrplot")
library(corrplot)
corrplot(cor(df))
df[1, ]
 
install.packages("GGally")
library(GGally)
ggpairs(df)

## ---- 
X = as.matrix(cbind(1, df[-ncol(df)])) # opérations sur les colonnes / intercept + variables
Y = as.matrix(df$y) # vecteur des observations
theta.est = solve(t(X) %*% X) %*% t(X) %*% Y # régression sur estimation de thêta
n = length(Y)
p = ncol(X)
sigma.est = sqrt(sum((X %*% theta.est - Y)^2) / (n - p)) 
V = solve(t(X) %*% X) * sigma.est^2
stddev = sqrt(diag(V))

## ----
# pour la deuxième observation
X[2, ] %*% theta.est # = 0.01550526

# pour toutes les observations
Yest = X %*% theta.est
head(Yest)

## lm(y~soaktime + soakpct + difftime + diffpct,data=df)
res=lm(y~.,data=df)
summary(res)
names(res)     # le nom des informations sorties par summary
res$coef       # accès à l'une de ces informations
## à compléter !

## ----
# colonne Estimate: valeur estimée du paramètre
theta = res$coef; theta

# Std Error: racine carrée de la variance de l'estimation de l'écart-type
v = vcov(res); v # variance de l'estimateur
s = sqrt(diag(V)); s # colonne Std. Error

# estimation de sigma
sqrt(sum((df$y - res$fitted)^2) / res$df.residual) # = 0.002282929

# dimension de l'espace des résidus
res$df.residual # = 27

# valeurs ajustées
res$fitted

# statistique de test de Student pour toutes les composantes
t_obs = theta / s ; t_obs

# colonne Pr(>|t|): p-value du test précédent
alpha = 0.05
qt = qt(1 - alpha / 2, res$df); q
IC = data.frame(mean=theta, min=theta-qt*sqrt(diag(V)), 
                max = theta + qt * sqrt(diag(V)))
IC

# on effectue le test sur chaque composante
abs(t_obs) > q

# p-values associées
pt = 2 * pt(abs(t_obs), n - p, lower.tail = FALSE) ; pt # p-value
pt = 2 * (1 - pt(abs(t_obs), n - p)) ; pt # équivalent de pt

# IC des paramètres du modèle
IC = data.frame(mean=theta, min=theta-qt*sqrt(diag(V)), 
                max = theta + qt * sqrt(diag(V)))
IC
confint(res) # 0 appartient à l'intervalle de confiance pour soakpct & diffpct
             # i. e. les valeurs où le test de p-value n'est pas significatif
## ------------
x0 = matrix(c(1, 0.58, 1.0, 0.6, 0.85), ncol=5, nrow=1)
y0.chap = x0 %*% theta
v0.chap = x0 %*% V %*% t(x0)

data.frame(mean=y0.chap, min=y0.chap - qt * sqrt(v0.chap),
           max = y0.chap + qt *sqrt(v0.chap)) #idem
# avec la fonction predict
predict(res, newdata = data.frame(soaktime = 0.58, soakpct = 1, difftime = 0.6, diffpct = 0.85),
                                  interval = "confidence")

# avec la fonction anova
anova(lm(y~1, data=df), res)

## -------
# test de significativité avec Wald exact ~ Fisher(p-1=4, n-p=32-5)
A = matrix(c(0, 1, 0, 0, 0,
             0, 0, 1, 0, 0,
             0, 0, 0, 1, 0,
             0, 0, 0, 0, 1), nrow=p-1, ncol=p)

#mai_old = par()$mai
#mai_old = c(0.5, mai_old[2], 0.3, mai_old[4])
#par(mfrow=c(1, 1), mai)

# plot(Y, Yest, type="p")
plot(df$y, X %*% theta.est, xlab="profondeur observée", ylab="profondeur ajustée")
abline(0, 1) # première bissectrice
