### df= dataframe, res=résultat de l'ajustement, df$y=variable expliquée
par(mfrow=c(2,2),oma=c(0,0,3,0))
plot(res$fitted,df$y,main="ajustés/observés")
abline(0,1)

##
plot(res$fitted,res$residuals,main="résidus bruts")

##
library(MASS) 
plot(res$fitted,studres(res),col=3,pch=3) 
points(res$fitted,stdres(res), col=2,pch=2 )
points(res$fitted,res$residuals,main="différents résidus")
abline(h=2,lty=2)
abline(h=-2,lty=2)
legend("bottomright",c("resid", "stdres", "studres"),col=1:3, pch=1:3,cex=0.5)

##
qqnorm(studres(res),main="graphe quantile-quantile")
qqline(stdres(res))

##
shapiro.test(studres(res)) 

title(main="Validation en régression linéaire multiple", outer=TRUE)
