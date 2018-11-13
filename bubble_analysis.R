#Response Surface Methodology

#Step 1
bubbleData=read.csv(file=file.choose())
attach(bubbleData)
model1=lm(Y~A+B+C+D)
summary(model1)
model2=lm(Y~A*B*C*D)
summary(model2)

#So factors A(gap) and D(power) are significant, 


#Step 2
#From above, the first order model
#is y = 776 -50.8*xA + 153.1*xD

#From the center point xA=xD=0,
#we wish to move -50.8 units in 
#the xA direction for every 153.1
#units in the xD direction.  Thus
#the path of steepest asecent passes
#through the point (xA=0,xD=0) and has
#slope 153.1/-50.8 ~ -3.

#The engineer decides on a step size of 25W
#of power (D).  This is equivalent to a step
#in coded variable xD of 1.  Therefore the steps
#along the path of steepest ascent are xD=+1
#xA=xD/-3=-0.33.  This is equivalent to -0.067cm
#in the original variable A.

#Step 3
rm(data)
data3=read.csv(file=file.choose())
attach(data3)
data3
model3=lm(Y ~ A+B+C+A*B+ A*C + B*C+ I(A^2)+I(B^2)+I(C^2))
summary(model3)

model3=lm(Y ~ A+B+C+A*B+ A*C + B*C)

model3=lm(Y~A+B+A*B) #Not orthogonal design, so need to refit
summary(model3)

#Therefore the final model of etch rate is
# y1 = 1155.7 + 57.1*xA + 149.7*xD + 89*xA*xD
# in the space of the NEW (step 3) coded variables xA and xD

#Graphical methods to display
f = function(xA,xD) 1155.7 + 57.1*xA + 149.7*xD + 89*xA*xD
x1 = seq(-1.4,1.4,0.1)
x2 = seq(-1.4,1.4,0.1)
etchrate = outer(x1,x2,f) #calculates f at every place on the grid of x1 and x2
contour(x1,x2,etchrate,xlab="Gap",ylab="Power",main="Etch Rate",col="red",levels=c(950,1000,1050,1100,1150,1200,1300,1400))
#persp(x1,x2,etchrate,theta=60,phi=30)

#To optimize over etch rate
f = function(x) -1*(1155.7 + 57.1*x[1] + 149.7*x[2] + 89*x[1]*x[2]) #-1 because optim is a minimization technique
myMax = optim(c(0,0),f,lower=c(-1.682,-1.682),upper=c(1.682,1.682),method="L-BFGS-B")
myMax$par

#The model of uniformity
model4=lm(Y2~A+D+A*D+Asq+Dsq)
summary(model4)
#Every term is significant, therefore our final model is
# y2 = 89.27 + 4.68*xA + 4.3523*xD -3.4*xA*xA - 1.82*xD*xD + 4.38*xA*xD
g = function(xA,xD) 89.27 + 4.68*xA + 4.3523*xD -3.4*xA*xA - 1.82*xD*xD + 4.38*xA*xD
uniformity = outer(x1,x2,g)
contour(x1,x2,uniformity,xlab="Gap",ylab="Power",main="Uniformity",col="blue")

#To simultaneously satisfy both objectives of etch rate between 1100 and 1150
# and uniformity < 80, graph the intersection of the contour plots

contour(x1,x2,etchrate,xlab="Gap",ylab="Power",main="Etch Rate",col="red",levels=c(950,1000,1050,1100,1150,1200,1300,1400))
contour(x1,x2,etchrate,xlab="Gap",ylab="Power",main="Feasible region",col="red",levels=c(1100,1150))
contour(x1,x2,uniformity,xlab="Gap",ylab="Power",main="Uniformity",col="blue",levels=80,add=TRUE)

#From this we can see one feasible solution is xA=-1.2,xD=0.75
#This translates to Gap = 0.56 cm, Power = 394 W
