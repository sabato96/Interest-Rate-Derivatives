
library(ggplot2)
library(reshape2)
library(readr)
library(dplyr)
library(zoo)
library(RQuantLib)


####################VASICEK STIMA MONDO REALE##############


#Caricare le funzioni dei parametri vasicek

Bvas= function(a,maturities) {
  
  Bvas=(1-exp(-a*maturities))/a
  
  return(Bvas)
}

Avas=function(a,b,dstvas,Bva,maturities){
  
  Avas=exp((Bva-maturities)*(a^2*b-dstvas^2/2)/(a^2)-dstvas^2*Bva^2/(4*a))
  return(Avas)
}

Rvas=function(Ava,Bva,r0,maturities){
  
  Rvas=(-log(Ava)+Bva*r0)/maturities
  return(Rvas)
  
}

Bcir=function(gamma,maturities,a){
  Bcir=(2*(exp(gamma*maturities)-1))/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma)
  return(Bcir)
}

Acir=function(gamma,a,maturities,b,dstcir){
  Acir=(2*gamma*exp((a+gamma)*maturities/2)/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma))^(2*a*b/(dstcir^2))
  return(Acir)
}

Rcir=function(Aci,Bci,r0,maturities){
  Rcir=(-log(Aci)+Bci*r0)/maturities
  return(Rcir)
}

#PRENDIAMO LA SERIE STORICA DEGLI ZERO RATES DEI TREASURT BOND AMERICANI A 3 MESI

data.tbill3 <- read_csv("C:\\Users\\gargi\\Desktop\\Universita'\\FIN STOCA\\Progetto finale\\Progetto fin. stoc GARGIULO SABATO\\3month T-bill.csv")
#colnames(data.tbill3)=c("Date","Close")
data.tbill3$Date=as.Date(data.tbill3$Date)

data.tbill3= subset(data.tbill3, Date > as.Date("1988-12-30"))

data.tbill3$Close = as.character(data.tbill3$Close)
data.tbill3$Close[data.tbill3$Close == "."] = NA

data.tbill3=data.tbill3 %>% do(na.locf(.))


data.tbill3$Close=as.numeric(data.tbill3$Close)
data.tbill3$Close=data.tbill3$Close/100
data.tbill3=na.omit(data.tbill3)
data.tbill3$Close=as.numeric(data.tbill3$Close)
plot(data.tbill3$Close)
#FINALMENTE ABBIAMO OTTENUTO LA SERIE STORICA DEI TASSI A 3 MESI
#AGGIUNGIAMO UNA COLONNA CONTENENTE Dr (delta R)

deltar=c(NA,(diff(data.tbill3$Close)))


data.tbill3$delta=unlist(deltar)


data.tbill3=data.tbill3[,c(1,5,8)]
data.tbill3$delta=as.numeric(data.tbill3$delta)
plot(data.tbill3$delta)
#facciamo una regressione lineare per stimare deltar in funzione di r

data.tbill3[1,3]=0
linearMod <- glm(delta ~ Close, data=data.tbill3)

summary(linearMod)

#intercetta
intercept=as.numeric(linearMod$coefficients[1])
#b1
b1=as.numeric(linearMod$coefficients[2])

#standard error (inserire manualmente)
sterror=0.0002396



#mondo risk neutral vs mondo reale

#Nel mondo risk neutral   dr=a(b-r)*dt + sig*dZ
#Nel mondo reale          dr=[a(b-r)+lambda*sig]dt + sig*dZ

#lambda=prezzo di mercato del rischio
#Se lambda è costante, il processo nel mondo reale assumerà la forma
# dr=a(bstar-r)dt + sig*dZ    CON   bstar= b+(lambda*sig)/a

#allora, tramite i risultati della regressione, intercept b1 e sterror, possiamo
#ricavare a , bstar,sig
#poniamo dt=1/250 essendoci circa 250 osservazioni ogni anno

dt=1/250

a=-b1/dt
bstar=intercept/-b1
sig=sterror*sqrt(1/dt)

#ABBIAMO OTTENUTO I PARAMETRI DEL MODELLO VASICEK NEL MONDO REALE

rm(b1,dt,intercept,sterror)


#MA A NOI INTERESSA IL MONDO RISK NEUTRAL, QUINDI DOBBIAMO STIMARE IL MODELLO RISK NEUTRAL
#a sarà uguale sia nel mondo risk neutral che nel mondo reale, così come sig
#cioè che cambia sarà b, che nel mondo reale è bstar, nel mondo risk-neutral è b


#b=bstar-(lambda*sig)/a

#Costruiamo la zero curve vigente oggi sul mercato


# This data is taken from sample code shipped with QuantLib 0.3.10.
# params <- list(tradeDate=as.Date('2019-07-08'),
#                 settleDate=as.Date('2019-07-10'),
#              dt=.25,
#               interpWhat="discount",
#                 interpHow="loglinear")
#  tsQuotes <- list(d1w =0.02377,
#                   d1m =0.02296,
#                   d3m= 0.022114,
#                   d6m= 0.022098,  #https://www.thefinancials.com/Default.aspx?SubSectionID=homesumm&ReportFileName=EX_Interest_Swaps.html
#                   d1y= 0.021916,
#                   s3y =0.01820,
#                   s5y =0.01810,
#                   s10y =0.01980,
#                   s30y =0.02210)
#  times <- c(0.5,1,2,3,5,7,10,15,20,29.75)
#  setEvaluationDate(params$tradeDate)
#  curves <- DiscountCurve(params, tsQuotes, times)
#  plot(curves$zerorates)
#  Rmrkt=curves$zerorates[c(1:9)]
####################################



Rmrkt=c(0.0214,0.0199,0.0188,0.0184,0.0186,0.0194,0.0205,0.0232,0.0253)
#Rmrkt=curves$zerorates[-1]
maturities=c(0.5,1,2,3,5,7,10,20,30)

today.curve=cbind(maturities,Rmrkt)

plot(today.curve)

#short rate oggi


r0=as.numeric(data.tbill3[7655,2])


#TEST

B.0T=Bvas(a,maturities)


r.fun=function(par,B.0T,r0,maturities,a,sig,bstar,Rmrkt){
  
  A.0T=exp((B.0T-maturities)*(a^2*(bstar-(par[1]*sig/a))-sig^2/2)/(a^2)-(sig^2)*B.0T^2/(4*a))
  
  
  R.0T=(-1/maturities)*log(A.0T)+(1/maturities)*(B.0T*r0)
  
  sqdiff=sum((Rmrkt-R.0T)^2)                     
  return(sqdiff)
}



estim=optimize(r.fun,c(-100,200),a=a,B.0T=B.0T,bstar=bstar,sig=sig,maturities=maturities
               ,r0=r0,Rmrkt=Rmrkt)
estim

lambda=estim$minimum    #price of risk
b=bstar-(lambda*sig)/a  #b risk neutral
rm(r.fun,Rmrkt)

#La term structure oggi secondo il modello di vasicek e CIR coi parametri sopra menzionati si ottiene con

#TERM STRUCTURE AL TEMPO 0
maturities=c(1,2,3,4,5,6,7,8,9,10,15,20,30)
dstvas=sig
dstcir=dstvas/sqrt(r0)
gamma=sqrt(a^2+2*dstcir^2)
rm(B.0T,deltar,sig,data.tbill3,estim)

Bva=Bvas(a,maturities)
Ava=Avas(a,b,dstvas,Bva,maturities)
Rva=Rvas(Ava,Bva,r0,maturities)

Bci=Bcir(gamma,maturities,a)
Aci=Acir(gamma,a,maturities,b,dstcir)
Rci=Rcir(Aci,Bci,r0,maturities)


#all.data
all.data=cbind(maturities,Bva,Ava,Rva,Bci,Aci,Rci)



#Plot delle term structure
all.data=as.data.frame(all.data)

term.struc=ggplot()+
  geom_line(data=all.data,aes(x=maturities,y=Rva),color="blue",size=0.8)+
  geom_line(data=all.data,aes(x=maturities,y=Rci),color="red",size=0.8)+
  xlab('Maturities')+
  ylab('R')

print(term.struc)




#Ricaviamo la curva forward vasicek

Rva.=Rva[c(1:9)]
R.la=Rva[c(2:10)]
mat=maturities[c(1:9)]
mat.la=maturities[c(2:10)]
f.va=matrix(0,9,1)#vettore che conterrà i tassi forward

frw.impl=function(Rva.,R.la,mat,mat.la){
  
  
  f=((R.la*mat.la)-(Rva.*mat))/(mat.la-mat)
  return(f)
}
f.va[1:9]=frw.impl(Rva.,R.la,mat,mat.la)

# f.va contiene i forward rate vasicek
f.curv.va=cbind(f.va,mat)
f.curv.va=as.data.frame(f.curv.va)
frw.curv.va=ggplot()+
  geom_line(data=f.curv.va,aes(x=mat,y=f.va),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('F Vasicek')
print(frw.curv.va)

#forward CIR

Rci.=Rci[c(1:9)]
R.cla=Rci[c(2:10)]
f.ci=matrix(0,9,1)   #vettore che conterrà i tassi forward

frw.impl=function(Rci.,R.cla,mat,mat.la){
  
  f=((R.cla*mat.la)-(Rci.*mat))/(mat.la-mat)
  return(f)
}
f.ci[1:9]=frw.impl(Rci.,R.cla,mat,mat.la)


# f contiene i forward rate CIR

f.curv.ci=cbind(f.ci,mat)
f.curv.ci=as.data.frame(f.curv.ci)

frw.curv.ci=ggplot()+
  geom_line(data=f.curv.va,aes(x=mat,y=f.ci),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('F CIR')

print(frw.curv.ci)




###############PRICING CAP AND FLOOR


library(fOptions)

#Per prezzare cap (call) , floor (put) su un tasso di interesse
#col modello BLack-76 è necessario fornire i seguenti input


#INPUT
#Supponiamo di dover pagare lo US zero rate a 1 anno su un prestito
#concessoci da un finanziatore,per 5 anno
notional=1000000
time=1
#un cap ci permetterà di coprirci contro l'aumento del tasso di interesse
#in ogni periodo
#Supponiamo lo strike price del cap sia K
K=c(0.022,0.025,0.021,0.022,0.023)
#Definiamo la volatilità del tasso forward
sig.for=0.10
#Definiamo il forward rate per il periodo considerato
F.vas=f.va[1:5]
F.ci=f.ci[1:5]
#definiamo il risk free rate
r.free=0.02

cap.vas=GBSOption("c",F.vas,K,time,r.free,0,sig.for)

cap.cir=GBSOption("c",F.ci,K,time,r.free,0,sig.for)
cap.cir
cap.vas

price.va=sum(cap.vas@price*time*notional)
price.ci=sum(cap.cir@price*time*notional)
price.va
price.ci




# #GREEKS
# delta=GBSGreeks("delta","c",F.r1,K,time,r.free,0,sig.for)
# gamma=GBSGreeks("gamma","c",F.r1,K,time,r.free,0,sig.for)
# vega=GBSGreeks("vega","c",F.r1,K,time,r.free,0,sig.for)
# theta=GBSGreeks("theta","c",F.r1,K,time,r.free,0,sig.for)
# rho=GBSGreeks("rho","c",F.r1,K,time,r.free,0,sig.for)




#Supponiamo di voler valutare quale sarà la configurazione degli zero rate tra 1 anno



##SIMULAZIONE VASICEK

#Questo parte di script simula il tasso a breve, r, per il modello di Vasicek 
#utilizzando intervalli di tempo trimestrali per un periodo di 10 anni.
#Ad ogni intervallo calcola lo zero rate, R, a T.m anni. 
#Inoltre, calcola la zero curve alla fine dei T.m anni.

#INPUT

times=seq(0,10,by=0.25) 
T.m=1
B=Bvas(a,T.m)
A=Avas(a,b,dstvas,B,T.m)

#############simula path
n = 50  #numero di path generate
T1 = 10 #tempo
m = 120   # sottointervalli
dt=T1/m   #dt

r = matrix(0,m+1,n)  # matrice dove confluiranno le simulazioni
r[1,] = r0           #con primo elemento r0

#Loop per generare vasicek path multipli
for(j in 1:n){
  for(i in 2:(m+1)){
    dr = a*(b-r[i-1,j])*dt + dstvas*sqrt(dt)*rnorm(1,0,1)
    r[i,j] = r[i-1,j] + dr
  }
} 
###COSTRUZIONE MEDIA E INTERVALLI DI VARIAZIONE +- 2dst
t = seq(0, T1, dt)
rT.expected = b + (r0-b)*exp(-a*t)
rT.stdev <- sqrt( dstvas^2/(2*a)*(1-exp(-2*a*t)))
matplot(t, r[,1:50], type="l", lty=1, main="Simulazioni Short Rate Vasicek", ylab="rt") 
abline(h=b, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)

vas.path=rowMeans(r)     ###Scelta tra media dei tassi e estrazione di un singolo path 
                         #
                         #
#vas.path=r[,21]         #

plot(vas.path)
#CALCOLO ZERO RATE a T.m anni ad ogni intervallo

z.rate=function(T.m,B,A,vas.path){
  
  zero.rate=(-log(A)+B*vas.path)/T.m
}
zero.rate.vas=z.rate(T.m,B,A,vas.path)



#CONFRONTIAMO ORA L'ANDAMENTO DELLO ZERO RATE E QUELLO DELLO SHORT RATE

rate.all=cbind(t,vas.path,zero.rate.vas)
rate.all=as.data.frame(rate.all)


rate.vs=ggplot()+
  geom_line(data=rate.all,aes(x=t,y=vas.path),color="blue",size=1.2)+
  geom_line(data=rate.all,aes(x=t,y=zero.rate.vas),color="red",size=1.2)+
  xlab('Tempo')+
  ylab('R')

print(rate.vs)

sd(vas.path)
sd(zero.rate.vas)

#E' possibile notare come lo zero.rate a T.m anni sia sempre meno volatile dello short rate
#Estraiamo lo short rate r fra T.m anni dalla corrispondente riga di vas.path

r10=vas.path[12*T.m + 1]  

Bva=Bvas(a,maturities)
Ava=Avas(a,b,dstvas,Bva,maturities)
Rva=Rvas(Ava,Bva,r0,maturities)



#Cerchiamo gli Rvas tra T.m anni con varie maturities
get.vas.curve=function(r10,maturities,Ava,Bva){
  
  curve=(-log(Ava)+Bva*r10)/maturities
  return(curve)
  
}
zero.curve.vas=get.vas.curve(r10,maturities,Ava,Bva) #zero curve

#PLOTTING
df.zero=cbind(maturities,zero.curve.vas)
df.zero=as.data.frame(df.zero)


zero.graph=ggplot()+
  geom_line(data=df.zero,aes(x=maturities,y=zero.curve.vas),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('Rvas')

print(zero.graph)

#Abbiamo quindi simulato il tasso a breve r, per il modello di Vasicek, utilizzando intervalli di tempo trimestrali per un periodo di 10 anni.
#Per ogni intervallo abbiamo calcolato lo zero rate a T.m anni. Inoltre abbiamo costruito
#anche la zero curve per 10 anni. E' possibile Eseguire più volte il codice per vedere come
#la componente stocastica modifica la forma della zero curve al variare delle simulazioni

##a=rev. rate ; b= livello tendenz ; dts.vas=dst. vasicek 




#Ricaviamo la curva forward vasicek

Rva.=zero.curve.vas[c(1:9)]
R.la=zero.curve.vas[c(2:10)]
mat=maturities[c(1:9)]
mat.la=maturities[c(2:10)]
f.va=matrix(0,9,1)#vettore che conterrà i tassi forward

frw.impl=function(Rva.,R.la,mat,mat.la){
  
  
  f=((R.la*mat.la)-(Rva.*mat))/(mat.la-mat)
  return(f)
}
f.va[1:9]=frw.impl(Rva.,R.la,mat,mat.la)

# f.va contiene i forward rate vasicek
f.curv.va=cbind(f.va,mat)
f.curv.va=as.data.frame(f.curv.va)
frw.curv.va=ggplot()+
  geom_line(data=f.curv.va,aes(x=mat,y=f.va),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('F Vasicek')
print(frw.curv.va)


###FINE SIMULAZIONE VASICEK

#SIMULAZIONE COX-INGERSOLL-ROSS (CIR)


#Questo foglio simula il tasso a breve, r, per il modello di CIR 
#utilizzando intervalli di tempo trimestrali per un periodo di 10 anni.
#Ad ogni intervallo calcola lo zero rate, R, a 10 anni. 
#Inoltre, calcola la zero curve alla fine dei 10 anni.

maturities=c(1,2,3,4,5,6,7,8,9,10,15,20,30)
times=seq(0,10,by=0.25)  #sequenza ""TENOR""
T.m=1 
B=Bcir(gamma,T.m,a)
A=Acir(gamma,a,T.m,b,dstcir)

n <- 100  #numero di path generate
T1 <- 10 #tempo
m <- 120   # sottointervalli
dt=T1/m   #dt

r <- matrix(0,m+1,n)  # matrice dove confluiranno le simulazioni
r[1,] <- r0           #con primo elemento r0

#Loop per generare vasicek path multipli
for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + dstcir*sqrt(dt)*rnorm(1,0,1)*sqrt(r[i-1,j])
    r[i,j] <- r[i-1,j] + dr
  }
} 

t <- seq(0, T1, dt)
rT.expected <- b -(b-r0)*exp(-a*t)
rT.stdev <- sqrt(((dstcir^2)/(2*a))*(1-(exp(-2*a*t)))*((2*r0*exp(-a*t))+b*(1-exp(-a*t))))
matplot(t, r[,1:100], type="l", lty=1, main="Short Rate Paths CIR", ylab="rt") 
abline(h=b, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)



cir.path=rowMeans(r)     ###Scelta tra media dei tassi e estrazione di un singolo path 
#
#
#cir.path=r[,21]         #

plot(cir.path)

#CALCOLO ZERO RATE 

z.rate=function(T.m,B,A,cir.path){
  
  zero.rate=(-log(A)+B*cir.path)/T.m
}
zero.rate.cir=z.rate(T.m,B,A,cir.path)



#CONFRONTIAMO ORA L'ANDAMENTO DELLO ZERO RATE E QUELLO DELLO SHORT RATE

rate.all=cbind(t,cir.path,zero.rate.cir)
rate.all=as.data.frame(rate.all)


rate.vs=ggplot()+
  geom_line(data=rate.all,aes(x=t,y=cir.path),color="blue",size=1.2)+
  geom_line(data=rate.all,aes(x=t,y=zero.rate.cir),color="red",size=1.2)+
  xlab('Tempo')+
  ylab('R')

print(rate.vs)

sd(cir.path)
sd(zero.rate.cir)
#E' possibile notare che il tasso R a lunga scadenza è 
#meno volatile del tasso a breve r

#Costruiamo ora la zero-curve

#Richiamiamo A e B stimati in precedenza nella parte sulla TermStructure

Bci=Bcir(gamma,maturities,a)

Aci=Acir(gamma,a,maturities,b,dstcir)

Rci=Rcir(Aci,Bci,r0,maturities)

#Estraiamo il tasso r a T.m anni dalla rispettiva riga di cir.path

r10=cir.path[T.m*12+1]  
#Cerchiamo gli Rcir a 10 anni con vasicek
get.cir.curve=function(r10,maturities,Aci,Bci){
  
  curve=(-log(Aci)+Bci*r10)/maturities
  return(curve)
  
}
cir.curve=get.cir.curve(r10,maturities,Aci,Bci) #zero curve

#PLOTTING
df.zero=cbind(maturities,cir.curve)
df.zero=as.data.frame(df.zero)


zero.graph=ggplot()+
  geom_line(data=df.zero,aes(x=maturities,y=cir.curve),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('Rcir')

print(zero.graph)



#Ricaviamo la curva forward CIR

Rci.=cir.curve[c(1:9)]
R.cla=cir.curve[c(2:10)]
f.ci=matrix(0,9,1)   #vettore che conterrà i tassi forward

frw.impl=function(Rci.,R.cla,mat,mat.la){
  
  f=((R.cla*mat.la)-(Rci.*mat))/(mat.la-mat)
  return(f)
}
f.ci[1:9]=frw.impl(Rci.,R.cla,mat,mat.la)


# f contiene i forward rate CIR

f.curv.ci=cbind(f.ci,mat)
f.curv.ci=as.data.frame(f.curv.ci)

frw.curv.ci=ggplot()+
  geom_line(data=f.curv.va,aes(x=mat,y=f.ci),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('F CIR')

print(frw.curv.ci)

###############PRICING CAP AND FLOOR


library(fOptions)

#Per prezzare cap (call) , floor (put) su un tasso di interesse
#col modello BLack-76 è necessario fornire i seguenti input


#INPUT
#Supponiamo di dover pagare lo US zero rate a 1 anno su un prestito
#concessoci da un finanziatore,per 5 anno
notional=1000000
time=1
#un cap ci permetterà di coprirci contro l'aumento del tasso di interesse
#in ogni periodo
#Supponiamo lo strike price del cap sia K
K=c(0.022,0.025,0.021,0.022,0.023)
#Definiamo la volatilità del tasso forward
sig.for=0.10
#Definiamo il forward rate per il periodo considerato
F.vas=f.va[1:5]
F.ci=f.ci[1:5]
#definiamo il risk free rate
r.free=0.02

cap.vas=GBSOption("c",F.vas,K,time,r.free,0,sig.for)

cap.cir=GBSOption("c",F.ci,K,time,r.free,0,sig.for)
cap.cir
cap.vas

price.va=sum(cap.vas@price*time*notional)
price.ci=sum(cap.cir@price*time*notional)
price.va
price.ci

#_______________________________________________________________________________####
################################BONUS SCRIPT#
rm(list=ls())

##################################CONFRONTO TERM STRUCTURE ##################


##a=rev. rate ; b= livello tendenz ; dts.vas=dst. vasicek 
#TERM STRUCTURE

#INPUT
r0=0.04
a=0.2
b=0.041
dstcir=0.05
dstvas=sqrt(r0)*dstcir
gamma=sqrt(a^2+2*dstcir^2)
maturities=c(1,2,3,4,5,6,7,8,9,10,15,20,30)


###################   VASICEK   #######################

#B 
Bvas= function(a,maturities) {
  
  Bvas=(1-exp(-a*maturities))/a
  
  return(Bvas)
  
}

#A in vasicek

Avas=function(a,b,dstvas,Bva,maturities){
  
  Avas=exp((Bva-maturities)*(a^2*b-dstvas^2/2)/(a^2)-dstvas^2*Bva^2/(4*a))
  return(Avas)
}

#TASSO R IN VASICEK

Rvas=function(Ava,Bva,r0,maturities){
  
  Rvas=(-log(Ava)+Bva*r0)/maturities
  return(Rvas)
}

Bva=Bvas(a,maturities)
Ava=Avas(a,b,dstvas,Bva,maturities)
Rva=Rvas(Ava,Bva,r0,maturities)


#########################    CIR    ########################

#B CIR
Bcir=function(gamma,maturities,a){
  Bcir=(2*(exp(gamma*maturities)-1))/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma)
  return(Bcir)
}
Bci=Bcir(gamma,maturities,a)

#A CIR

Acir=function(gamma,a,maturities,b,dstcir){
  Acir=(2*gamma*exp((a+gamma)*maturities/2)/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma))^(2*a*b/(dstcir^2))
  return(Acir)
}
Aci=Acir(gamma,a,maturities,b,dstcir)

#R CIR

Rcir=function(Aci,Bci,r0,maturities){
  Rcir=(-log(Aci)+Bci*r0)/maturities
  return(Rcir)
}
Rci=Rcir(Aci,Bci,r0,maturities)

all.data=cbind(maturities,Bva,Ava,Rva,Aci,Bci,Rci)

#Plot delle term structure
all.data=as.data.frame(all.data)

term.struc=ggplot()+
  geom_line(data=all.data,aes(x=maturities,y=Rva),color="blue",size=1.2)+
  geom_line(data=all.data,aes(x=maturities,y=Rci),color="red",size=1.2)+
  xlab('Maturities')+
  ylab('R')

print(term.struc)


####################################### simulazione vasicek definendo input

rm(list=ls())
##SIMULAZIONE VASICEK

#Questo foglio simula il tasso a breve, r, per il modello di Vasicek 
#utilizzando intervalli di tempo trimestrali per un periodo di 10 anni.
#Ad ogni intervallo calcola lo zero rate, R, a 10 anni. 
#Inoltre, calcola la zero curve alla fine dei 10 anni.
#RICHIAMIAMO LE FUNZIONI AVAS,BVAS,RVAS
#B 
Bvas= function(a,maturities) {
  
  Bvas=(1-exp(-a*maturities))/a
  
  return(Bvas)
  
}

#A in vasicek

Avas=function(a,b,dstvas,Bva,maturities){
  
  Avas=exp((Bva-maturities)*(a^2*b-dstvas^2/2)/(a^2)-dstvas^2*Bva^2/(4*a))
  return(Avas)
}

#TASSO R IN VASICEK

Rvas=function(Ava,Bva,r0,maturities){
  
  Rvas=(-log(Ava)+Bva*r0)/maturities
  return(Rvas)
}


#INPUT

r0=0.04
a=0.2
b=0.041
dstcir=0.05
dstvas=sqrt(r0)*dstcir
maturities=c(1,2,3,4,5,6,7,8,9,10,15,20,30)
times=seq(0,10,by=0.25)  #frequenza capitalizzaz.
T.m=2  #time to maturity
B=Bvas(a,T.m)
A=Avas(a,b,dstvas,B,T.m)

#############simula path
n <- 50  #numero di path generate
T1 <- 10 #tempo
m <- 120   # sottointervalli
dt=T1/m   #dt

r <- matrix(0,m+1,n)  # matrice dove confluiranno le simulazioni
r[1,] <- r0           #con primo elemento r0

#Loop per generare vasicek path multipli
for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + dstvas*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 
###COSTRUZIONE MEDIA E INTERVALLI DI VARIAZIONE +- 2dst
t <- seq(0, T1, dt)
rT.expected <- b + (r0-b)*exp(-a*t)
rT.stdev <- sqrt( dstvas^2/(2*a)*(1-exp(-2*a*t)))
matplot(t, r[,1:50], type="l", lty=1, main="Short Rate Paths", ylab="rt") 
abline(h=b, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)

vas.path=rowMeans(r)
#CALCOLO ZERO RATE 

z.rate=function(T.m,B,A,vas.path){
  
  zero.rate=(-log(A)+B*vas.path)/T.m
}
zero.rate=z.rate(T.m,B,A,vas.path)

#CONFRONTIAMO ORA L'ANDAMENTO DELLO ZERO RATE E QUELLO DELLO SHORT RATE

rate.all=cbind(t,vas.path,zero.rate)
rate.all=as.data.frame(rate.all)


rate.vs=ggplot()+
  geom_line(data=rate.all,aes(x=t,y=vas.path),color="blue",size=1.2)+
  geom_line(data=rate.all,aes(x=t,y=zero.rate),color="red",size=1.2)+
  xlab('Tempo')+
  ylab('R')

print(rate.vs)

sd(vas.path)
sd(zero.rate)

#E' possibile notare come lo zero.rate a T.m anni sia sempre meno volatile dello short rate
#Estraiamo lo short rate r fra T.m anni dalla corrispondente riga di vas.path

r10=vas.path[12*T.m + 1]  

Bva=Bvas(a,maturities)
Ava=Avas(a,b,dstvas,Bva,maturities)
Rva=Rvas(Ava,Bva,r0,maturities)



#Cerchiamo gli Rvas tra T.m anni con varie maturities
get.vas.curve=function(r10,maturities,Ava,Bva){
  
  curve=(-log(Ava)+Bva*r10)/maturities
  return(curve)
  
}
zero.curve.vas=get.vas.curve(r10,maturities,Ava,Bva) #zero curve

#PLOTTING
df.zero=cbind(maturities,zero.curve.vas)
df.zero=as.data.frame(df.zero)


zero.graph=ggplot()+
  geom_line(data=df.zero,aes(x=maturities,y=zero.curve.vas),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('Rvas')

print(zero.graph)

###########################SIMULAZIONE COX-INGERSOLL-ROSS (CIR)

rm(list=ls())
#Questo foglio simula il tasso a breve, r, per il modello di CIR 
#utilizzando intervalli di tempo trimestrali per un periodo di 10 anni.
#Ad ogni intervallo calcola lo zero rate, R, a 10 anni. 
#Inoltre, calcola la zero curve alla fine dei 10 anni.

#B CIR 
Bcir=function(gamma,maturities,a){
  Bcir=(2*(exp(gamma*maturities)-1))/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma)
  return(Bcir)
}


#A CIR

Acir=function(gamma,a,maturities,b,dstcir){
  Acir=(2*gamma*exp((a+gamma)*maturities/2)/((gamma+a)*(exp(gamma*maturities)-1)+2*gamma))^(2*a*b/(dstcir^2))
  return(Acir)
}


#R CIR
Rcir=function(Aci,Bci,r0,maturities){
  Rcir=(-log(Aci)+Bci*r0)/maturities
  return(Rcir)
}

#INPUT

r0=0.05
a=0.25
b=0.04
dstcir=0.08
dstvas=sqrt(r0)*dstcir
gamma=sqrt(a^2+2*dstcir^2)
maturities=c(1,2,3,4,5,6,7,8,9,10,15,20,30)
times=seq(0,10,by=0.25)  #sequenza ""TENOR""
T.m=1  #time to maturity
B=Bcir(gamma,T.m,a)
A=Acir(gamma,a,T.m,b,dstcir)

n <- 50  #numero di path generate
T1 <- 10 #tempo
m <- 120   # sottointervalli
dt=T1/m   #dt

r <- matrix(0,m+1,n)  # matrice dove confluiranno le simulazioni
r[1,] <- r0           #con primo elemento r0

#Loop per generare vasicek path multipli
for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + dstcir*sqrt(dt)*rnorm(1,0,1)*sqrt(r[i-1,j])
    r[i,j] <- r[i-1,j] + dr
  }
} 

t <- seq(0, T1, dt)
rT.expected <- b -(b-r0)*exp(-a*t)
rT.stdev <- sqrt(((dstcir^2)/(2*a))*(1-(exp(-2*a*t)))*((2*r0*exp(-a*t))+b*(1-exp(-a*t))))
matplot(t, r[,1:50], type="l", lty=1, main="Short Rate Paths", ylab="rt") 
abline(h=b, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)

cir.path=rowMeans(r)
#CALCOLO ZERO RATE 

z.rate=function(T.m,B,A,cir.path){
  
  zero.rate=(-log(A)+B*cir.path)/T.m
}
zero.rate=z.rate(T.m,B,A,cir.path)



#CONFRONTIAMO ORA L'ANDAMENTO DELLO ZERO RATE E QUELLO DELLO SHORT RATE

rate.all=cbind(t,cir.path,zero.rate)
rate.all=as.data.frame(rate.all)


rate.vs=ggplot()+
  geom_line(data=rate.all,aes(x=t,y=cir.path),color="blue",size=1.2)+
  geom_line(data=rate.all,aes(x=t,y=zero.rate),color="red",size=1.2)+
  xlab('Tempo')+
  ylab('R')

print(rate.vs)


#E' possibile notare che il tasso R a lunga scadenza è 
#meno volatile del tasso a breve r

#Costruiamo ora la zero-curve

#Richiamiamo A e B stimati in precedenza nella parte sulla TermStructure

Bci=Bcir(gamma,maturities,a)

Aci=Acir(gamma,a,maturities,b,dstcir)

Rci=Rcir(Aci,Bci,r0,maturities)

#Estraiamo il tasso r a T.m anni dall'ultima riga di cir.path

r10=cir.path[T.m*12+1]  #T.m*4+1
#Cerchiamo gli Rcir a 10 anni con vasicek
get.cir.curve=function(r10,maturities,Aci,Bci){
  
  curve=(-log(Aci)+Bci*r10)/maturities
  return(curve)
  
}
zero.curve=get.cir.curve(r10,maturities,Aci,Bci) #zero curve

#PLOTTING
df.zero=cbind(maturities,zero.curve)
df.zero=as.data.frame(df.zero)


zero.graph=ggplot()+
  geom_line(data=df.zero,aes(x=maturities,y=zero.curve),color="blue",size=1.2)+
  xlab('Maturities')+
  ylab('Rcir')

print(zero.graph)


