library(Deriv)
Func = function(x) {tan(x * pi) - sin(x * pi)}
FuncG = function(x) {exp(x)/pi}

Fx <- function (x) {tan(x * pi) - sin(x * pi)}
Hx <- function(x,x2) {x - ((Fx(x)*(x - x2))/(Fx(x) - Fx(x2)))}
funcionRecursiva <- function(a,b)
{
  sequi = seq(0,2,0.1)
  plot(sequi,Fx(sequi),type = 'l', col = c("blue"))
  
  abline(h = 0)
  x1 = a
  x2 = b
  x = Hx(x1,x2)
  
  i = 0
  error<-abs(Fx(x1)/Hx(x1,x2))
  
    
  
  while(Fx(x) != 0)
  {
    cat("Iteracion=",i,"\tFx(x)=",Fx(x),"\tX=",x,"\tError=",error,"\n")
    points(rbind(c(x,Fx(x))),pch=15,cex=0.4,col="red")
    
    
    if(error >  1.e-9)
    {
      x2 = x1
      x1 = x
    }
    else {break}
    
    x = Hx(x1,x2)
    error<-abs(Fx(x1)/Hx(x1,x2))
    i = i + 1
  }
  cat("Iteracion=",i,"\tFx(x)=",Fx(x),"\tX=",x,"\tError=",error)
  points(rbind(c(x,Fx(x))),pch=15,cex=0.4,col="red")
}

funcionRecursiva(1,0.7)

fPuntoFijo = function(a,b)
{
  x = seq(a,b,0.1)
  FuncDerG = Deriv(FuncG)
  print (FuncDerG(a))
  i = 0
  if(FuncDerG(a) < 1)
  {
    fijo = FuncG(a)
    
    plot(x,Func(x),type = 'l', col = c("blue"))
    
    abline(h = 0)
    
    aux = Func(fijo)
    
    points(rbind(c(fijo,aux)),pch=15,cex=0.4,col="red")
    cat("Iteracion=",i,"\tFunc(x)=",Func(fijo),"\tX=",fijo,"\tError=---\n")
    repeat
    {
      fijoN = FuncG(fijo)
      
      error = abs(fijoN-fijo)/fijoN
      
      if(error < 1.e-9)
      { 
        
        break
        
      }
      
      i = i + 1
      
      fijo = fijoN
      
      aux = Func(fijo)
      
      points(rbind(c(fijo,aux)),pch=15,cex=0.4,col="red")
      
      cat("Iteracion=",i,"\tFunc(x)=",Func(fijo),"\tX=",fijo,"\tError=",error,"\n")
    }
    
  }
  
  
}

#fPuntoFijo(0.2,1)
