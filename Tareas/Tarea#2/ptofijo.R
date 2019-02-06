library(Deriv)
Func = function(x) {exp(x) - x*pi}
FuncG = function(x) {exp(x)/pi}



fPuntoFijo = function(a,b)
{
  x = seq(a,b,0.1)
  FuncDerG = Deriv(FuncG)
  i = 0
  if(abs(FuncDerG(a)) < 1 && ((FuncG(a)-a)*(FuncG(b)-b)) < 0)
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
      
      if(error < 1.e-8)
      { 
        
        break
        
      }
      
      i = i + 1
      
      fijo = fijoN
      
      aux = Func(fijo)
      points(rbind(c(i,error)),pch=15,cex=0.4,col="red")
      points(rbind(c(fijo,aux)),pch=15,cex=0.4,col="red")
      
      cat("Iteracion=",i,"\tFunc(x)=",Func(fijo),"\tX=",fijo,"\tError=",error,"\n")
    }
    
  }
  else
  {
    cat("En el intervalo[",a,",",b,"] no hay una raiz")
  }
  
  
}

fPuntoFijo(0.2,1)
