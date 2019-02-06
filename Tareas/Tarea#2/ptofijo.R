library(Deriv)
Func = function(x) {exp(x) - x*pi}
FuncG = function(x) {exp(x)/pi}



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
      
      if(error < 1.e-8)
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

fPuntoFijo(0.2,1)