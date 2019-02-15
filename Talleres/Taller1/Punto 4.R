library(Deriv)
Func = function (t) cos(3*t) +  exp(t)


fBiseccion <- function(x1,x2) {
   cat("f1",Func(x1),"f2",Func(x2),"totql",Func(x1)*Func(x2))
  
  if(Func(x1)*Func(x2) < 0)
  {
    print(Func(x2))
    x = seq(x1,x2,0.1)
    
    plot(x,Func(x),type="l",col="red")
    
    abline(h=0)
    
    ptoM = (x1+x2)/2
    
    error = abs(x1-x2)/2
    
    i = 0
    
    aux = Func(ptoM)
    
    points(rbind(c(ptoM,aux)),pch=15,cex=0.4,col="blue")
    
    valor = log10((x2-x1)/1.e-8)/log10(2)
    
    cat("Iteracion=",i,"\tFunc(x)=",Func(ptoM),"\tX=",ptoM,"\tError=",error,"\n")
    
    while (error > 1.e-8) {
      
      i = i+1
      
      if (Func(ptoM) == 0) break
      
      if (Func(ptoM)*Func(x1) < 0) x2 = ptoM else {x1 = ptoM}
      
      ptoM = (x1+x2)/2
      
      error = abs(x1-x2)/2
      
      aux = Func(ptoM)
      
      points(rbind(c(ptoM,aux)),pch=15,cex=0.4,col="blue")
      
      cat("Iteracion=",i,"\tFunc(x)=",Func(ptoM),"\tX=",ptoM,"\tError=",error,"\n")
      
    }
    cat ("Valor aproximado de iteraciones:", valor)
    
  }
  else
  {
    cat ("No valida")
  }
  
}

fSecante <- function(x0,x1) {
  
  x = seq(x0,0,0.1)
  
  plot(x,Func(x),type="l",col="red")
  
  abline(h=0)
  
  x2 = x1-(((x1-x0)*(Func(x1)))/(Func(x1)-Func(x0)))
  
  error = abs(x2-x1)/abs(x2)
  
  i = 0
  
  aux = Func(x2)
  
  points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
  
  cat("Iteracion=",i,"\tFunc(x)=",Func(x2),"\tX=",x2,"\tError=",error,"\n")
  
  while (error > 1.e-8) {
    
    x0 = x1
    
    x1 = x2
    
    x2 = x1-(((x1-x0)*(Func(x1)))/(Func(x1)-Func(x0)))
    
    i = i+1
    
    error = abs(x2-x1)/abs(x2)
    
    aux = Func(x2)
    
    points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
    
    cat("Iteracion=",i,"\tFunc(x)=",Func(x2),"\tX=",x2,"\tError=",error,"\n")
    
  }
  
}

fBiseccion(-1, 3)
fSecante(-1, 3)