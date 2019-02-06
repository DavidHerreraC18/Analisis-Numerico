

Func = function(x) {exp(x) - x*pi}



fBiseccion <- function(x1,x2) {
  if(Func(x1)*Func(x2) < 0)
  {
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
  
}

fBiseccion(0.2,1)