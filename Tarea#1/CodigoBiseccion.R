

Func = function(x) {x*exp(x) - pi}



fBiseccion <- function(x1,x2) {
  if(Func(x1)*Func(x2) < 0)
  {
  x = seq(x1,x2,0.5)
  
  plot(x,Func(x),type="l",col="red")

  ptoM = (x1+x2)/2
  
  error = abs(x1-x2)/2
  
  i = 0
  
  aux = Func(ptoM)
  
  points(rbind(c(ptoM,aux)),pch=15,cex=0.4,col="blue")
  
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
  }
  
}

fBiseccion(0,2)
