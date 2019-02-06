Func = function(x) {exp(x) - x*pi}



fSecante <- function(x0,x1) {
  
    x = seq(x0,x1,0.1)
    
    plot(x,Func(x),type="l",col="red")
    
    abline(h=0)
    
    x2 = x1-(((x1-x0)*(Func(x1)))/(Func(x1)-Func(x0)))
    
    error = abs(x2-x1)/x2
    
    i = 0
    
    aux = Func(x2)
    
    points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
    
    cat("Iteracion=",i,"\tFunc(x)=",Func(x2),"\tX=",x2,"\tError=",error,"\n")
    
    while (error > 1.e-8) {
      
      x0 = x1
      
      x1 = x2
      
      x2 = x1-(((x1-x0)*(Func(x1)))/(Func(x1)-Func(x0)))
      
      i = i+1
      
      error = abs(x2-x1)/x2
      
      aux = Func(x2)
      
      points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
      
      cat("Iteracion=",i,"\tFunc(x)=",Func(x2),"\tX=",x2,"\tError=",error,"\n")
      
    }
  
}

fSecante(0.2,1)