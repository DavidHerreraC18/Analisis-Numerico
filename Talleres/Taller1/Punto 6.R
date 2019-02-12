#Número a conocer la raiz
numero = 5



#Valor inicial de n
valorini= 4

Func = function(x) ((x^valorini)-numero)
Dx = function(x) (valorini*x^(valorini-1))
Hx <- function(x) x - (Func(x)/Dx(x))

newton <- function(a,b) {
  seqx = seq(a,b,0.1)
  plot(seqx,Func(seqx),type="l",col="red")
  if((Func(a)-a)*(Func(b)-b)<0)
  {
    
    x<-0.70
    r<-Hx(x)
    i<-0
    
    while (Func(r) != 0 ) 
    {    
      error<-abs(r-x)
      
      if(error > 1.e-8)
      {
        x<-r
        
      }
      else
      {
        break
      }
      
      r<-Hx(x)  
      i<-i+1
      points(rbind(c(x,Func(x)),pch=15,cex=0.4,col="green"))
      cat("I=",i,"\tF(x) =",Func(x),"\tX=",signif(x, digits = 8),"\tE=",error,"\n")
    }
    
  }
  else
  {
    cat("No tiene raíz la funcion en ese intervalo.\n")
  }
 
}

newton(-2,1)
