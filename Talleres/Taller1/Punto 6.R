     
    Newton <- function(x,n,v) x-(x^n-v)/(n*x^(n-1))
    FuncG <- function(x,n,v) x^n-v
    Func <- function(x,n) n*x^(n-1)
    Aitken <- function(x,x1,x2) x - (((x1-x2)^2)/(x2-2*x1+x))
    
    calcularRaiz <- function(v,n)
    {
       
       x <- v/2
       error <- 1
       i <- 0
       while(error > 1.e-8)
       {
         x1<-(Newton(x,n,v))
         x2<-(Newton(x1,n,v))
         xr<-(Func(x,x1,x2))
         
         if(Newton(xr,n,v)!=0)
         {
           x<-x1
         }
         else
         {
           break
         }
         
         error <- abs(FuncG(xr,n,v))/Func(xr,n)
         cat("Iteracion =", i,"\t x = ",xr,"\t   Error= ",error,"\n")
         i <- i + 1
         
       }
       
    
    }
    
    calcularRaiz2 <-function(v,n)
    {
      x <-v/2
      i <- 0
      error <-1
      while(error > 1.e-8)
      {
        x <-Newton(x,n,v)
        error <-abs(FuncG(x,n,v))/Func(x,n)
        i <- i + 1
        cat("Iteración",i,"x= ",x," \tError= ",error,"\n")
      }
    }
    
    
    calcularRaiz(53,3)
    calcularRaiz2(53,3)