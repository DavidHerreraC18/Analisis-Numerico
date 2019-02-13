def fx(x):
#aqui se define la funcion f(x), la propuesta aca es un ejemplo: f(x)=x^2
	y=x^2
    
	return y
def raiz(a,b,E):

    d=(b+a)/10

    x=a

    y=fx(x)

    while(d>E):

        x= x+d

        yant=y

        y=fx(x)

        if ((y*yant)<0):

            x=x-d

            d=d/10

     return x