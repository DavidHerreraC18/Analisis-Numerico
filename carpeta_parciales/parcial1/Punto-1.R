#Punto 1

#a



#b(ii)


b= c(-exp(1),5,6,0)

# Se calculan ambas soluciones utilizando eliminación gaussiana
A0 = matrix(c(4,-1,-1,-1,
              -1,4,-1,-1,
              -1,-1,4,-1,
              -1,-1,-1,4), nrow=4, byrow=TRUE)
AM = matrix(c(4,-1,-1,-1,
              -1,4,-1,-1,
              -0.9,-1,4,-1,
              -1,-1,-1,4), nrow=4, byrow=TRUE)
print (A0)

# Se calculan las dos matrices, se usa solve() por conveniencia
X0=solve(A0,b)
XM = solve(AM,b)
print("Matriz original")
print(X0)
print("Matriz modificada")
print(XM)
x0 = matrix(X0)
xm = matrix(XM)
#Se calcula el error para verificar la variación en las soluciones
variacion = abs((norm(xm)-norm(x0)))/norm(x0)

cat("La variacion fue de:", variacion,"Su variacion porcentual fue de: ",variacion*100,"%")

#Se tiene que el numero de condicionamiento esta dado por cond(A) = ||A||||A^-1|| ( Formula en el libro de  Walter Mora F. -  Introducción a los métodos númericos)
nC = norm(A0,'i')*norm(solve(A0),'i')
#Y la cota en base al numero de condicionamiento es
cotaError = (nC*(norm(AM-A0, 'i'))/norm(A0))

cat("El numero de condicionamiento es: ", nC, "La cota de error es", cotaError," = ", cotaError*100,"%")