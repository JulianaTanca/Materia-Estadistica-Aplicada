# Intervalos de confianza para la diferencia de medias (3 casos).
# Seleccionar código según el caso que corresponda.

x = c(428,419,458,439,441,456,463,429,438,445,441,463)
y = c(462,448,435,465,429,472,453,459,427,468,452,447)

# Testeo normalidad (p-valor > 0.15 es normal)

shapiro.test(x)
shapiro.test(y)

# Testeo igualdad de varianzas (p-valor > 0,15 no rechazo H0)

var.test(x,y)

# Promedio muestral 

x_raya=mean(x)
y_raya=mean(y)
  
# Varianza muestral 

var_x=var(x)
var_y=var(y)

# Desvio muestral 

s1=sd(x)
s2=sd(y)

n1=12
n2=12

# Nivel del intervalo de confianza 
# En este ejemplo es del 90 % por eso el 0.9

alfa=(1-0.99)

# Calculamos el intervalo de confianza para la diferencia de medias de dos poblaciones 
# Se tiene el caso de normales con varianzas desconocidas y distintas. (caso 3)

k= trunc((s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1)))

a= x_raya-y_raya-qt(alfa/2,k,lower.tail=F)*(sqrt(s1^2/n1+s2^2/n2))
b= x_raya-y_raya+qt(alfa/2,k,lower.tail=F)*(sqrt(s1^2/n1+s2^2/n2))
IC=c(a,b)


# Calculamos el intervalo de confianza para la diferencia de medias de dos poblaciones 
# normales con varianzas iguales y desconocidas (caso 2).

sp= ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2) 
a= x_raya-y_raya-qt(alfa/2,n1+n2-2,lower.tail=F)*(sqrt(sp*(1/n1+1/n2)))
b= x_raya-y_raya+qt(alfa/2,n1+n2-2,lower.tail=F)*(sqrt(sp*(1/n1+1/n2)))
IC=c(a,b)


# Calculamos el intervalo de confianza para la diferencia de medias de dos poblaciones
# normales con varianzas conocidas (caso 1)
# Acá necesitamos cargar los valores de sigma1 y sigma2.

n1=15
n2=15
sigma1=3
sigma2=3
a= x_raya-y_raya-qnorm(alfa/2,lower.tail=F)*(sqrt(sigma1^2/n1+sigma2^2/n2))
b= x_raya-y_raya+qnorm(alfa/2,lower.tail=F)*(sqrt(sigma1^2/n1+sigma2^2/n2))
IC=c(a,b)

# Calculo el intervalo de confianza con t.test
t.test(x,y,alternative="two.sided",var.equal=T,conf.level=0.9)
# Solo sirve t.test para varianzas iguales y desconocidas (caso 2).
