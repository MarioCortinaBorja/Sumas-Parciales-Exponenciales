library(ggplot2)
library(easyGgplot2)


plot.expsum<-
function(expr=NULL, fecha=NULL, coef2=NULL, asintota=NULL, gauss=NULL,
         titulo=NULL, N0=1, N1=5000, ejes=TRUE,
         xname = "x",  color='red', archivo=NULL, regresa=FALSE)
{
  ### Sólo uno de los argumentos expr, fecha, coef2, asintota puede ser !is.null 
  ### fecha debe ser "d.m.a" por día, mes y año
  ### a debe tener dos dígitos; m y d puede tener uno o no más de dos dígitos
  ### expr debe tener xname como argumento, por ejemplo expr = (log(x))^4
  ### coef2 es un número real que multiplica a sqrt(x)
  ### asintota es un vector de longitud 4 con componentes (theta, p, m, n)
  ### N0, N1 son los límites de la suma exponencial
  ### NB - library(ggplot2) debe estar la sesión de R
  ### color es el color de la gráfica; si ejes es TRUE incluye ejes
  ### archivo es el nombre del pdf; si es NULL no genera un archivo
  ### si regresa es TRUE la función resulta en el data.frame df,
  ### de otra forma regresa la gráfica p1
  ### MCB, Cambridge, 03.12.2017; in memoriam MBM ob. 03.12.1990
  ### 
  N<- N0:N1  ### valores sobre los que se evalúa la función
  if(is.null(fecha) & is.null(coef2) & is.null(asintota) & is.null(gauss))### expr
  { ### basado en la función curve{graphics}
    sexpr <- substitute(expr)
    if (is.name(sexpr)) 
      expr <- call(as.character(sexpr), as.name(xname))
    else 
    {
      if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% all.vars(sexpr))) 
        stop(gettextf("'expr' debe ser una función, 
                            o un call o una expresión con '%s'",
                            xname), domain = NA)
      expr <- sexpr
      titulo <- ifelse(is.null(titulo), paste('suma exponencial con f =', 
                                                     deparse(expr)), titulo)
      ll <- list(x = N)
      names(ll) <- xname
      f <- eval(expr, envir = ll, enclos = parent.frame())
      y<- cumsum( exp(2*pi* (0 + 1i) * f))
      if (length(y) != length(N)) 
          stop("'expr' resultó en un objeto de longitud diferente a 'N'")
    }
  } ### fin expr
  if(is.null(expr) & is.null(coef2) & is.null(asintota) & is.null(gauss)) ### fecha
  { 
    aux<- gregexpr("[.]", fecha)[[1]]
    if(length(aux)!=2) stop('especifica la fecha en la forma d.m.a')
    d<-as.numeric(substring(fecha,1,aux[1]-1) )
    m<-as.numeric(substring(fecha,aux[1]+1,aux[2]-1))
    a<-as.numeric(substring(fecha,aux[2]+1,nchar(fecha)))
    if(a==0) stop('a debe ser distinto a 0')
    titulo <- ifelse(is.null(titulo), 
                       paste('suma exponencial; fecha =', fecha,sep=''), titulo)
    f<- function(n) { cumsum( exp(2 * pi * (0+1i)  *(n/d+ n^2/m + n^3/a) ))} 
    y<-f(N)
  } ### fin cumpleaños
  if(is.null(fecha) & is.null(expr) & is.null(asintota) & is.null(gauss))### coef2
  {
    titulo <- ifelse(is.null(titulo),
                     paste('suma exponencial; gaussiana =', 
                     paste(coef2, "x^(1/2)", sep='*'),sep=''),
                     titulo)
    f<- function(n) { cumsum( exp(2 * pi * (0+1i)  * coef2 * sqrt(n) ))} 
    y<-f(N)
  } ### fin gaussiana con raíz cuadrada
  if(is.null(fecha) & is.null(expr) & is.null(coef2)  & is.null(gauss))### asíntota
  { 
    theta<- asintota[1]; m<- asintota[2]; p<-asintota[3];  n<- asintota[4]
    titulo <- ifelse(is.null(titulo), 
                     paste('asintótica: ', 
                     paste(c('theta','m','p', 'n'), 
                             c(round(theta,1), m, p,   n), sep='=', collapse=', '), 
                             sep=' '),  titulo)
    N<- 1:n
    f<-exp(-N/m - (0 + 1i)*theta*exp(-p*N/m)) 
    f<-c( exp( -(0 + 1i)*theta)/2, f) ### sigma primada
    y<-cumsum(f)
  } ### fin asíntota
  if(is.null(fecha) & is.null(expr) & is.null(coef2) & is.null(asintota)) 
  ### gaussiana ordinaria
  { 
    titulo <- ifelse(is.null(titulo), 
                     paste(c('suma gaussiana (p,N)=', gauss), 
                     sep='', collapse=' '),   titulo)
    N<- 0:gauss[2]
    f<-exp( 2*pi*(0 + 1i)*(N^gauss[1])/gauss[2])
    y<-cumsum(f)
  } ### fin gauss
  
  ### gráfica
  df<- data.frame(x=Re(y), y=Im(y))
  p1<- ggplot(df, aes(x=x,y=y)) + geom_path(color=color) + 
    coord_equal(ratio=1) +  theme(aspect.ratio=1) + theme_bw() +
    ylab("") + xlab("") +
    theme( panel.border= element_blank(),
           axis.text.x=element_blank(), axis.ticks.x=element_blank(),
           axis.text.y=element_blank(), axis.ticks.y=element_blank(),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank()) +
           ggtitle(titulo) +  theme(plot.title = element_text(hjust = 0.5)) 
  if (ejes) p1<- p1 + geom_hline(yintercept=0) + geom_vline(xintercept=0)
  print(p1)
  if(!is.null(archivo)) ggsave(filename=paste(archivo,'.pdf'), 
                                      plot=p1, device='pdf')
  if(regresa) return(invisible(df))
  else return(invisible(p1))
}


########################## ejemplos
###############################################################################

### figura 1

p1<- plot.expsum2(gauss=c(1, 1024), regresa=FALSE)
p2<- plot.expsum2(gauss=c(1.5, 1024), regresa=FALSE)
p3<- plot.expsum2(gauss=c(2, 1024), regresa=FALSE)
p4<- plot.expsum2(gauss=c(3, 1024), regresa=FALSE)

ggsave(filename='Plots/Fig1.jpeg', plot=ggplot2.multiplot(p1,p2, p3,p4, cols=2), 
       device='jpg', dpi=600)

### figura 2

p1<-plot.expsum2(gauss=c(2, 1002)) ### traversed
p2<-plot.expsum2(gauss=c(2, 1003)) ### nice

ggsave(filename='Plots/Fig2.jpeg', plot=ggplot2.multiplot(p1,p2, cols=2), 
       device='jpg', dpi=600)



### figura 3



p1<- plot.expsum2(coef2=1, N1=500)
p2<- plot.expsum2(coef2=10, N1=500)
p3<- plot.expsum2(coef2=100, N1=500)
p4<- plot.expsum2(coef2=1000, N1=500)


ggsave(filename='Plots/Fig3.jpeg', plot=ggplot2.multiplot(p1,p2, p3,p4, cols=2), 
       device='jpg', dpi=600)


### Figura 4
p1<- plot.expsum2(coef2=1, N1=5000)
p2<- plot.expsum2(coef2=10, N1=5000)
p3<- plot.expsum2(coef2=100, N1=5000)
p4<- plot.expsum2(coef2=1000, N1=5000)


ggsave(filename='Plots/Fig4.jpeg', plot=ggplot2.multiplot(p1,p2, p3,p4, cols=2), 
       device='jpg', dpi=600)


### Figura 5
p1<- plot.expsum2(coef2=1024, N0=0, N1=1024) ### ex chaos
p2<- plot.expsum2(coef2=1024, N0=0, N1=2048) ### ex chaos
p3<- plot.expsum2(coef2=1024, N0=0, N1=4096) ### ex chaos
p4<- plot.expsum2(coef2=1024, N0=0, N1=40096) ### ex chaos


ggsave(filename='Plots/Fig5.jpeg', plot=ggplot2.multiplot(p1,p2, p3,p4, cols=2), 
       device='jpg', dpi=600)


### Figura 6 - espiral

ggsave(filename="Plots/Fig6.jpeg", plot=plot.expsum1(expr=log(x)))

ggsave(filename="Plots/Fig7.jpeg", plot=plot.expsum1(expr=log(x)^4, N0=1, N1=5000))
### monstruo de Loch Ness

ggsave(filename="Plots/Fig8.jpeg", plot=plot.expsum1(expr=log(x)^7, N0=1, N1=5000))
## Vía Láctea




