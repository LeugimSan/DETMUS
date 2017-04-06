#########################################################################
## DETMUS: A Deterministic Music Generator by Images
## Developers: Alex Rodriguez Antolin & Miguel Angel Rodriguez Muinos
## Contact: mail[at]leugimsan.es
## From: Madrid, Spain
## Version: 1.0
## creation Date: 2017/03/08
## Last Version Date: 2017/04/06
## License: GNU/GPL v3.0
#########################################################################

# Carga de librerias necesarias
library(tuneR)
library(imager)
library(gWidgets2)
options(guiToolkit="tcltk")
require(gWidgets2tcltk)

# Establecimiento de la carpeta de trabajo 
# OJO: Cambiar a las carpetas correctas!!!!! 
# ... en todos los sitios que aparecen en este código
setwd("C:/Users/mrodmui/Desktop/DETMUS")

# Función 
detmus=function()
{
  options(guiToolkit="tcltk")
  estilos=c("Please, Select a Music Style...", 
            "Classical", 
            "Vocal",
            "DubStep, TriHop, ...",
            "Ambiental",
            "Electronic",
            "Noise")
  
  win=gwindow("DETMUS")
  group=ggroup(horizontal=FALSE, container=win)
  texto=glabel("A Deterministic Music Generator by Images", container=group, font.attr=list(style="bold"))
  addSpring(group)
  addSpace(group,15)
  estilo=gcombobox(estilos, container=group)
  addSpring(group)
  boton=gbutton("Run", container=group, 
                handler=function(h,...)
                {eleccion=svalue(estilo)
                 print(eleccion)
                 if (eleccion==estilos[2]) 
                   classical()
                 else
                   if (eleccion==estilos[3]) 
                     vocal()
                 else
                   if (eleccion==estilos[4]) 
                     hop()
                 else
                   if (eleccion==estilos[5]) 
                     ambiental()
                 else
                   if (eleccion==estilos[6]) 
                     electronic()
                 else
                   if (eleccion==estilos[7]) 
                     noise()
                 else
                   print("Please, Select a Style.")
                }
  )
}


classical=function()
{

# Carga de la imagen a procesar
image.import=gfile("Select IMAGE to import...",filter="*.*")
image=load.image(image.import)
# Esto nos pinta la imagen 
# (sólo para comprobar que se carga correctamente)
# no la carga con el aspect-ratio adecuado pero eso no influye
plot(image)
class(image)
dim(image)
x=dim(image)[1]
y=dim(image)[2]
cc=dim(image)[4]

#Procesado de la matriz numérica que vamos a usar para generar los sonidos

# Forma antigua
# image.data=as.data.frame(image) <- Forma antigua
# secuence=image.data$value
# secuence.int=secuence*1000000000
# secuence.end=trunc(secuence.int)
# len=length(secuence.end)

image.data=matrix(image,ncol=3)
secuence=cbind(image.data,image.data[,1]*image.data[,2]*image.data[,3])
# aqui falta 'colapsar' las columnas de la matriz para que quede de la longitud de la fila (por ejemplo)
# ahora mismo la longitud es x*y (muy grande 'tocar' todos los puntos del cuadro)
secuence.int=secuence[,4]*100000000000
secuence.end=trunc(secuence.int)


# Generación del TRACK de Violín
setwd("C:/Users/mrodmui/Desktop/DETMUS/LOOPS/POSS/violin_C/")
# duracion=x*y 
duracion=50
data.music=secuence.end%%416
sample=data.frame(head(data.music,duracion))
Wobj=vector("list",duracion)
Wobj2=readMP3("0.mp3")
contador=1
for (contador in 1:duracion)
{
pista=sample[contador,]
fichero=paste0(pista,".mp3")
Wobj[contador]=readMP3(fichero)
Wobj2<-bind(Wobj2,Wobj[[contador]])
}
setwd("C:/Users/mrodmui/Desktop/DETMUS/TRACKS_GENERADOS")
writeWave(Wobj2, filename = "Track_violin.wav")
setwd("C:/Users/mrodmui/Desktop/DETMUS")

# Generación del TRACK de Flauta
setwd("C:/Users/mrodmui/Desktop/DETMUS/LOOPS/POSS/flute_C/")
# duracion=x*y 
duracion=20
data.music=secuence.end%%200
sample=data.frame(head(data.music,duracion))
Wobj=vector("list",duracion)
Wobj2=readMP3("0.mp3")
contador=1
for (contador in 1:duracion)
{
  pista=sample[contador,]
  fichero=paste0(pista,".mp3")
  Wobj[contador]=readMP3(fichero)
  Wobj2<-bind(Wobj2,Wobj[[contador]])
}
setwd("C:/Users/mrodmui/Desktop/DETMUS/TRACKS_GENERADOS")
writeWave(Wobj2, filename = "Track_flauta.wav")
setwd("C:/Users/mrodmui/Desktop/DETMUS")


}




# Generación del TRACK de Percusión (TR-909)
# setwd("C:/Users/mrodmui/Desktop/DETMUS/LOOPS/ROLAND_TR-909/")
# duracion=50
# data.music=secuence.end%%161
# sample=data.frame(head(data.music,duracion))
# Wobj=vector("list",duracion)
# Wobj2=readWave("0.wav")
# contador=1
# for (contador in 1:duracion)
# {
# pista=sample[contador,]
# fichero=paste0(pista,".wav")
# Wobj[contador]=readWave(fichero)
# Wobj2<-bind(Wobj2,Wobj[[contador]])
# }
# setwd("C:/Users/mrodmui/Desktop/DETMUS/TRACKS_GENERADOS")
# writeWave(Wobj2, filename = "Track_TR909.wav")
# setwd("C:/Users/mrodmui/Desktop/DETMUS")


# Ahora a producir los tracks generados de la carpeta TRACKS_GENERADOS
# con nuestro software preferido

### FIN del Programa. 


 