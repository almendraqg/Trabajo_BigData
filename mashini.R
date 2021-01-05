#############
#mashini
############

# Libreria indispensable para extraer información de sitios web:
library(rvest)

# Libreria para manipular bases de datos
library(tidyverse)

# Librerias para manejo de caracteres y otros datos
library(stringi)

library(ggplot2)

# Se crea data framme vacio
datos <- data.frame(
  "productos"="1",
  "precios"="0" ,
  "precios_oferta"="0" 
)

n=1

for (i in 1:n) {
  
  x="https://mashini.cl/collections/ropa-de-cama?page="
  y=as.character(n)
  url =  paste0(x,y)
  
  url <- read_html(url)
  
  productos <- html_text(html_nodes(url, ".title"))
  productos = gsub("[[:cntrl:]]", "", productos)
  productos  <- tolower(productos )
  productos <- stripWhitespace(productos)
  
  precios <- html_text(html_nodes(url, ".money"))
  seq <- seq(1, length(precios), by=2)
  seq2 <- seq(2, length(precios), by=2)
  
  precios_oferta <- 0
  precios_oferta <- precios[-c(seq)]
  precios <- precios[-c(seq2)]
  
  # Quitar el simbolo del dólar
  precios_oferta <- stri_replace_all(precios_oferta, "", fixed="$")
  precios <- stri_replace_all(precios, "", fixed="$")
  
  # Convertir a datos númericos
  precios_oferta <- as.numeric(precios_oferta)
  precios <- as.numeric(precios)
  
  # Unir las variables
  data <- cbind(productos,precios,precios_oferta)
  datos <- rbind(datos,data)
  
  Sys.sleep(2)
  
}

head(datos, 10)

# Eliminar la fila 1:
mashini <- datos[-1,]
mashini <- as.data.frame(mashini)

# Etiquetar los productos
etiquetas <-  substring(mashini$productos, 1,4)

# Unir la base de datos y etiqueta de producto
mashini <- cbind(mashini,etiquetas)
attach(mashini)

# Verificamos los elementos que se han extraido (incluyendo sus categorias y precios de oferta)
head(mashini, 20)

pl <- ggplot(mashini, aes(x=precios,y=etiquetas))
pl + geom_boxplot() + ggtitle("PRECIOS NORMAL | SÁBANAS, QUILTS, PLUMONES, ALMOHADAS")

pl <- ggplot(mashini, aes(x=precios_oferta,y=etiquetas))
pl + geom_boxplot()  + ggtitle("PRECIOS OFERTA | SÁBANAS, QUILTS, PLUMONES, ALMOHADAS")


