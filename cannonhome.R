#############
#cannonhome
############

# Libreria indispensable para extraer información de sitios web:
library(rvest)

# Libreria para manipular bases de datos
library(tidyverse)

# Librerias para manejo de caracteres y otros datos
library(stringi)
library(stringr)

library(ggplot2)

# Se crea data framme vacio
datos <- data.frame(
  "productos"="1",
  "precios"="0" ,
  "precios_oferta"="0" 
)

n=10

for (i in 1:n) {
  
  x="https://cannonhome.cl/12-ropa-de-cama?p="
  y=as.character(n)
  url =  paste0(x,y)
  
  url <- read_html(url)
  
  productos <- html_text(html_nodes(url, ".product-item-link"))
  productos = gsub("[[:cntrl:]]", "", productos)
  productos  <- tolower(productos )
  productos <- stripWhitespace(productos)
  
  precios <- html_text(html_nodes(url, ".price_ammount"))
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

head(cannonhome, 10)

# Eliminar la fila 1:
cannonhome <- datos[-1,]
cannonhome <- as.data.frame(cannonhome)

# Etiquetar los productos
etiquetas <-  substring(cannonhome$productos, 1,4)

# Unir la base de datos y etiqueta de producto
cannonhome <- cbind(cannonhome,etiquetas)
attach(cannonhome)

# Verificamos los elementos que se han extraido (incluyendo sus categorias y precios de oferta)
head(cannonhome, 20)

pl <- ggplot(cannonhome, aes(x=precios,y=etiquetas))
pl + geom_boxplot() + ggtitle("PRECIOS NORMAL | SÁBANAS, COJÍNES, PLUMONES, QUILTS")

pl <- ggplot(cannonhome, aes(x=precios_oferta,y=etiquetas))
pl + geom_boxplot()  + ggtitle("PRECIOS OFERTA | SÁBANAS, COJÍNES, PLUMONES, QUILTS")



