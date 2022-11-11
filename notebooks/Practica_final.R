
#'**LIBRERIAS.Cargamos las librerias que necesesitaremos para la realización de la práctica.**
library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(purrr)


#'**EJERCICIO 1.  Crea un data frame o tibble a partir de los datos del fichero mktmix.csv. Usa la función clean_names() del paquete janitor para cambiar los nombres de columnas.**
#'
#'
#'
#'Cargamos el CSV y asignamos un nombre al DF.

df_mmm <- read_csv("../data/mktmix.csv")

#'Cambiamos el nombre de las columnas y sobreescribimos el nuevo DF bajo el mismo nombre de antes.

df_mmm <-  clean_names(df_mmm)


#'Comprobamos como han cambiado los nombres de las columnas.

names(df_mmm)

#'
#'
#'
#'

#'**EJERCICIO 2. ¿Cuántas columnas tiene el data frame? ¿Y filas? ¿Cuáles son las clases de las columnas base_price y discount? Explica qué información crees que contienen.**
#'
#'
#'
#'Calculamos nº de filas y columnas respectivamente. Observamos 104 filas y 9 columnas.

nrow(df_mmm)
ncol(df_mmm)

#'Comprobamos la clase de las columnas *'base_price'* y *discount'*:

class(df_mmm$base_price)
class(df_mmm$discount)

#'
#'El DF al parecer contiene información acerca de productos vendidos a través de diferentes campañas de Marketing. 
#'Concretamente, 'base_price' muestra el precio base de un producto y 'discount' el descuento ofrecido en esa campaña para el producto.
#'
#'

#'**EJERCICIO 3. La clase de newspaper_inserts es character. Cambia sus valores para que sea numérica de la siguiente forma: todos los valores NA deben ser 0; los demás, 1.**
#'
#'
#'
#'Comprobamos cuales son los valores que no son NA en la columna *'newspaper_isnerts'*, de manera que los que no lo sean recibirán el valor TRUE y los que si, el valor FALSE.
#'Y le asignamos a esa función el nombre de *'conversion'*.


conversion<- !is.na(df_mmm$newspaper_inserts)
 
#'Ahora convertimos como numéricos los valores recibidos de *'conversion'*. Para que estos pasen a valores 1 y 0 respectivamente y 
#'los sobreescribimos sobre la columna original, 'newspaper_inserts'. 

df_mmm$newspaper_inserts <- as.numeric(conversion)

glimpse(df_mmm)


#'**EJERCICIO 4.¿Cuántos valores distintos (o únicos) hay en la columna website_campaign (NA no cuenta)? Crea nuevas columnas para cada una de estas categorías, definidas como 1 si website_campaign toma el valor de esa categoría y 0 en caso contrario. Por ejemplo, si una de las categorías de website_campaign es "Google", crea una columna nueva llamada google que valga 1 en los registros en los que website_campaign valga "Google" y 0 en los demás**

df_mmm %>% 
  distinct(website_campaign)

#'Encontramos 3 valores únicos sin contar los NA: *'Facebook'*, *'Twitter'* y *'Website Campaign'*.
#'
#'
#'
#'Pasamos los valores NA a al valor 10, siguiendo la recomendación.

prueba <- df_mmm %>% mutate(website_campaign = ifelse(is.na(website_campaign),10, website_campaign))
tail(prueba)

#'Una vez transformados los NA, procedemos a crear las nuevas columnas de *Twitter* y *Facebook*, dejando que en la columna website campaigns aparezan 1 y 0 si corresponden con el valor:


df_mmm <-prueba  %>% mutate(
  Facebook = ifelse(website_campaign %in% 'Facebook', 1,0),
  Twitter= ifelse(website_campaign %in% 'Twitter', 1,0),
  website_campaign =ifelse(website_campaign %in% 'Website Campaign', 1,0))

head(df_mmm)

#'**EJERCICIO 5. Cada fila de la tabla representa una semana en el histórico. Calcula cuántas semanas hahabido campaña de Facebook y cuántas semanas ha habido campaña de Twitter (hazlo con las columnas que calculaste en el ejercicio anterior).**
#'

df_mmm %>%  
  summarise(sum(Facebook), sum(Twitter))


#'Observamos que hubo 4 semanas de campaña en Facebook y otras 4 semanas en Twitter.
#'
#'
#'
#'**EJERCICIO 6. La columna tv indica la cantidad de inversión que se ha hecho en anuncios de televisión. La unidad es grp. ¿Cuántas semanas se ha realizado una inversión de menos de 50 grp?**
#'
#'
#'


df_mmm %>% 
  select(tv, everything()) %>% 
  filter(tv <50) %>% nrow()

#'Observamos que la inversión en tv ha sido menor que 50 grp unicamente 3 semanas.
#'
#'
#'
#'**EJERCICIO 7. Calcula la media de inversión en tv durante las semanas en las que hubo inversión en radio y en durante las que no hubo (aquellas en las que radio sea NA). ¿Qué media es mayor?**
#'
#'
#'Primero vamos a pasar los NA a 0 para que al agruparlos podamos calcular la media directamente.

df_mmm_1 <- df_mmm %>% 
            mutate(radio = ifelse(is.na(radio),0, radio)) 

  
#'
#'


df_mmm_1 %>%  
  mutate(invertido_radio= as.integer((radio!=0))) %>%  
  group_by(invertido_radio) %>% 
  summarise(mediaTV = mean(tv)) 

#'Observamos que la media de inversion en TV es mayor aquellas semanas donde no hubo inversión en radio.
#'
#'
#'
#'**EJERCICIO 8. Crea un gráfico de líneas con ggplot2 que muestre la evolución de new_vol_sales, que es la columna con datos sobre las ventas del producto. Como no tienes datos de fechas, tendrás que inventarte el eje x: haz que sean valores 1, 2, 3, 4… hasta el número de filas.**
#'
#'Utilizamos como eje X, el rango de filas del DF.

ggplot(df_mmm) +
  geom_line(aes(x=(1:104) ,y= new_vol_sales))


#'**EJERCICIO 9. Crea un histograma y un boxplot de esa variable, new_vol_sales. A ojo, a partir de los gráficos, ¿cuál dirías que es la mediana de la variable? Y sin hacer cálculos, ¿crees que la media es mayor o menor?**
#'
#'
#'
#'Creamos las gráficas.

ggplot(df_mmm) +
  geom_histogram(aes(x=new_vol_sales ), col = 'black',,fill ='darkgreen')

ggplot(df_mmm) +
  geom_boxplot(aes(x=new_vol_sales))


#'La mediana parece encontrarse por la zona de los 20000, basandome en que hay mayor frecuencia de conteo por esa zona, como observamos en el histograma. La media parece que será mayor a la mediana, dada las dimensiones de la caja.
#'
#'
#'

#'**EJERCICIO 10. Crea un data frame o tibble nuevo que tenga solo las columnas tv, radio y stout. Son las columnas que tienen datos de medios publicitarios: televisión, radio y exterior (carteles de esos que ves en la calle o en la carretera). Usa ese data frame para generar el gráfico siguiente.**
#'
#'
#'
#'
#'Creamos el Dataframe : *'df_media'*, únicamente con las columnas _'tv, 'radio'_ y _'stout'_.

df_media <- df_mmm %>% 
            select(tv, radio, stout)



head(df_media)

#'Ejecutamos el código del enunciado:


df_media <- df_media %>%
            pivot_longer(everything())

#'Construimos los gráficos.
#'
#'
ggplot(df_media) +
  geom_line(aes(x= 1:312, y = value),size=1) +
  facet_wrap( ~ name  ,dir = 'v' , strip.position = 'right',scales = 'free')

#'
#'
#'Observamos que la inversión en radio en diferentes momentos ha sido nula,
#'mientras que la de cartelería se ha visto como ha ido creciendo independientemente de las otras variables.
#'En cuanto a la inversión en tv vemos como la primera vez que se dejó de invertir en radio supuso un aumento de esta,
#'sin embargo, la segunda vez, cayó también, e incluso se ha ido reduciendo un poco.
#'Además el volumen de inversión en radio es mayor que el de televisión, seguido por la cartelería en último lugar.
#'
#'
#'
#'
#'**EJERCICIO 11. La columna in_store mide el stock disponible que hay en las tiendas para vender el producto, de manera indexada. Crea un gráfico de dispersión con ggplot que compare new_vol_sales frente a in_store. Presta atención a qué columna pondrás en el eje 𝑥 y cuál en el 𝑦: para ello, ten en cuenta que new_vol_sales será la variable objetivo del modelo, i.e., el analista explicar esa variable en función de las demás. Además, añade una capa con geom_smooth(): los ejes 𝑥 e 𝑦 serán los mismos que pongas en el geom del gráfico de dispersión. Comenta qué conclusiones sacas a la vista del gráfico.**
#'
#'
#'
#'Seleccionamos stock como variable independiente (Eje x), y el volumen de ventas como dependiente, dado que el stock es controlado mientras que las ventas dependen del stock disponible.


df_mmm %>% 
  select(in_store, new_vol_sales) %>% 
  
  ggplot(aes(x=in_store, y = new_vol_sales)) +
  geom_point()+
  geom_smooth( method = 'lm')

#'Tal y como observamos en la gráfica, según van creciendo el número de stock disponible, el volumen de ventas aumenta proporcionalmente.
#'
#'
#'

#'**EJERCICIO 12. Repite el gráfico anterior pero de dos formas diferentes (no pongas geom_smooth esta vez).Colorea cada punto en función de la columna newspaper_inserts; Colorea cada punto de manera diferente en función de la columna tv.**
#'
#'
#'

#'Coloreamos los puntos en función de 'newspaper_insert'. 

ggplot(df_mmm) +
  geom_point(aes(x=in_store, y = new_vol_sales ,colour= as.factor(newspaper_inserts)))

#'Coloreamos los puntos en función de 'tv, utilizamos el paquete RColorBrewer para diferenciar mejor los colores.

ggplot(df_mmm,aes(x=in_store, y = new_vol_sales)) +
  geom_point(aes(x=in_store, y = new_vol_sales,col= tv)) +
  scale_color_viridis(discrete= FALSE)


#'**EJERCICIO 13. Crea otra columna indicando si se ha aplicado descuento o no (es decr, si la columna discount es mayor que 0 o no). Puedes llamarla discount_yesno, por ejemplo, y puede ser numérica o logical. Luego agrega el data frame calculando la media de base_price para los casos en los que hay descuento y los casos en los que no. O sea, el resultado será un data frame de dos filas y dos columnas. Usa este data frame para crear un gráfico de columnas.**
#'

df_mmm %>% 
  mutate(discount_yesno = as.logical((discount >0))) %>%
  group_by(discount_yesno) %>% summarise(media_base_price = mean(base_price)) %>% 
      ggplot(aes(x=discount_yesno, y= media_base_price),scale= 'free') +
      geom_bar(stat="identity", fill='darkgreen')

#'Los casos SIN descuento son mayores que CON descuento, pero con una diferencia leve.
#'
#'

#'**EJERCICIO 14. Apóyate en el siguiente código para crear una función que ajuste un modelo de regresión en los datos. La función recibirá como entrada un vector character con las variables que se quieran usar como explicativas en el modelo. Con ese vector, dentro de la función crea un nuevo data frame que tenga esas variables y la variable explicativa, new_vol_sales. Este data frame lo usarás para ajustar el modelo siguiendo el código siguiente.Finalmente, llama a la función usando como vector de entrada c("tv", "radio").**
#'
#'
#'
#'Creamos la función.

inserta<- function(un_vector_character){
  
    df_aux<- df_mmm %>%select(un_vector_character, new_vol_sales) 
    
    my_model <- lm(new_vol_sales ~ ., data = df_aux)
    
    resumen<- summary(my_model)$adj.r.squared
    
  return(resumen)
}


#'Insertamos los valores de _'tv'_ y _'radio'_.

  
inserta(c('tv', 'radio'))

#'Observamos que el resultado al ser tan próximo a 0 demuestra que no es eficiente al tener bastante error. 
#'
#'
#'


#'**EJERCICIO 15. Debajo tienes tres vectores con nombres de variables. Crea una lista con esos vectores, es decir, una lista de tres elementos. ¿Qué modelo es el mejor, de acuerdo al R cuadrado ajustado?**
#'
#'
#'
#'Creamos la lista de tres vectores.

lista <- list(c("base_price", "radio", "tv", "stout"),c("base_price", "in_store", "discount", "radio", "tv", "stout"),c("in_store", "discount"))

lista
     
#'Calculamos el R cuadrado ajustado de los tres modelos.
 
map_dbl(lista, inserta)

#'Observamos que el mejor modelo es el segundo, que contiene _'base price','in store' , 'discount', 'radio', 'tv' y 'stout'_ al tener el R cuadrado ajustado más alto, por lo que tiene menor error que el resto de modelos.

