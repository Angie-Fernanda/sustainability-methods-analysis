# Cargar librerías necesarias
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(forcats)

# Definir el directorio donde están los archivos
carpeta <- "D:/2024/Google Drive/UDENAR/Proyectos/Proyecto copoazu/Artículo 5/WOS_36000"

# Listar todos los archivos .txt de la carpeta
archivos_txt <- list.files(path = carpeta, pattern = "\\.txt$", full.names = TRUE)

# Leer y unir el contenido de todos los archivos
contenido_unido <- lapply(archivos_txt, readLines)
contenido_unido <- unlist(contenido_unido)

# Paso 1: Unir todo el contenido y normalizar texto (minúsculas, sin acentos, sin puntuación)
texto_completo <- paste(contenido_unido, collapse = " ")
texto_completo <- tolower(texto_completo)
#texto_completo <- stri_trans_general(texto_completo, "Latin-ASCII")  # elimina acentos
#texto_completo <- str_replace_all(texto_completo, "[[:punct:]]", " ") # elimina signos de puntuación
texto_completo <- str_squish(texto_completo) # elimina espacios múltiples

# Crear una lista con sigla y nombre completo para cada metodología
metodologias <- list(
  "LCA" = "Life Cycle Assessment",
  "PCA" = "Principal Component Analysis",
  "MCA" = "Multiple Correspondence Analysis",
  "IDEA" = "Indicateurs de Durabilité des Exploitations Agricoles",
# "EEEC" = "Energy-Economy-Environmental-Cycle",
  "SCLA" = "Social Life Cycle Assessment",
  "RISE" = "Response-Inducing Sustainability Evaluation",
  "SAFA" = "Sustainability Assessment of Food and Agriculture systems",
  "SMART-Farm" = "SMART-Farm",
  "MESMIS" = "Framework for the Evaluation of Natural Resource Management Systems",
  "RAPTA" = "Resilience, Adaptation Pathways and Transformation Assessment",
# "Remote sensing" = "Remote sensing",
  "TAPE" = "Tool for Agroecological Performance Evaluation",
  "FESLM" = "Framework for Evaluating Sustainable Land Management",
  "DPSIR" = "Driver–Pressure–State–Impact–Response",
  "GSI" = "General Sustainability Index"
)

# Unir todo el texto y pasar a minúsculas
texto_completo <- tolower(paste(contenido_unido, collapse = " "))

# Contar menciones combinadas (sigla + nombre completo)
frecuencias <- sapply(names(metodologias), function(sigla) {
  nombre_completo <- metodologias[[sigla]]
  str_count(texto_completo, fixed(tolower(sigla))) + 
    str_count(texto_completo, fixed(tolower(nombre_completo)))
})

# Crear dataframe para graficar
df_frecuencias <- data.frame(
  Metodología = names(metodologias),
  Frecuencia = frecuencias
) %>%
  arrange(Frecuencia) %>%
  mutate(Metodología = fct_inorder(Metodología))

# Graficar (etiquetas en inglés y escala logarítmica)
A<- ggplot(df_frecuencias, aes(x = log(Frecuencia + 1), y = Metodología)) +  # log + 1 para evitar log(0)
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(
    title = "A",
    x = "Log of frequency",
    y = "Sustainability assessment methodology"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 0.8)
  )
A


### Año y metodologías ------------
head(contenido_unido, n=200)

# Paso 1: Extraer solo líneas que empiecen con "TI" o "PY"
lineas_filtradas <- grep("^(AB|PY)\\s", contenido_unido, value = TRUE)

# Paso 2: Asegurar que hay un número par de líneas (TI seguido por PY)
lineas_filtradas <- lineas_filtradas[1:(length(lineas_filtradas) - length(lineas_filtradas) %% 2)]

# Paso 3: Extraer títulos y años en orden
titulos <- lineas_filtradas[seq(1, length(lineas_filtradas), 2)]
anios   <- lineas_filtradas[seq(2, length(lineas_filtradas), 2)]

# Paso 4: Eliminar el prefijo ("TI " y "PY ")
titulos <- str_remove(titulos, "^AB\\s+")
anios   <- str_remove(anios, "^PY\\s+")

# Paso 5: Crear el data frame emparejado correctamente
df_sustentabilidad <- data.frame(
  TI = titulos,
  PY = as.integer(anios),
  stringsAsFactors = FALSE
)

# Verificar
head(df_sustentabilidad)

# Crear una lista con sigla y nombre completo para cada metodología
metodologias <- list(
  "LCA" = "Life Cycle Assessment",
  "PCA" = "Principal Component Analysis",
  "MCA" = "Multiple Correspondence Analysis",
  "IDEA" = "Indicateurs de Durabilité des Exploitations Agricoles",
  # "EEEC" = "Energy-Economy-Environmental-Cycle",
  "SCLA" = "Social Life Cycle Assessment",
  "RISE" = "Response-Inducing Sustainability Evaluation",
  "SAFA" = "Sustainability Assessment of Food and Agriculture systems",
  "SMART-Farm" = "SMART-Farm",
  "MESMIS" = "Framework for the Evaluation of Natural Resource Management Systems",
  "RAPTA" = "Resilience, Adaptation Pathways and Transformation Assessment",
  # "Remote sensing" = "Remote sensing",
  "TAPE" = "Tool for Agroecological Performance Evaluation",
  "FESLM" = "Framework for Evaluating Sustainable Land Management",
  "DPSIR" = "Driver–Pressure–State–Impact–Response",
  "GSI" = "General Sustainability Index"
)

# Crear una versión normalizada de la columna TI
df_sustentabilidad <- df_sustentabilidad %>%
  mutate(TI_norm = str_squish(str_remove_all(
    stri_trans_general(tolower(TI), "Latin-ASCII"), 
    "[[:punct:]]"
  )))

# Función para buscar sigla si aparece el nombre o sigla en TI_norm
buscar_metodologia <- function(titulo) {
  for (sigla in names(metodologias)) {
    nombre <- tolower(metodologias[[sigla]])
    nombre <- stri_trans_general(nombre, "Latin-ASCII")
    nombre <- str_replace_all(nombre, "[[:punct:]]", "")
    if (str_detect(titulo, fixed(tolower(sigla))) || str_detect(titulo, fixed(nombre))) {
      return(sigla)
    }
  }
  return(NA)
}

# Eliminar columna auxiliar
library(dplyr)
library(stringi)
library(stringr)

# Paso 1: Normalizar títulos
df_sustentabilidad <- df_sustentabilidad %>%
  mutate(TI_norm = str_squish(str_remove_all(
    stri_trans_general(tolower(TI), "Latin-ASCII"), "[[:punct:]]"
  )))

# Paso 2: Clasificar usando dplyr::case_when (vectorizado y mucho más rápido)
df_sustentabilidad <- df_sustentabilidad %>%
  mutate(
    Metodologia = case_when(
      str_detect(TI_norm, "life cycle assessment|\\blca\\b") ~ "LCA",
      str_detect(TI_norm, "principal component analysis|\\bpca\\b") ~ "PCA",
      str_detect(TI_norm, "multiple correspondence analysis|\\bmca\\b") ~ "MCA",
      str_detect(TI_norm, "indicateurs de durabilite des exploitations agricoles|\\bidea\\b") ~ "IDEA",
      str_detect(TI_norm, "social life cycle assessment|\\bscla\\b") ~ "SCLA",
      str_detect(TI_norm, "response inducing sustainability evaluation|\\brise\\b") ~ "RISE",
      str_detect(TI_norm, "sustainability assessment of food and agriculture systems|\\bsafa\\b") ~ "SAFA",
      str_detect(TI_norm, "smart farm") ~ "SMART-Farm",
      str_detect(TI_norm, "framework for the evaluation of natural resource management systems|\\bmesmis\\b") ~ "MESMIS",
      str_detect(TI_norm, "resilience adaptation pathways and transformation assessment|\\brapta\\b") ~ "RAPTA",
      str_detect(TI_norm, "tool for agroecological performance evaluation|\\btape\\b") ~ "TAPE",
      str_detect(TI_norm, "framework for evaluating sustainable land management|\\bfeslm\\b") ~ "FESLM",
      str_detect(TI_norm, "driver pressure state impact response|\\bdpsir\\b") ~ "DPSIR",
      str_detect(TI_norm, "general sustainability index|\\bgsi\\b") ~ "GSI",
      TRUE ~ NA_character_
    )
  ) %>%
  select(TI, PY, Metodologia)

head(df_sustentabilidad)

df_sustentabilidad <- df_sustentabilidad %>%
  filter(!is.na(Metodologia))

head(df_sustentabilidad)

library(ggplot2)
library(dplyr)

# Agrupar por año y metodología, y contar frecuencia
frecuencias_por_anio <- df_sustentabilidad %>%
  group_by(PY, Metodologia) %>%
  summarise(Frecuencia = n(), .groups = "drop")

# Graficar
library(ggplot2)
library(dplyr)

frecuencias_por_anio <- frecuencias_por_anio %>%
  filter(PY != 2026)

B<- ggplot(frecuencias_por_anio, aes(x = PY, y = log(Frecuencia), color = Metodologia)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "lm", size = 1) +
  scale_x_continuous(
    breaks = seq(min(frecuencias_por_anio$PY), max(frecuencias_por_anio$PY), by = 1)
  ) +
  labs(
    title = "B",
    x = "Year",
    y = "Log of number of publications",
    color = "Methodology"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black",
                                fill = NA, 
                                
                            linewidth = 0.8)
  ) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
B
# Figura final ----------
library(ggpubr)
#install.packages("ggpubr")

B <- ggplot(frecuencias_por_anio, aes(x = PY, y = log(Frecuencia), color = Metodologia)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "lm", size = 1) +
  scale_x_continuous(
    breaks = seq(min(frecuencias_por_anio$PY), max(frecuencias_por_anio$PY), by = 1)
  ) +
  labs(
    title = "B",
    x = "",
    y = "Log of number of publications",
    color = "Methodology"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 8),  # 👈 aquí ajustas el tamaño
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

B

ggarrange(
  A, B,
#  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  widths = c(1, 1),        # Asegura el mismo tamaño horizontal
  common.legend = FALSE
)



# SIN LOG -------------
# Graficar (etiquetas en inglés y escala logarítmica)
A<- ggplot(df_frecuencias, aes(x = (Frecuencia + 1),
              y = Metodología)) +  # log + 1 para evitar log(0)
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(
    title = "A",
    x = "Log of frequency",
    y = "Sustainability assessment methodology"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 0.8)
  )
A

B <- ggplot(frecuencias_por_anio,
            aes(x = PY, y = (Frecuencia), color = Metodologia)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "lm", size = 1) +
  scale_x_continuous(
    breaks = seq(min(frecuencias_por_anio$PY), max(frecuencias_por_anio$PY), by = 1)
  ) +
  labs(
    title = "B",
    x = "",
    y = "Log of number of publications",
    color = "Methodology"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 8),  # Aquí ajustas el tamaño
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

B

ggarrange(
  A, B,
  #  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  widths = c(1, 1),        # Asegura el mismo tamaño horizontal
  common.legend = FALSE
)


###################
# Establecer el directorio de trabajo
setwd("D:/2024/Google Drive/UDENAR/Seminario de Investigación 2/Text mining/WOS")
getwd()
# Frecuencia de palabras ------
library(readr)
df <- read_csv("wos sustainability.txt")

# Unir y limpiar el texto si es necesario
texto_completo <- paste(df, collapse = " ")
texto_limpio <- gsub('^c\\(|\\)$', '', texto_completo)     # Eliminar 'c(' y ')'
texto_limpio <- gsub('\\"', '', texto_limpio)              # Eliminar comillas escapadas
texto_limpio <- gsub(',\\s*', ' ', texto_limpio)           # Reemplazar comas por espacios
head(texto_limpio)

# Septexto_limpio# Separar por frases (cada una termina con punto)
frases <- unlist(strsplit(texto_limpio, "(?<=[\\.])\\s+", 
                          perl = TRUE))
head(frases)
# Eliminar espacios innecesarios al principio y al final
frases <- trimws(frases)

# Crear el data frame con número de frase y texto
df_frases <- data.frame(
  n = seq_along(frases),
  frase = frases,
  stringsAsFactors = FALSE
)

# Mostrar las primeras filas
head(df_frases, 6)

# Instalando paquetes
# install.packages("tm")  # Solo la primera vez
# install.packages("NLP")
library(tm)
library(NLP)

# Usar inglés como idioma (puedes cambiarlo si necesitas otro)
stop_words <- stopwords("en")
?stopwords

# Eliminar stopwords de cada frase
df_frases$frase_sin_stopwords <- sapply(df_frases$frase, function(frase) {
  palabras <- unlist(strsplit(frase, "\\s+"))                # Separar en palabras
  palabras_filtradas <- palabras[!tolower(palabras) %in% stop_words]  # Filtrar stopwords
  frase_limpia <- paste(palabras_filtradas, collapse = " ")  # Reconstruir la frase
  return(frase_limpia)
})

head(df_frases[, c("n", "frase_sin_stopwords")], 10)

# 1. Unir todas las frases sin stopwords en un solo texto
todo_el_texto <- paste(df_frases$frase_sin_stopwords, collapse = " ")

# 2. Eliminar puntuación, paréntesis, saltos de línea y caracteres especiales
texto_limpio <- gsub("[[:punct:]]", "", todo_el_texto)     # Elimina . , ; : ( ) etc.
texto_limpio <- gsub("[\r\n\t]", " ", texto_limpio)        # Elimina saltos de línea/tabulaciones
texto_limpio <- tolower(texto_limpio)                      # Convierte a minúsculas

# 3. Separar por palabras
palabras <- unlist(strsplit(texto_limpio, "\\s+"))

# 4. Contar frecuencia de cada palabra
tabla_frecuencia <- table(palabras)
tail(tabla_frecuencia)

# 5. Convertir en data frame
df_frecuencia <- as.data.frame(tabla_frecuencia, stringsAsFactors = FALSE)
colnames(df_frecuencia) <- c("palabra", "frecuencia")

# 6. Ordenar por frecuencia
df_frecuencia <- df_frecuencia[order(-df_frecuencia$frecuencia), ]

# Filtrar palabras que empiezan con "infestan"
social <- df_frecuencia[grepl("^socia", df_frecuencia$palabra), ]

# Ver el resultado
print(social)

# Filtrar palabras que empiezan con "infestan"
environ <- df_frecuencia[grepl("^environ", df_frecuencia$palabra), ]

# Ver el resultado
print(environ)

# Filtrar palabras que empiezan con "infestan"
economi <- df_frecuencia[grepl("^economi", df_frecuencia$palabra), ]

# Ver el resultado
print(economi)

head(df_frecuencia)


library(ggplot2)

# 1. Filtrar las 50 palabras más frecuentes
top100 <- head(df_frecuencia, 50)

# 2. Ordenar las palabras como factor según frecuencia para que el gráfico respete el orden
top100$palabra <- factor(top100$palabra, levels = rev(top100$palabra))

# Lista de palabras que quieres eliminar manualmente
palabras_inútiles <- c(
  "j", "n", "af", "data", "ti", "di", "sn", "jnau", "pt", "au", "so", "ab", "t1", "de", "by", "py", 
  "ut", "kg", "eq", "vl", "c", "b", "e", "f", "g", "h", "i", "l", "m", "o", "p", "q", "r", 
  "s", "also","can", "u", "v", "w", "x", "y", "z", "oi", "ri", "ar", "ei", "pd", "er", "ii"
)

# Filtrar df_frecuencia para excluir esas palabras
df_frecuencia_limpio <- df_frecuencia[!(df_frecuencia$palabra %in% palabras_inútiles), ]

# Filtrar top 50 después de eliminar palabras irrelevantes
top100_limpio <- head(df_frecuencia_limpio, 50)

# Ordenar las palabras como factor para gráfico horizontal
top100_limpio$palabra <- factor(top100_limpio$palabra, levels = rev(top100_limpio$palabra))

# Crear gráfico
ggplot(top100_limpio, aes(x = palabra, y = frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "Frequency") +
  theme_bw(base_size = 8)

library(ggplot2)

###### 6. Ordenar por frecuencia
df_frecuencia <- df_frecuencia[order(-df_frecuencia$frecuencia), ]

# Filtrar palabras que empiezan con "infestan"
social <- df_frecuencia[grepl("^socia", 
                              df_frecuencia$palabra), ]

# Ver el resultado
print(social)
# Filtrar palabras que empiezan con "infestan"
environ <- df_frecuencia[grepl("^environ", 
                               df_frecuencia$palabra), ]

# Ver el resultado
print(environ)

# Filtrar palabras que empiezan con "infestan"
economi <- df_frecuencia[grepl("^economi", 
                               df_frecuencia$palabra), ]

# Ver el resultado
print(economi)

# Crear copia del data frame original
df_frec_agregado <- df_frecuencia

# Función para agrupar palabras por raíz y reemplazarlas por una sola
agrupar_palabras <- function(df, patron, nuevo_nombre) {
  grupo <- df[grepl(patron, df$palabra), ]
  total <- sum(grupo$frecuencia)
  df <- df[!grepl(patron, df$palabra), ]
  df <- rbind(df, data.frame(palabra = nuevo_nombre, frecuencia = total))
  return(df)
}

# Agrupar palabras relacionadas con "economic"
df_frec_agregado <- agrupar_palabras(df_frec_agregado, 
                                     patron = "^economic|^economi|economicsnab|economia", 
                                     nuevo_nombre = "economic")

# Agrupar palabras relacionadas con "environmental"
df_frec_agregado <- agrupar_palabras(df_frec_agregado, 
                                     patron = "^environment", 
                                     nuevo_nombre = "environmental")

# Agrupar palabras relacionadas con "social"
df_frec_agregado <- agrupar_palabras(df_frec_agregado, 
                                     patron = "^social", 
                                     nuevo_nombre = "social")

# Agrupar palabras relacionadas con "social"
df_frec_agregado <- agrupar_palabras(df_frec_agregado, 
                                     patron = "^sustainab", 
                                     nuevo_nombre = "sustainability")

# Ordenar por frecuencia descendente
df_frec_agregado <- df_frec_agregado[order(-df_frec_agregado$frecuencia), ]

# Verificar los resultados agrupados
df_frec_agregado[df_frec_agregado$palabra %in% c("economic", "environmental", "social"), ]

# Lista de palabras que quieres eliminar manualmente
palabras_inútiles <- c(
  "j", "n", "af", "ti", "di", "sn", "jnau", "pt", "au", "so", "ab", "t1", "de", "by", "py", 
  "ut", "kg", "eq", "vl", "c", "b", "e", "f", "g", "h", "i", "l", "m", "o", "p", "q", "r", 
  "s", "also","can", "u", "v", "w", "x", "y", "z", "oi", "ri", "ar", "ei", "pd", "er"
)

# Filtrar df_frecuencia para excluir esas palabras
df_frec_agregado <- df_frec_agregado[!(df_frec_agregado$palabra %in% palabras_inútiles), ]

# 1. Seleccionar las 50 palabras más frecuentes de la base limpia
top100_agrupado <- head(df_frec_agregado, 50)

# 2. Ordenar para que el gráfico respete el orden descendente
top100_agrupado$palabra <- factor(top100_agrupado$palabra, levels = rev(top100_agrupado$palabra))

# 3. Crear gráfico de barras horizontal
grafico_barras <- ggplot(top100_agrupado, aes(x = palabra, y = frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "",  # Puedes añadir un título si deseas
       x = "",
       y = "Frequency") +
  theme_bw(base_size = 12);grafico_barras

# Guardar usando ggsave
ggsave("grafico_barras_publicacion.tiff", 
       plot = grafico_barras,
       width = 7, height = 10, 
       units = "in", dpi = 300,
       compression = "lzw")

#
#
#
#
#
#
# WordCloud ---------------------------------
#
#
#
#
#
#

# install.packages("wordcloud")  # Si no lo tienes instalado
# install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)

# Usar las palabras y sus frecuencias
p2 <- wordcloud(words = df_frec_agregado$palabra, 
                freq = df_frec_agregado$frecuencia,
                min.freq = 2,              # Puedes ajustar este umbral
                max.words = 50,           # Número máximo de palabras a mostrar
                random.order = FALSE,      # Ordenado por frecuencia
                rot.per = 0.25,            # Proporción de palabras rotadas
                colors = brewer.pal(8, "Dark2"));p2  # Paleta de colores


# Configurar el dispositivo gráfico TIFF
tiff("wordcloud_publicacion.tiff", 
     width = 7, height = 7, units = "in", 
     res = 300, compression = "lzw")

# Forzar uso del sistema gráfico base para evitar errores con Wordcloud
par(mar = c(0, 0, 0, 0))  # Sin márgenes

wordcloud(words = df_frec_agregado$palabra, 
          freq = df_frec_agregado$frecuencia,
          min.freq = 2,              # Puedes ajustar este umbral
          max.words = 50,           # Número máximo de palabras a mostrar
          random.order = FALSE,      # Ordenado por frecuencia
          rot.per = 0.25,            # Proporción de palabras rotadas
          colors = brewer.pal(8, "Dark2"))

# Cerrar el dispositivo gráfico
dev.off()



#
#
#
#
#
# Bigramas --------------
#
#
#
#
#

# Establecer el directorio de trabajo
setwd("D:/2024/Google Drive/UDENAR/Seminario de Investigación 2/Text mining/WOS/")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("tidyr")

library(tidytext)
library(dplyr)
library(tidyr)
data("stop_words")  # Esto carga el data.frame 'stop_words' con columnas: word, lexicon
library(readr)

df <- read_csv("wos sustainability.txt")

# Paso 1: Unir y limpiar el texto si es necesario
texto_completo <- paste(df, collapse = " ")
texto_limpio <- gsub('^c\\(|\\)$', '', texto_completo)     # Eliminar 'c(' y ')'
texto_limpio <- gsub('\\"', '', texto_limpio)              # Eliminar comillas escapadas
texto_limpio <- gsub(',\\s*', ' ', texto_limpio)           # Reemplazar comas por espacios

# Paso 2: Separar por frases (cada una termina con punto)
frases <- unlist(strsplit(texto_limpio, "(?<=[\\.])\\s+", perl = TRUE))

# Paso 3: Eliminar espacios innecesarios al principio y al final
library(tidyverse)
library(stringr)

# Crear el data frame con ID secuencial
df_frases <- data.frame(
  ID = seq_along(frases),      # o length(frases)
  frase = frases,
  stringsAsFactors = FALSE
)

# Tokenizar frases en palabras
palabras_limpias <- df_frases %>%
  unnest_tokens(word, frase)
?unnest_tokens

# Eliminar stopwords correctamente
palabras_limpias <- palabras_limpias %>%
  anti_join(stop_words, by = "word")

# Reconstruir las frases sin stopwords
df_frases_sin_stopwords <- palabras_limpias %>%
  group_by(ID) %>%
  summarise(frase = paste(word, collapse = " "), .groups = "drop")

# Verificar
head(df_frases_sin_stopwords)

library(tidytext)
library(dplyr)

# Crear bigramas (2 tokens consecutivos)
bigrams <- df_frases_sin_stopwords %>%
  unnest_tokens(bigrama, frase, token = "ngrams", n = 2)

# Ver los primeros bigramas
head(bigrams, n =20)

bigrama_freq <- bigrams %>%
  count(bigrama, sort = TRUE)

# Mostrar los más frecuentes
head(bigrama_freq, 10)

# Lista de palabras que quieres eliminar manualmente
palabras_inútiles <- c(
  "j", "n", "af", "ti", "di", "sn", "jnau", "pt", "au", "so", "ab", "t1", "de", "by", "py", 
  "ut", "kg", "eq", "vl", "c", "b", "e", "f", "g", "h", "i", "l", "m", "o", "p", "q", "r", 
  "s", "also","can", "u", "v", "w", "x", "y", "z", "oi", "ri", "ar", "ei", "pd", "er",
  "0000", "0002", "NA", "0003", "0001", "ghg", "ha", "1", "2071", "1050",
  "0959", "6526", "1879", "1786", "10.1016", "j.jclepro", "10.1016", "j.agsy",
  "0308", "521x", "2", "bp", "1873", "2267", "3", "978"
)

# Separar bigramas en dos palabras
bigrams_limpios <- bigrams %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ")%>%
  na.omit()

# Filtrar los que no contienen palabras indeseadas
bigrams_filtrados <- bigrams_limpios %>%
  filter(!palabra1 %in% palabras_inútiles,
         !palabra2 %in% palabras_inútiles)

# Volver a unir las palabras como bigrama
bigrams_filtrados <- bigrams_filtrados %>%
  mutate(bigrama = paste(palabra1, palabra2)) %>%
  select(ID, bigrama)

bigrama_freq_limpio <- bigrams_filtrados %>%
  count(bigrama, sort = TRUE)

# Ver los más frecuentes
head(bigrama_freq_limpio, 10)

# Seleccionar los top 50 bigramas
top100_bigramas <- head(bigrama_freq_limpio, 50)

# Reordenar factor para gráfico horizontal
top100_bigramas$bigrama <- factor(top100_bigramas$bigrama,
                                  levels = rev(top100_bigramas$bigrama))

library(ggplot2)

plot_bigramas <- ggplot(top100_bigramas, aes(x = bigrama, y = n)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +  # Color azul elegante
  coord_flip() +
  labs(title = "",
       x = "",
       y = "Frequency") +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8)
  );plot_bigramas

plot_bigramas

# Guardar usando ggsave
ggsave("plot_bigramas.tiff",
       plot = plot_bigramas,
       width = 7, height = 10, units = "in",
       dpi = 300, compression = "lzw")

# Plot Grafo Bigrama -------------
library(igraph)
library(ggraph)
library(tidygraph)

# Separar bigramas en dos palabras y conservar la frecuencia
bigrama_red <- top100_bigramas %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  filter(n > 1)  # opcional: quitar relaciones poco frecuentes

# Crear objeto de red
grafo_bigrama <- as_tbl_graph(bigrama_red, directed = FALSE)

set.seed(123)  # Para que el layout sea reproducible

plot_grafo <- ggraph(grafo_bigrama, layout = "fr") +  # "fr" = Fruchterman-Reingold layout
  geom_edge_link(aes(width = n), edge_alpha = 0.4, edge_colour = "#2b8cbe") +
  geom_node_point(color = "#08519c", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 size = 4, color = "black") +
  theme_void() +
  labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"));plot_grafo

# Configurar el dispositivo gráfico TIFF
tiff("plot_grafo.tiff", 
     width = 6, height = 6, units = "in", 
     res = 300, compression = "lzw")

# Forzar uso del sistema gráfico base para evitar errores con Wordcloud
par(mar = c(0, 0, 0, 0))  # Sin márgenes

ggraph(grafo_bigrama, layout = "fr") +  # "fr" = Fruchterman-Reingold layout
  geom_edge_link(aes(width = n), 
                 edge_alpha = 0.4, 
                 edge_colour = "#2b8cbe") +
  geom_node_point(color = "#08519c", size = 4) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, size = 4, 
                 color = "black") +
  theme_void() +
  labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"))
# Cerrar el dispositivo gráfico
dev.off()

# Multipanel plot ------------
mA<-ggplot(top100_agrupado, aes(x = palabra, y = frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "A",  # Puedes añadir un título si deseas
       x = "",
       y = "Frequency") +
  theme_bw(base_size = 12)
mA

mB <- ggraph(grafo_bigrama, layout = "fr") +  # "fr" = Fruchterman-Reingold layout
  geom_edge_link(aes(width = n), 
                 edge_alpha = 0.4, 
                 edge_colour = "#2b8cbe") +
  geom_node_point(color = "#08519c", size = 4) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, size = 4, 
                 color = "black") +
  theme_void() +
  annotate("text", x = -Inf, y = Inf, 
           label = "B", hjust = -0.5,
           vjust = 1.5, size = 5)  # Letra B en la esquina superior izquierda

library(patchwork)
# Combinar gráficos en un panel usando patchwork
multipanel_plot <- mA + mB + plot_layout(ncol = 2)

# Mostrar el multipanel
print(multipanel_plot)

# Guardar la imagen multipanel en formato TIFF
ggsave("multipanel_plot.tiff", plot = multipanel_plot,
       width = 14, height = 7, units = "in", dpi = 300, 
       compression = "lzw")

# Multipanel 2 

# Usar las palabras y sus frecuencias
p2 <- wordcloud(words = df_frec_agregado$palabra, 
                freq = df_frec_agregado$frecuencia,
                min.freq = 2,              # Puedes ajustar este umbral
                max.words = 50,           # Número máximo de palabras a mostrar
                random.order = FALSE,  
                title = "A",# Ordenado por frecuencia
                rot.per = 0.25,            # Proporción de palabras rotadas
                colors = brewer.pal(8, "Dark2"))  # Paleta de colores
p2

# Tabla de frecuencia --------------
# Verificar
head(df_frases_sin_stopwords)

# Crear un nuevo data frame agrupando las frases por categorías
df_agrupado <- df_frases_sin_stopwords %>%
  mutate(
    category = case_when(
      str_detect(frase, "\\b(socia\\w*)\\b") ~ "Social",
      str_detect(frase, "\\b(econom\\w*)\\b") ~ "Economical",
      str_detect(frase, "\\b(environ\\w*)\\b") ~ "Environmental",
      TRUE ~ NA_character_  # Para asegurar que solo queden las que cumplen alguna condición
    )
  ) %>%
  filter(!is.na(category))  # Eliminar las filas que no tengan ninguna categoría asignada

# Mostrar las primeras filas
head(df_agrupado)

# Tokenizar frases en palabras individuales
df_palabras <- df_agrupado %>%
  unnest_tokens(word, frase) %>%
  count(category, word, sort = TRUE) %>%    # Contar frecuencia por categoría
  group_by(category) %>%                    # Agrupar por categoría
  top_n(40, n) %>%                          # Seleccionar las 20 palabras más frecuentes por categoría
  ungroup()

# Mostrar la tabla
head(df_palabras)

# Crear un heatmap
grafico_heatmap <- df_palabras %>%
  ggplot(aes(x = category, y = reorder(word, n), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap de palabras más frecuentes por categoría",
       x = "Categoría",
       y = "Palabra",
       fill = "Frecuencia") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar heatmap
print(grafico_heatmap)

# Eliminando palabras indeseables
palabras_indeseables <- c(
  "0000", "0002", "ab", "tl", "wos", "ut", "py", "er", "vl",
  "agricultural","economy", "ri", "sn", "0003", "ar",
  "pd", "oi", "ei", "0001", 
  "di", "pt", "au", "ti", "af", "impacts", "socio", "environment"
)

# Filtrar las palabras indeseables de tu data frame de frecuencias
df_palabras_limpio <- df_palabras %>%
  filter(!word %in% palabras_indeseables)

# Mostrar las primeras filas para confirmar
head(df_palabras_limpio)

# Crear heatmap con palabras filtradas
mC <- df_palabras_limpio %>%
  ggplot(aes(x = category, y = reorder(word, n), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "C",
       x = "",
       y = "",
       fill = "Frequency") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar el heatmap
print(mC)

# Combinar gráficos en un panel usando patchwork
multipanel_plot2 <- mA + mC + mB + plot_layout(ncol = 3)

# Mostrar el multipanel
print(multipanel_plot2)

# Guardar la imagen multipanel en formato TIFF
ggsave("multipanel_plot2.tiff", plot = multipanel_plot2,
       width = 17, height = 7, units = "in", dpi = 300, 
       compression = "lzw")
getwd()

