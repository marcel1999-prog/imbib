# Cargar librerías necesarias
library(googlesheets4)
library(dplyr)
library(huito)

# Autenticarse en Google Sheets
gs4_auth()

# Leer los datos desde Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1X5wBN_FrxXZ-XCQjYROYdNjaJcSAwmeBk9zd66utBSI/edit?usp=sharing"
gs <- as_sheets_id(url)
fb <- range_read(gs, sheet = "fb")

# Definir tratamientos
veinte <- c(102, 105, 111, 112)
quince <- c(101, 104, 107, 109)
control<- c(103, 106, 108, 110)

# Elegir fuentes
font <- c("Courgette", "Tillana")
huito_fonts(font)

# Crear etiquetas
label <- fb %>%
  rename(scientific.name = FACTOR, name = PLOTS) %>%
  mutate(
    tratamiento = case_when(
      name %in% veinte ~ "20 min",
      name %in% quince ~ "15 min",
      name %in% control ~ "Control",
      TRUE ~ ""
    ),
    color_tratamiento = case_when(
      tratamiento == "VEINTE" ~ "darkblue",
      tratamiento == "QUINCE" ~ "black",
      tratamiento == "Control" ~ "green",
      TRUE ~ "black"
    ),
    color_numero = color_tratamiento,
    number = row_number(),
    barcode = paste(number, gsub("(\\w+\\s+\\w+).*", "\\1", scientific.name), sep = "_"),
    barcode = gsub(" ", "-", barcode)
  ) %>%
  label_layout(size = c(9, 6), border_color = "darkgreen") %>%
  
  # Incluir imágenes
  include_image(
    value = "https://drive.google.com/uc?export=view&id=1saNYKk0nO96bVjHhExjAdtt07Iiu0SFb",
    size = c(4.6, 4.3),
    position = c(0.68, 3.78)
  ) %>%
  include_image(
    value = "https://drive.google.com/uc?export=view&id=1EBOKCdj2MG_b7oCumjf_Hda6GAHIHjwi",
    size = c(5, 1),
    position = c(2.55, 0.55)
  ) %>%
  include_image(
    value = "https://drive.google.com/uc?export=view&id=1LJx4DQpiX4KUBEWGiWOK6SQOSu2GOgQ3",
    size = c(2, 2),
    position = c(8.2, 5.3)
  ) %>%
  
  # Incluir código de barras
  include_barcode(
    value = "barcode",
    size = c(1.95,1.95),
    position = c(8,0.95)
  ) %>%
  
  # Textos principales
  include_text(value = "Orden: Caryophyllales", position = c(2.4, 3.8), size = 12, color = "black", opts = list(hjust = 0)) %>%
  include_text(value = "Familia: Amaranthaceae", position = c(2.2, 2.99), size = 12, color = "black", opts = list(hjust = 0)) %>%
  include_text(value = "Spinacia oleracea", position = c(4.2, 5.5), size = 18, color = "black", font = font[1]) %>%
  include_text(value = "Espinaca", position = c(4.2, 4.8), size = 16, color = "black", font = font[2]) %>%
  
  # Texto "LIJADO" o "CORTE CON BISTURI" al lado del código QR
  include_text(
    value = "tratamiento",
    position = c(5.27, 0.3),
    size = 13,
    color = "color_tratamiento",
    font = font[2],
    opts = list(hjust = 0)
  ) %>%
  
  # Número de parcela (rotado vertical) al lado derecho con mismo color
  include_text(
    value = "name",
    position = c(8.5, 2.8),
    size = 16,
    color = "color_numero",
    angle = 90,
    font = font[2]
  )

# Vista previa
label %>% label_print(mode = "preview")

# Exportar a PDF con 12 etiquetas por página
label %>% label_print(mode = "complete", filename = "pdf_etiquetas", nlabel = 12)
