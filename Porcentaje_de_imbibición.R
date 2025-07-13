library(dplyr)
library(ggplot2)
library(knitr)

# Crear data frame con los datos
df_imbibicion <- data.frame(
  tratamiento = c("20min", "15min", "control"),
  seco = c(0.301925, 0.3324, 0.317975),
  humedo = c(0.508125, 0.53705, 0.504675)
)

# Calcular porcentaje de imbibición
df_imbibicion <- df_imbibicion %>%
  mutate(imbibicion = ((humedo - seco) / seco) * 100)

# Mostrar tabla con kable
df_imbibicion %>%
  select(tratamiento, imbibicion) %>%
  kable(caption = "Porcentaje de imbibición por tratamiento")

# Gráfico descriptivo con eje Y ajustado
ggplot(df_imbibicion, aes(x = tratamiento, y = imbibicion, fill = tratamiento)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(imbibicion, 1)), vjust = -0.5, size = 5) +
  labs(
    title = "Imbibición (%) por tratamiento",
    x = "Tratamiento",
    y = "Imbibición (%)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(legend.position = "none")
