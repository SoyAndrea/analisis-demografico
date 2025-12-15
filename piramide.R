library(tidyverse)

niveles_edad <- c(
  "00-04","05-09","10-14","15-19","20-24","25-29",
  "30-34","35-39","40-44","45-49","50-54",
  "55-59","60-64","65-69","70-74","75-79",
  "80-84","85-89","90+"
)

## 2001

base_2001 <- tribble(
  ~edad_quinquenal, ~Varon, ~Mujer,
  "0-4",1703190,1646088,
  "5-9",1760659,1710558,
  "10-14",1738744,1688456,
  "15-19",1613030,1575274,
  "20-24",1597939,1601400,
  "25-29",1329493,1365848,
  "30-34",1159698,1205205,
  "35-39",1086600,1143017,
  "40-44",1043147,1093389,
  "45-49",959135,1012776,
  "50-54",895127,955354,
  "55-59",718159,785887,
  "60-64",597259,687078,
  "65-69",499544,610244,
  "70-74",422426,574099,
  "75-79",289055,438840,
  "80-84",152255,280653,
  "85-89",68423,156040,
  "90-94",20758,56896,
  "95+",4431,13956
) %>%
  pivot_longer(c(Varon, Mujer), names_to = "sexo", values_to = "poblacion") %>%
  mutate(anio = 2001)

## 2022

base_2022 <- tribble(
  ~edad_quinquenal, ~Varon, ~Mujer,
  "0-4",1402857,1440893,
  "5-9",1772183,1824234,
  "10-14",1786201,1842705,
  "15-19",1764602,1794417,
  "20-24",1775436,1735025,
  "25-29",1820852,1729539,
  "30-34",1784888,1684646,
  "35-39",1689906,1598863,
  "40-44",1711533,1603622,
  "45-49",1486233,1376718,
  "50-54",1278561,1169613,
  "55-59",1155449,1038682,
  "60-64",1054462,923244,
  "65-69",941658,790896,
  "70-74",793035,622529,
  "75-79",602502,419408,
  "80-84",404175,241330,
  "85-89",237241,116378,
  "90-94",111080,45076,
  "95-99",30852,11729,
  "100-104",3615,1110,
  "105+",585,224
) %>%
  pivot_longer(c(Varon, Mujer), names_to = "sexo", values_to = "poblacion") %>%
  mutate(anio = 2022)

## base 

base_censos <- bind_rows(base_2001, base_2022) %>%
  mutate(
    edad_quinquenal = case_when(
      edad_quinquenal %in% c("90-94","95+","95-99","100-104","105+") ~ "90+",
      edad_quinquenal == "0-4"  ~ "00-04",
      edad_quinquenal == "5-9"  ~ "05-09",
      TRUE ~ edad_quinquenal
    )
  ) %>%
  group_by(anio, sexo, edad_quinquenal) %>%
  summarise(poblacion = sum(poblacion), .groups = "drop") %>%
  mutate(
    edad_quinquenal = factor(edad_quinquenal, levels = niveles_edad),
    sexo = recode(sexo, Varon = "varones", Mujer = "mujeres")
  )

### piramide

piramide <- base_censos %>%
  group_by(anio) %>%
  mutate(porcentaje = poblacion / sum(poblacion) * 100) %>%
  ungroup() %>%
  mutate(porcentaje = if_else(sexo == "varones", -porcentaje, porcentaje))

## grafico 

g_piramide <- ggplot(piramide,
                     aes(x = edad_quinquenal,
                         y = porcentaje,
                         fill = sexo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~anio) +
  scale_y_continuous(breaks = seq(-10, 10, by = 2), 
                     labels = paste0(c(seq(-10, 0, by = 2)*-1, seq(2, 10, by = 2)), "%")) +
  scale_fill_manual(values = c("varones" = "#083d77",
                               "mujeres" = "#0cce6b")) +
  labs(
    x = "Grupo de edad",
    y = "Porcentaje",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

g_piramide


ggsave(filename = "censo/piramide.png", plot =g_piramide, width = 20, height = 10, units = "cm")
