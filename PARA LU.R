

## Tasas de actividad, empleo y desocupación de las poblacipon de 14 años y más por sexo

base_eph <- get_total_urbano(year = 2017:2023,type = "individual")

tasas_trabajo <- base_eph %>%
  filter(CH06 >= 14) %>% 
  group_by(ANO4, CH04) %>% 
  summarise(poblacion                  = sum(PONDERA),
            Ocupados                   = sum(PONDERA[ESTADO == 1]),
            Desocupados                = sum(PONDERA[ESTADO == 2]),
            PEA                        = Ocupados + Desocupados,
            tasa_actividad           = round((PEA/poblacion)*100, 1),
            tasa_empleo              = round((Ocupados/poblacion)*100,1),
            tasa_desocupacion        = round((Desocupados/PEA)*100, 1)
  ) %>% 
  select(ANO4,CH04, tasa_actividad, tasa_empleo, tasa_desocupacion) %>% 
  rename(sexo = CH04) %>% 
  mutate(sexo = ifelse(sexo == 1, "Varones", "Mujeres"))


## Brecha de género e ingreso medio de la ocupación principal, por sexo de la población asalariada de 14 años y 


sub_base <- base_eph %>% 
  filter(CH06   >= 14,
         ESTADO == 1,
         PP07H  == 2,
         P21    != -9) %>% #No tiene descuento jubilatorio 
  organize_cno() %>% 
  organize_labels() %>% 
  select(1:CH06, P21, PONDIIO)

sub_base$P21 <- as.numeric(sub_base$P21)


ingreso <- sub_base  %>% 
  group_by(ANO4, CH04) %>% 
  summarise(ingreso_medio = round(weighted.mean(P21, w = PONDIIO),0)) %>% 
  select(ANO4, CH04, ingreso_medio) %>% 
  pivot_wider(names_from = CH04, values_from = ingreso_medio) %>% 
  rename(Mujeres = Mujer, Varones  = Varon) %>% 
  relocate(Mujeres, .before = Varones) %>% 
  mutate(Brecha = round(Mujeres/Varones*100,2)