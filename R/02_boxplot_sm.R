library(tidyverse)
library(ggrepel)

load("data/sm_data_clean.rda")


a <- position_dodge(width = 0.8)


sm_median <- sm_data_clean %>% 
  group_by(treatment, plot, depth) %>% 
  summarise(mediana = median(raw, na.rm = T))

sm_data_clean %>% 
  ggplot() +
  aes(plot, raw) +
  geom_boxplot(aes(fill = factor(depth)), 
               linewidth = 0.25, 
               outlier.size = 0.5) +
  geom_label_repel(data = sm_median,
             mapping = aes(label = round(mediana, 3),
                           y = mediana),
             position = position_dodge(width = 0.8),
             size = 2.8,
             # nudge_x = 0.4,
             segment.size = unit(0.5, "mm")) +
  facet_grid(~ treatment) +
  labs(x = "Parcela",
       y = expression(paste("Contenido volumétrico de agua [cm"^3," cm"^-3,"]")), 
       fill = "Depth\n[cm]"
  )+
  theme_bw() +
  theme(legend.title = element_text(hjust = 0.5), 
        #axis.title.x = element_blank(), 
        panel.grid = element_blank())


### Modelo lineal para encontrar el porcentaje de diferencia entre parcelas

linealm <- lm(raw ~ treatment, sm_data_clean)
summary(linealm)

sm_median <- sm_data_clean %>% 
  group_by(treatment, plot, depth) %>% 
  summarise(mediana = median(raw, na.rm = T)) %>% 
  pivot_wider(id_cols = c(plot, depth),
              names_from = treatment,
              values_from = mediana) %>% 
  mutate(diferencia = Control - Droguht,
         relacion = Droguht/Control) 

sm_median <- sm_data_clean %>% 
  group_by(treatment, plot, depth) %>% 
  summarise(mediana = median(raw, na.rm = T)) %>% 
  pivot_wider(id_cols = c(plot, treatment),
              names_from = depth,
              values_from = mediana) %>% 
  mutate(diferencia = `60` - `15`,
         relacion = `15`/`60`) %>% 
  group_by(treatment) %>% 
  mutate(promediodif = mean(diferencia),
         promediorela = mean(relacion))

