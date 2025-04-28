library(tidyverse)
library(ggstatsplot)
library(zoo)

load("../Sensor de humedad/data/sm_data_clean.rda")


sm <- sm_data_clean %>% 
  mutate(raw = as.numeric(raw)) %>% 
  rename(fecha = `Timestamp`,
         profundidad = depth,
         contenido_vol = raw)
  
avg_sm <-  sm %>% 
  group_by(treatment, plot, profundidad) %>% 
  fill(contenido_vol) %>% 
  mutate(contenido_vol = scales::rescale(contenido_vol),
         max = rollmax(contenido_vol,k = 8, fill = NA),
         dif = contenido_vol - rollapply(contenido_vol,width = 8,FUN = min, fill = NA, align = "right"),
         pico = dif > 0.03 & contenido_vol == max) %>% 
  ungroup()

picos <- avg_sm %>% 
  filter(pico)

sm_eventos_match <- eventos_match %>% 
  rename(fecha = start.y) %>% 
  merge(picos, by = "fecha") %>% 
  select(-(2:9))



ggplot(avg_sm, aes(x = fecha,  y = max, color = factor(profundidad))) +
  geom_line() +
  geom_point(data = filter(avg_sm, pico)) +
  facet_grid(plot~treatment) +
  labs(y = expression(paste("VWC [cm"^3," cm"^-3,"]")), 
       color = "Depth\n[cm]") +
  theme_bw() +
  theme(legend.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank())

plotly::ggplotly()



# sm %>%
#   grouped_ggbetweenstats(treatment, contenido_vol,
#                          pairwise.display = "all",
#                          type = "parametric",
#                          grouping.var = profundidad,
#                          results.subtitle = FALSE,
#                          centrality.label.args = list(size = 3,
#                                                       nudge_x = 0.4,
#                                                       segment.linetype = 4,
#                                                       min.segment.length = 0),
#                          point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
#                                            alpha = 0.4, size = 2,
#                                            stroke = 0, na.rm = TRUE),
#                          boxplot.args = list(linewidth = 0.25, alpha = 0.2, na.rm = TRUE),
#                          violin.args = list(linewidth = 0.25, alpha = 0.2, na.rm = TRUE))




load("../Precipitación/output/tablas/eventos_match70.rda")



