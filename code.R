## Tempo de download ---------
getOption('timeout')
options(timeout = 1e3)

## Pacotes ------------
library(tidyverse)
library(sdmpredictors)
library(raster)
library(sp)
library(dismo)
library(sdm)
library(dplyr)
library(tidyr)
library(mapview)
library(usdm)
library(rgbif)
library(scrubr)
library(maps)
library(ggcorrplot)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  
library(ggthemes) 
library(metR)
library(tidyverse)
# install.packages("fuzzySim", repos="http://R-Forge.R-project.org")
# install.packages("geodata")
library(fuzzySim)
library(geodata)
library(sf)
library(ggsn)
library(writexl)
library(rJava)
library(maptools)
library(rgdal)
library(rgeos)
library(ENMeval)
library(ENMGadgets)
library(sqldf)
library(fields)
library(ecospat)
library(rmaxent)
library(hexbin)

# Baixar os dados do GBIF ------------------

## Dados GBIF filtrando América do Sul -------------
sp <- occ_search(taxonKey = name_backbone(name = "Megalops atlanticus")$speciesKey,
                 limit = 3000)
sp <- sp$data # Salvando o data frame de ocorrência
table(sp$basisOfRecord) 

# Criando um data frame com os seguintes tipo de registros:
sp1 <- sp %>% 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN", "HUMAN_OBSERVATION"))

# sp1 = total de 2965 registros

# Filtrando e renomeando as variáveis de interesse
spg <- sp1 %>% dplyr::select(decimalLongitude,decimalLatitude)
colnames(spg) <- c("lon", "lat")
spg$species <- 1
spg <- spg %>% drop_na() # Removendo os NA's
spg <- spg %>% distinct(lon, lat, .keep_all = TRUE) # Removendo coordenadas duplicadas

# spg = total de 2613 registros 

# Adicionando observações manuais, que não existiam no GBIF
new_rows <- data.frame(
  lon = c(-47.85667, -46.26583, -47.79778, -46.3336, -47.0063, -45.3581 ), 
  lat = c(-25.115, -24.05111, -25.086111, -23.9561,  -24.3125, -23.7785),
  species = c(1, 1, 1, 1, 1, 1) 
)

spg <- rbind(spg, new_rows) # spg final 2619 registros

# Salvando em formato Excel:
write_xlsx(spg, "megalops_atlanticus.xlsx") 

# Transformando em coordenadas:
coordinates(spg) <- c('lon','lat')

## Mapa com Área de Distribuição da Espécie  ---------

sp3 <- sp1 %>% dplyr::select(decimalLongitude,decimalLatitude)
colnames(sp3) <- c("lon", "lat")
world <- map_data("world")

p1 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(data = sp3, 
             aes(x = lon, y = lat),
             color = "black", alpha = 0.8, pch = 17, size = 1.3, 
             inherit.aes = FALSE)+
  theme_bw()+
  scale_y_latitude(breaks = seq(-60, 60, 20), limits = c(-70, 70))+
  scale_x_longitude(breaks = seq(-110, 30, 20),
                    limits = c(-120, 30))+
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))+
  annotate("rect", xmin = -100, xmax = -28, 
           ymin = -62, ymax = 40, 
           alpha = 0.2, color = "black", fill =  "red")+
  annotate("text", x = -40, y = 30, label = "Área \n Modelada",
           family = "serif", fontface = "bold") + 
  annotate("rect", xmin = -80, xmax = -30, ymin = -60, ymax = 15,
           alpha = 0.3, color= "black", fill = "purple")+
  annotate("text", x = -42, y = -48, label = "Área \n Predita" ,
           family = "serif", fontface = "bold")+
  coord_equal()+
  ggsn::north(y.min = -60, y.max = -50, 
              x.min = 20, x.max = 28, scale = 1.5, symbol = 1)


p1
ggsave("mapa_ocorrencia.jpg", p1)

# Baixando os dados Bio-Oracle (Presente) ------------ 

temp.bottom = c("BO2_tempmean_bdmean",
                "BO2_temprange_bdmean" 
)
# temp.surface = c("BO2_tempmean_ss", 
#               "BO2_temprange_ss" 
# )
sal.bottom = c(
  "BO2_salinityrange_bdmean" ,
  "BO2_salinitymean_bdmean"
)
sal.surface = c(
  "BO2_salinityrange_ss") 

curvel = c("BO2_curvelmean_ss")

present = c(temp.bottom, sal.bottom, 
            sal.surface,curvel)
options(sdmpredictors_datadir = "C:/R-4.0.2/library/sdmpredictors/Meta/")
present.rasters = load_layers(present)
plot(present.rasters)
names(present.rasters)
names(present.rasters) <- c("Temp_média_profun_média",
                            "Variação_temp_profun_média", 
                            "Variação_salin_profun_média",
                            "Salin_média_profun_média",
                            "Variação_salin_superf",
                            "Veloc_média_correntes_superf"
)
plot(present.rasters)

# Correlation Plot ------------
layers_correlation(present) %>% round(digits = 2) 
df <- as.data.frame(layers_correlation(present))
rownames(df) <-  c("Temp_média_profun_média",
                   "Variação_temp_profun_média", 
                   "Variação_salin_profun_média",
                   "Salin_média_profun_média",
                   "Variação_salin_superf",
                   "Veloc_média_correntes_superf"
)
colnames(df) <- rownames(df)

p2 <- ggcorrplot(df, lab  = TRUE, lab_size = 2.5,
                 type = "lower",
                 outline.col = "white",
                 ggtheme = ggplot2::theme_light,
                 colors = c("#6D9EC1", "gray88", "#E46726"),
                 legend.title = "Correlação")

ggsave("correlation.jpg", p2)

# Baixar Dados RCP 2.6 -----------------
fut.26.temp.bottom = c("BO2_RCP26_2100_tempmean_bdmean", 
                       "BO2_RCP26_2100_temprange_bdmean" 
)
#fut.26.temp.surface = c("BO2_RCP26_2100_tempmean_ss", 
#                       "BO2_RCP26_2100_temprange_ss" 
#)
fut.26.sal.bottom = c(
  "BO2_RCP26_2100_salinityrange_bdmean" ,
  "BO2_RCP26_2100_salinitymean_bdmean"
)
fut.26.sal.surface = c( 
  "BO2_RCP26_2100_salinityrange_ss")

fut.26.curvel = c("BO2_RCP26_2100_curvelmean_ss" 
)

future_26= c(fut.26.temp.bottom, 
             fut.26.sal.bottom, fut.26.sal.surface,
             fut.26.curvel)

options(sdmpredictors_datadir = "C:/R-4.0.2/library/sdmpredictors/Meta/")
future.26.rasters = load_layers(future_26)
plot(future.26.rasters)

# Baixar Dados RCP 8.5 --------------------
fut.85.temp.bottom = c("BO2_RCP85_2100_tempmean_bdmean",
                       "BO2_RCP85_2100_temprange_bdmean" 
)
#fut.85.temp.surface = c("BO2_RCP85_2100_tempmean_ss", 
#                       "BO2_RCP85_2100_temprange_ss" 
#)
fut.85.sal.bottom = c(
  "BO2_RCP85_2100_salinityrange_bdmean",
  "BO2_RCP85_2100_salinitymean_bdmean"
  
)
fut.85.sal.surface = c( 
  "BO2_RCP85_2100_salinityrange_ss")

fut.85.curvel = c("BO2_RCP85_2100_curvelmean_ss"
)

future_85= c(fut.85.temp.bottom,
             fut.85.sal.bottom, fut.85.sal.surface,
             fut.85.curvel)

options(sdmpredictors_datadir = "C:/R-4.0.2/library/sdmpredictors/Meta/")

future.85.rasters = load_layers(future_85)

plot(future.85.rasters)

# Similaridade e Modelagem -----------------------
## Definindo a área ---------
plot(present.rasters[[1]]) 
points(spg)
extension <- extent(-100,-20,-70,45)
spg_crop <- crop(spg, extension)
present_extent <- crop(present.rasters, extension)
plot(present_extent[[1]])
points(spg_crop)

## MESS 2.6 VS Presente ------------------
occ_final <- raster::extract(present_extent, spg)
fut.26.extent <- crop(future.26.rasters, present_extent)
final.sim <- similarity(fut.26.extent, occ_final)
final.mess <- final.sim$similarity_min
plot(final.mess)
p_sim_1 <- rasterVis::levelplot(final.mess,
                     main = "Similaridade Ambiental (Presente X RCP 2.6)", 
                     margin = FALSE) 
p_sim_1


## MESS 8.5 VS Presente ------------------
occ_final <- raster::extract(present_extent, spg)
fut.85.extent <- crop(future.85.rasters, present_extent)
final.sim <- similarity(fut.85.extent, occ_final)
final.mess <- final.sim$similarity_min
p_sim_2 <- rasterVis::levelplot(final.mess,
                                main = "Similaridade Ambiental (Presente X RCP 8.5)", 
                                margin = FALSE) 
p_sim_2

## Dados e modelo ------------
d <- sdmData(species~., spg_crop, 
             predictors = present_extent,
             bg = list(method = "gRandom",
                       n = 6000)) # 6 mil pontos
d

m <- sdm(species~., d,
         methods=c('maxent'), 
         replication=c('boot'), 
         n=5, 
         test.percent = 30, 
         parallelSetting=list(ncore=4,method='parallel')) 
m
# AUC = 0,99, COR = 0.95, TSS = 0.96, Deviance = 0.31, Kappa = 0.96
gui(m)

m1 <- predict(m, present_extent, 
              parallelSetting=list(ncore=4,method='parallel'))

plot(m1)

mean_maxent <- mean(m1)
plot(mean_maxent)

# Mapa Atual ------------------------
maxent_atual_df <- as(mean_maxent, "SpatialPixelsDataFrame")
maxent_atual_df <- as.data.frame(maxent_atual_df )
colnames(maxent_atual_df) <- c("value", "x", "y")
mapa <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                     "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
                                     "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
                                     "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
                                     "Dominica", "Saba"), 
                fill = "grey70", colour = "black")
p3 <- ggplot() +  
  geom_tile(data=maxent_atual_df, aes(x=x, y=y, fill=value),
            alpha=0.8) +
  scale_fill_viridis("Adequabilidade", limits = c(0,1)) +
  coord_equal() +
  geom_path(color = 'black')+
  theme(legend.key.width=unit(2, "cm")) +
  theme_gray()+
  scale_y_latitude(breaks = seq(10, -50, -10), limits = c(-58,13))+
  scale_x_longitude(limits = c(-87, -30),
                    breaks = seq(-30, -80, -10))+
  mapa+
  coord_equal()+
  ggsn::north(y.min = 9, y.max = 12, 
              x.min = -34, x.max = -30, scale = 1.5, symbol = 1)+
  labs(title = "Modelo Atual")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'darkgray'),
        plot.title = element_text(color="black", 
                                  size=14, face="bold.italic",
                                  hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

p3
ggsave("mapa_atual.jpg", p3)

# Mapa RCP 2.6 -----------------------
fut.26.extent <- crop(future.26.rasters, present_extent)
names(fut.26.extent) <- names(present_extent)

m2 <-  predict(m, fut.26.extent, 
               parallelSetting=list(ncore=4,method='parallel'))
plot(m2)
mean_maxent_fut_26 <- mean(m2)
plot(mean_maxent_fut_26)

mean_maxent_fut_26_df <- as(mean_maxent_fut_26 , "SpatialPixelsDataFrame")
mean_maxent_fut_26_df <- as.data.frame(mean_maxent_fut_26_df)
colnames(mean_maxent_fut_26_df) <- c("value", "x", "y")

p4 <- ggplot() +  
  geom_tile(data=mean_maxent_fut_26_df, aes(x=x, y=y, fill=value),
            alpha=0.8) +
  scale_fill_viridis("Adequabilidade", limits = c(0,1)) +
  coord_equal() +
  geom_path(color = 'black')+
  theme(legend.key.width=unit(2, "cm")) +
  theme_gray()+
  scale_y_latitude(breaks = seq(10, -50, -10), limits = c(-58,13))+
  scale_x_longitude(limits = c(-87, -30),
                    breaks = seq(-30, -80, -10))+
  mapa+
  coord_equal()+
  ggsn::north(y.min = 9, y.max = 12, 
              x.min = -34, x.max = -30, scale = 1.5, symbol = 1)+
  labs(title = "RCP 2.6 (2090-2100)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'darkgray'),
        plot.title = element_text(color="black", 
                                  size=14, face="bold.italic",
                                  hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))
p4
ggsave("mapa_2_6.jpg", p4)

# Mapa RCP 8.5 -----------------------
fut.85.extent <- crop(future.85.rasters, present_extent)
names(fut.85.extent) <- names(present_extent)

m3 <-  predict(m, fut.85.extent, 
               parallelSetting=list(ncore=4,method='parallel'))

mean_maxent_fut_85 <- mean(m3)
plot(mean_maxent_fut_85)

mean_maxent_fut_85_df <- as(mean_maxent_fut_85 , "SpatialPixelsDataFrame")
mean_maxent_fut_85_df <- as.data.frame(mean_maxent_fut_85_df)
colnames(mean_maxent_fut_85_df) <- c("value", "x", "y")

p5 <- ggplot() +  
  geom_tile(data=mean_maxent_fut_85_df, aes(x=x, y=y, fill=value),
            alpha=0.8) +
  scale_fill_viridis("Adequabilidade", limits = c(0,1)) +
  coord_equal() +
  geom_path(color = 'black')+
  theme(legend.key.width=unit(2, "cm")) +
  theme_gray()+
  scale_y_latitude(breaks = seq(10, -50, -10), limits = c(-58,13))+
  scale_x_longitude(limits = c(-87, -30),
                    breaks = seq(-30, -80, -10))+
  mapa+
  coord_equal()+
  ggsn::north(y.min = 9, y.max = 12, 
              x.min = -34, x.max = -30, scale = 1.5, symbol = 1)+
  labs(title = "RCP 8.5 (2090-2100)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'darkgray'),
        plot.title = element_text(color="black", 
                                  size=14, face="bold.italic",
                                  hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

p5
ggsave("mapa_8_5.jpg", p5)

## Niche Overlap -------------
nicheOverlap(mean_maxent_fut_26, mean_maxent, stat  = "D")
# Niche Overlap 0,93 (RCP 2.6 X Presente)
nicheOverlap(mean_maxent_fut_85, mean_maxent, stat  = "D")
# Niche Overlap 0,85 (RCP 8.5 X Presente)

# Curva Resposta -------------
p6 <- rcurve(m, id = 1:5)+
  theme_bw() +
  labs(y = "Curva de Resposta", x = "Variáveis Ambientais",
       title = NULL)
p6 

# Importância das variávies ---------
p7 <- plot(getVarImp(m))+ labs(x = NULL,
                               y = "Importância das variáveis",
                               title = NULL)+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.05,1)) +
  theme_bw()+
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    axis.text.x = element_text(angle=90, hjust=1)
  )
p7

ggsave("variáveis.jpg", p7)




