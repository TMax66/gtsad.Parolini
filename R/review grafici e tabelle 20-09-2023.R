#MAP-PREV-STIM----
library(tidyverse)
library(readxl)
library(here)
library(binom)
#library(rmeta)
library(forestplot)
library(leaflet)
library(sp)
library(sf)
library(leaflet)
library(rmapshaper)
library(gt)

library(showtext)
sigla_pro <- read_excel(here("sigle province.xlsx"))
pos_est <- read_excel("dati.xlsx")
bdr <- read.csv2("bdr_lom_suidi.csv", sep = ",")

bdr <- bdr %>% 
  mutate_if(is.character,as.factor) %>% 
  select(PROVINCIA,ID_COMUNE,DS_COMUNE,CT_AZIENDA,CT_SOTTOCOD_AZIENDA,XT_SPECIE,XT_TIPOLOGIA_ALLEV,XT_MODALITA_ALLEV,capi,DT_APERTURA,DT_CHIUSURA,CT_LATITUDINE,CT_LONGITUDINE) %>% 
  filter(DT_CHIUSURA == ("0000-00-00")) %>% 
  filter(PROVINCIA %in% c("BS","CR")) %>% 
  filter(XT_TIPOLOGIA_ALLEV == "Ingrasso Familiare (Autoconsumo)")

tot_capi <- bdr %>% 
  group_by(DS_COMUNE) %>% 
  summarise(tot_capi = sum(capi))

tot_allev <- bdr %>% 
  group_by(DS_COMUNE) %>% 
  summarise(tot_allev = n())

dt <- bdr %>% 
left_join(tot_capi, by = c("DS_COMUNE")) %>%
  left_join(.,tot_allev, by = c("DS_COMUNE")) %>% 
  arrange(PROVINCIA, DS_COMUNE) %>% 
  distinct(DS_COMUNE, .keep_all = TRUE) %>% 
  select(PROVINCIA,DS_COMUNE,tot_allev,tot_capi)



pos_est <- pos_est %>%
  mutate(pr = case_when(
    repacc == "Sede Territoriale di Cremona" & pr == "ND" ~ "CR",
    repacc == "Sede Territoriale di Bergamo" & pr == "ND" ~ "BG",
    repacc == "Sede Territoriale di Pavia" & pr == "ND" ~ "PV",
    repacc == "Sede Territoriale di Brescia" & pr == "ND" ~ "BS",
    TRUE ~ paste0(pr)
    )) %>% 
  left_join(sigla_pro, by = c("pr" = "Sigla")) %>% 
  relocate(Provincia, .after = pr) %>%
  select(-Regione)

prevB <- pos_est %>%
  mutate_if(is.character,as.factor) %>% 
  filter(pr %in% c("BS", "CR")) %>% 
  filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%
  # filter(!specie %in% c(
  #   "MINILEPRE (SYLVILAGUS FLORIDANUS)",
  #   "CAMOSCIO",
  #   "CONIGLIO",
  #   "CONIGLIO SELVATICO",
  #   "STAMBECCO")) %>%
  # mutate(sex_en = case_when(
  #   sex == "M" ~ "Male",
  #   sex == "F" ~ "Female",
  #   is.na(sex) ~ "Unknown"),
  #   .after = sex) %>% 
  # mutate(age_class = case_when(
  #   specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
  #   specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
  #   specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
  #   specie == "LEPRE" & age == "Giovane" ~ "Juvenile",
  #   specie == "LEPRE" & age == "Adulto" ~ "Adult",
  #   specie == "CERVO" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
  #   specie == "CERVO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
  #   specie == "CERVO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
  #   specie == "CAPRIOLO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
  #   specie == "CAPRIOLO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
  #   is.na(age) ~ "Unknown",
  #   TRUE ~ paste0("?")),
  #   .after = age) %>%
  mutate(specie_en = case_when(
   specie == "CINGHIALE" ~ "WILD BOAR",
   specie == "LEPRE" ~ "HARE",
   specie == "DAINO" ~ "FALLOW DEER",
   specie == "CERVO" ~ "RED DEER",
   specie == "CAPRIOLO" ~ "ROE DEER",
   TRUE ~ paste0(specie)),
   .after = specie) %>%
  mutate(comune = str_to_upper(comune)) %>% 
  group_by(specie, comune, esito) %>% 
  count() %>% 
  pivot_wider(names_from = "esito", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(N, P)) %>% 
  select(-N) %>%
  arrange(specie, desc(tested))

#options(digits = 2)

resbinomB <- binom.bayes(
  x = prevB$P, n = prevB$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)

dtB <- cbind(prevB, resbinomB[,6:8])

dtB <- dtB %>% mutate(comune = case_when(
  comune == "LONATO" ~ paste0("LONATO DEL GARDA"),
  comune == "RODENGO-SAIANO" ~ paste0("RODENGO SAIANO"),
  comune == "ROÈ VOLCIANO" ~ paste0("ROE' VOLCIANO"),
  comune == "SALÒ" ~ paste0("SALO'"),
  comune == "TOSCOLANO-MADERNO" ~ paste0("TOSCOLANO MADERNO"),
  comune == "TREMOSINE" ~ paste0("TREMOSINE SUL GARDA"),
  comune == "TORRE DÈ PICENARDI" ~ paste0("TORRE DE' PICENARDI"),
  TRUE ~ paste0(comune)))

dtB <- dtB %>% 
  left_join(dt, by = c("comune" = "DS_COMUNE"))
#dtB = dati su brucella join con banca dati regionale suini - OK


sigla_pro <- read_xlsx(here("data", "processed", "sigle province.xlsx"))
reg <- readRDS(here("data", "processed", "ITA_adm1.sf.rds"))
pro <- readRDS(here("data", "processed", "ITA_adm2.sf.rds"))
com <- readRDS(here("data", "processed", "ITA_adm3.sf.rds"))
sf::st_crs(reg) = 4326
sf::st_crs(pro) = 4326
sf::st_crs(com) = 4326

com1 <- com[com$NAME_1 %in% c("Lombardia"),]
pro1 <- pro[pro$NAME_1 %in% c("Lombardia"),]
reg1 <- reg[reg$NAME_1 %in% c("Lombardia"),]

pro1 <- pro1 %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>% 
  mutate(NAME_2 = case_when(
    NAME_2 == "Monza and Brianza" ~ paste0("Monza-Brianza"),
    NAME_2 == "Mantua" ~ paste0("Mantova"),
    TRUE ~ paste0(NAME_2)
  ))

com1 <- com1 %>%
  mutate(NAME_3 = str_to_upper(NAME_3))

com1 <- com1[com1$NAME_2 %in% c("Brescia","Cremona"),]

com1 <- com1 %>% mutate(NAME_3 = case_when(
  NAME_3 == "LONATO" ~ paste0("LONATO DEL GARDA"),
  NAME_3 == "RODENGO-SAIANO" ~ paste0("RODENGO SAIANO"),
  NAME_3 == "ROÈ VOLCIANO" ~ paste0("ROE' VOLCIANO"),
  NAME_3 == "SALÒ" ~ paste0("SALO'"),
  NAME_3 == "TOSCOLANO-MADERNO" ~ paste0("TOSCOLANO MADERNO"),
  NAME_3 == "TREMOSINE" ~ paste0("TREMOSINE SUL GARDA"),
  TRUE ~ paste0(NAME_3)))

# pos %>% 
#   filter(pr %in% c("BS", "CR")) %>% 
#   filter(specie == "CINGHIALE") %>% 
#   filter(esito == "P") %>% 
#   mutate(comune = str_to_upper(comune)) %>% 
#   group_by(pr,comune) %>% 
#   summarise(pos = n()) %>%  
#   left_join(com1, by = c("comune"= "NAME_3")) %>% View()

com1 <- com1 %>%
  left_join(dtB, by = c("NAME_3"= "comune"))

com1 <- com1 %>%
  mutate(mean = ifelse(is.na(mean), -1, mean)) %>%
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))



head(sort(com1$mean, decreasing = T))
bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
RColorBrewer::brewer.pal(9, "OrRd")[1:9]
vec <- RColorBrewer::brewer.pal(9, "OrRd")[c(4,6,7,8,9)]
pal <- colorBin(palette = vec, domain = com1$mean,
                na.color = "white",
                bins = bins)#, na.color = "honeydew")

# addLegendCustom <- function(map, colors, labels, sizes, opacity = 1){
#   colorAdditions <- paste0(colors, "; margin-top: 4px; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
#   labelAdditions <- paste0("<div style='display: inline-block;height: ", 
#                            sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
#                            labels, "</div>")
#   
#   
# 
#   return(addLegend(map, colors = colorAdditions, 
#                    labels = labelAdditions, opacity = opacity))
# }


addLegendCustom <- function(map, colors, labels, sizes, opacity = 1){
  colorAdditions <- paste0(colors, "; margin-top: 4px; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px; font-size: 13px; margin-top: 4px; line-height: ", sizes, "px;'>", 
                           "<b>",labels,"</b>", "</div>")

  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}


##map----
#45.54118123378887, 10.211316983326657
backg <- htmltools::tags$style(
  ".info {
    padding: 16px 18px;
    font: 14.65px/17px Arial, sans-serif;
    background: white;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
}")  

m1 <- leaflet(com1,
        # height = 900,
        # width = 900,
        options = leafletOptions(zoomControl = FALSE,
                                 attributionControl = FALSE)) %>% 
  setView(10.21 ,45.65, zoom = 9) %>%
  #fitBounds(9, 44, 11, 46) %>% 
  #addTiles() %>% 
  addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("polygons", zIndex = 420) %>%        # Level 2: middle
  addMapPane("labels", zIndex = 430) %>%          # Level 3: top
  addMapPane("markers", zIndex = 440) %>%          # Level 3: top
  addProviderTiles(provider = "CartoDB.VoyagerNoLabels",
                   options = pathOptions(pane = "background_map")) %>%
  # addProviderTiles(provider = "Stamen.TerrainLabels", #CartoDB.VoyagerNoLabels - #Stamen.TerrainLabels - CartoDB.VoyagerOnlyLabels
  #                  options = pathOptions(pane = "labels")) %>%
  addPolygons(data = reg1,
              stroke = TRUE,
              fill = FALSE,
              weight = 1,
              opacity = 1,
              color = "black",
              smoothFactor = 0,
              # dashArray = "1",
    options = pathOptions(pane = "polygons")
  ) %>% 
  addPolygons(data = pro1[-c(2,4),],
              stroke = TRUE,
              fill = T,
              fillColor = "white",
              fillOpacity = 0.8,
              
              smoothFactor = 0,
              weight = 1,
              opacity = 1,
              color = "black",
              # dashArray = "1",
    options = pathOptions(pane = "polygons")
  ) %>% 
  # addPolygons(data = pro1[c(2,4),],
  #             stroke = TRUE,
  #             fill = T,
  #             fillColor = "white",
  #             fillOpacity = 0.8,
  #             weight = 2.5,
  #             opacity = 1,
  #             color = "black",
  #             # dashArray = "1",
  #   options = pathOptions(pane = "polygons")
  # ) %>%
  
    # addPolygons(data = com1,
    #           stroke = TRUE,
    #           fill = FALSE,
    #           weight = 0.5,
    #           opacity = 0.5,
    #           color = "black",
    #           dashArray = "1")
  addPolygons(data = com1,
              label = as.character(com1$NAME_3),
              stroke = TRUE,
              fillColor = ~pal(com1$mean),
              
              smoothFactor = 0,
              # popup = paste0(
              #    "<b>",
              #    com1$NAME_3,
              #    "</b>",
              #    "<br>",
              #    "<b> n. vaccinazioni: </b>",
              #    com1$mean
              # ),
              weight = 0.5,
              opacity = 1,
              color = "black",
              dashArray = "1",
              fillOpacity = 0.7,
    options = pathOptions(pane = "polygons")
              # stroke = TRUE,
              # weight = 0.2,
              # opacity = 1,
              # color = "grey",
              # fillColor = ~pal(bubble$mean),
              # fillOpacity = ~bubble$mean / max(bubble$mean),
              # #color = ~pal(bubble$mean)
              ) %>% 
  
  addPolylines(data = pro1[c(2,4),],
              stroke = TRUE,
              fill = F,
              
              smoothFactor = 0,
              # fillColor = "white",
              # fillOpacity = 0.8,
              weight = 2.5,
              # opacity = 1,
              color = "black",
              opacity = 1,
              # dashArray = "1",
              options = pathOptions(pane = "polygons")
  ) %>%
  
  addLegend(pal = pal,
            values = ~com1$mean,
    
            # labels = c("0 - 500",
            #            "500 - 1000",
            #            "1000 - 5000",
            #            "5000 - 10000",
            #            "10000 - 25000",
            #            "25000 - 50000",
            #            "50000 - 100000"),
            # colors = vec,
            
            opacity = 1,
            #bins = c(0, 100, 1000, 5000, 10000, 50000, 150000, 250000, 450000),
            title = "ESTIMATED PREVALENCE",
            position = "topright",
            labFormat = labelFormat(
              prefix = "",
              suffix = "",
              between = " - ",
              digits = 3,
              big.mark = "",
              transform = identity
              )) %>% 
  addLabelOnlyMarkers(data = pro1[-c(2,4),], ~lng, ~lat,
                      label = ~as.character(toupper(pro1[-c(2,4),]$NAME_2)), 
                      labelOptions = labelOptions(
                      noHide = T,
                      direction = 'center',
                      textOnly = T,
                      style = list(
                      "font-family" = "Arial",
                      "color" = "black",
                      "font-weight" = "bold",
                      "font-size" = "8px"
                      #"text-shadow" = "0 0 5px #fff"
                      #"-webkit-text-stroke" = "2px #fff"
                      ))) %>%
    addLabelOnlyMarkers(data = pro1[c(2,4),], ~lng, ~lat,
                      label = ~as.character(toupper(pro1[c(2,4),]$NAME_2)), 
                      labelOptions = labelOptions(
                      noHide = T,
                      direction = 'center',
                      textOnly = T,
                      style = list(
                      "font-family" = "Arial",
                      "color" = "rgba(0,0,0, 0.7)", #rgba(120,121,118, 0.9)
                      "font-weight" = "bold",
                      "font-size" = "20px",
                      "-webkit-text-stroke-width" = "1px",
            "-webkit-text-stroke-color" =  "black"
                      #"text-shadow" = "0 0 5px #fff"
                      #"-webkit-text-stroke" = "2px #fff"
                      ))) %>%
  addCircleMarkers(data = bdr, lng = ~CT_LONGITUDINE, lat = ~CT_LATITUDINE,
                   radius = 0,
                   opacity = 0.5,
                   weight = 2,
                   color = "blue",
    options = pathOptions(pane = "markers")) %>% 
  addLegendCustom(colors = c("blue"), 
                  labels = c("BACKYARD SWINE FARMS"), sizes = c(10)) %>% 
  htmlwidgets::prependContent(backg)


m1

# library(htmltools)
# library(htmlwidgets)
# #https://stackoverflow.com/questions/61058110/r-code-to-save-shiny-tag-list-to-html-as-the-viewer-export-save-as-web-page-bu
# 
# m1 <- m1 %>% prependContent(
#     tags$head(
#       tags$style(
#         ".info {
#     padding: 16px 18px;
#     font: 14.65px/17px Arial, sans-serif;
#     background: white;
#     background: rgba(255,255,255,0.8);
#     box-shadow: 0 0 15px rgba(0,0,0,0.2);
#     border-radius: 5px;
# }"
#       )
#     ),
#     m1
#   )
# m1

# className = "info legend"

library(htmlwidgets)
library(webshot2)
saveWidget(m1, "mappa.html", selfcontained = TRUE)
webshot2::webshot("mappa.html",
                  file = "mappa prevalenza stimata 20-09-2023.png",
                  # delay = 5,
                  zoom = 5,
                  # vwidth = 3000, vheight = 3000,
                  # vwidth = 900,vheight = 900,
                  cliprect = "viewport")






#1) CINGHIALE BAYES----
#TUTTI I CINGHIALI DI TUTTE LE PROVINCE
library(tidyverse)
library(readxl)
library(here)
library(binom)
#library(rmeta)
library(forestplot)

sigla_pro <- read_excel(here("sigle province.xlsx"))
pos <- read_excel("dati.xlsx")

pos <- pos %>%
  mutate(pr = case_when(
    repacc == "Sede Territoriale di Cremona" & pr == "ND" ~ "CR",
    repacc == "Sede Territoriale di Bergamo" & pr == "ND" ~ "BG",
    repacc == "Sede Territoriale di Pavia" & pr == "ND" ~ "PV",
    repacc == "Sede Territoriale di Brescia" & pr == "ND" ~ "BS",
    TRUE ~ paste0(pr)
  )) %>% 
  left_join(sigla_pro, by = c("pr" = "Sigla")) %>% 
  relocate(Provincia, .after = pr) %>% 
  filter(Regione == "Lombardia") %>% 
  select(-Regione)

prevB <- pos %>%
  mutate_if(is.character,as.factor) %>% 
  # filter(pr %in% c("BS", "CR")) %>% 
  # filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%
  filter(!specie %in% c(
    "MINILEPRE (SYLVILAGUS FLORIDANUS)",
    "CAMOSCIO",
    "CONIGLIO",
    "CONIGLIO SELVATICO",
    "STAMBECCO")) %>%
  mutate(sex_en = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    is.na(sex) ~ "Unknown"),
    .after = sex) %>% 
  mutate(age_class = case_when(
    specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "LEPRE" & age == "Giovane" ~ "Juvenile",
    specie == "LEPRE" & age == "Adulto" ~ "Adult",
    specie == "CERVO" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CERVO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CERVO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "CAPRIOLO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CAPRIOLO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    is.na(age) ~ "Unknown",
    TRUE ~ paste0("?")),
    .after = age) %>%
  mutate(specie_en = case_when(
    specie == "CINGHIALE" ~ "WILD BOAR",
    specie == "LEPRE" ~ "HARE",
    specie == "DAINO" ~ "FALLOW DEER",
    specie == "CERVO" ~ "RED DEER",
    specie == "CAPRIOLO" ~ "ROE DEER",
    TRUE ~ paste0(specie)),
    .after = specie) %>%
  mutate(comune = str_to_upper(comune)) %>% 
  #filter(specie == "CINGHIALE") %>% 
  group_by(specie, sex_en, age_class, esito) %>% 
  count() %>% 
  pivot_wider(names_from = "esito", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(N, P)) %>% 
  select(-N) %>%
  arrange(specie,
          match(sex_en,
                c("Male", "Female", "Unknown")),
          match(age_class,
                c("Adult","Yearling", "Juvenile", "Unknown")))

#options(digits = 2)

resbinomB <- binom.bayes(
  x = prevB$P, n = prevB$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)

dtB <- cbind(prevB, resbinomB[,6:8])

cinghiali <- dtB %>% filter(specie == "CINGHIALE")

sum(cinghiali$P) #382
sum(cinghiali$tested) #6449

xx <- binom.bayes(
  x = 378, n = 6440,
  type = "highest", conf.level = 0.95, tol = 1e-9)
xx

cinghiali <- cinghiali %>%
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round, 2)) %>%
  select(-specie)

##fplot1----
xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

fplot1 <- cinghiali %>%
  ungroup() %>%
  mutate(est = sprintf("%.2f", mean), .after = tested) %>% 
  mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(
    mar = unit(rep(2, times = 4), "mm"),
    shapes_gp = fpShapesGp(default = gpar(lwd = 0.5)),
    labeltext = c(sex_en, age_class, P,
                  tested,  est, CI),
    clip = c(0, 1),
    ci.vertices = TRUE,
    ci.vertices.height = 0.05,
    xlog = FALSE, 
    #title = "WILD BOAR",
    xlab = expression(paste("Estimated prevalence of ",italic("Brucella spp. "),"with 95% CI")),
    colgap = unit(7, "mm"),
    xticks = xticks,
    #xticks.digits = 2,
    #graphwidth = unit(10, "cm"),
    txt_gp = fpTxtGp(
      xlab  = gpar(cex = 1),
      ticks = gpar(cex = 0.9),
      label = list(
        #riga1
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        #riga 13
        list(
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1)))),
    #boxsize = 0.2,
    # shapes_gp = fpShapesGp(box = list(gpar(fill = "yellow"),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5))),
    align = "llrrrc"
  ) %>% 
  # txt_gp = fpTxtGp(
  #   summary = gpar(cex = 1))) %>%
  fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_14 = gpar(lty = 1),
               h_15 = gpar(lty = 1)) %>% 
  fp_add_header(sex_en = "Sex", 
                age_class = "Age", 
                P = "Positive", 
                tested = "Tested", 
                est = "Prevalence",
                CI = "95% CI"
  ) %>%
  fp_append_row(mean  = 0.05876417,
                lower = 0.05306366,
                upper = 0.06454162,
                sex_en = expression(bold("Overall")),
                P = expression(bold("378")),
                tested = expression(bold("6440")),
                est = expression(bold("0.06")),
                CI = expression(bold("[0.05, 0.06]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 13), "royalblue"))

png(
  "forestplot1 20-09-2023.png",
  width     = 5.25,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 5
)
fplot1
dev.off() 


#2) PROV - CINGHIALE BAYES----
library(tidyverse)
library(readxl)
library(here)
library(binom)
#library(rmeta)
library(forestplot)

sigla_pro <- read_excel(here("sigle province.xlsx"))
pos <- read_excel("dati.xlsx")

pos <- pos %>%
  mutate(pr = case_when(
    repacc == "Sede Territoriale di Cremona" & pr == "ND" ~ "CR",
    repacc == "Sede Territoriale di Bergamo" & pr == "ND" ~ "BG",
    repacc == "Sede Territoriale di Pavia" & pr == "ND" ~ "PV",
    repacc == "Sede Territoriale di Brescia" & pr == "ND" ~ "BS",
    TRUE ~ paste0(pr)
  )) %>% 
  left_join(sigla_pro, by = c("pr" = "Sigla")) %>% 
  relocate(Provincia, .after = pr) %>% 
  filter(Regione == "Lombardia") %>% 
  select(-Regione)

prevB <- pos %>%
  mutate_if(is.character,as.factor) %>% 
  # filter(pr %in% c("BS", "CR")) %>% 
  # filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%
  filter(!specie %in% c(
    "MINILEPRE (SYLVILAGUS FLORIDANUS)",
    "CAMOSCIO",
    "CONIGLIO",
    "CONIGLIO SELVATICO",
    "STAMBECCO")) %>%
  mutate(sex_en = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    is.na(sex) ~ "Unknown"),
    .after = sex) %>% 
  mutate(age_class = case_when(
    specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "LEPRE" & age == "Giovane" ~ "Juvenile",
    specie == "LEPRE" & age == "Adulto" ~ "Adult",
    specie == "CERVO" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CERVO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CERVO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "CAPRIOLO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CAPRIOLO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    is.na(age) ~ "Unknown",
    TRUE ~ paste0("?")),
    .after = age) %>%
  mutate(specie_en = case_when(
    specie == "CINGHIALE" ~ "WILD BOAR",
    specie == "LEPRE" ~ "HARE",
    specie == "DAINO" ~ "FALLOW DEER",
    specie == "CERVO" ~ "RED DEER",
    specie == "CAPRIOLO" ~ "ROE DEER",
    TRUE ~ paste0(specie)),
    .after = specie) %>%
  mutate(comune = str_to_upper(comune)) %>% 
  #filter(specie == "CINGHIALE") %>% 
  group_by(specie, Provincia, esito) %>% 
  count() %>% 
  pivot_wider(names_from = "esito", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(N, P)) %>% 
  select(-N) %>%
  arrange(specie, desc(tested), desc(P))

#options(digits = 2)

resbinomB <- binom.bayes(
  x = prevB$P, n = prevB$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)

dtB <- cbind(prevB, resbinomB[,6:8])

cinghiali <- dtB %>% filter(specie == "CINGHIALE")

sum(cinghiali$P) #382
sum(cinghiali$tested) #6449

xx <- binom.bayes(
  x = 378, n = 6440,
  type = "highest", conf.level = 0.95, tol = 1e-9)
xx

cinghiali <- cinghiali %>%
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round, 2)) %>%
  select(-specie)

##fplot2----
xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

fplot2 <- cinghiali %>%
  ungroup() %>%
  mutate(est = sprintf("%.2f", mean), .after = tested) %>% 
  mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(
    mar = unit(rep(2, times = 4), "mm"),
    shapes_gp = fpShapesGp(default = gpar(lwd = 0.5)),
    labeltext = c(Provincia, P,
                  tested, est, CI),
    clip = c(0, 1),
    ci.vertices = TRUE,
    ci.vertices.height = 0.05,
    xlog = FALSE, 
    #title = "WILD BOAR",
    xlab = expression(paste("Estimated prevalence of ",italic("Brucella spp. "),"with 95% CI")),
    colgap = unit(9, "mm"),
    xticks = xticks,
    #xticks.digits = 2,
    #graphwidth = unit(10, "cm"),
    txt_gp = fpTxtGp(
      xlab  = gpar(cex = 1),
      ticks = gpar(cex = 0.9),
      label = list(
        #riga1
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        list(
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9),
          gpar(cex = 0.9)),
        #riga ultima
        list(
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1),
          gpar(cex = 1)))),
    #boxsize = 0.2,
    # shapes_gp = fpShapesGp(box = list(gpar(fill = "yellow"),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5),
    #                                   gpar(cex = 0.5))),
    align = "lrrrc"
  ) %>% 
  # txt_gp = fpTxtGp(
  #   summary = gpar(cex = 1))) %>%
  fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_9 = gpar(lty = 1),
               h_10 = gpar(lty = 1)) %>%
  fp_add_header(Provincia = "Province", 
                P = "Positive", 
                tested = "Tested", 
                est = "Prevalence",
                CI = "95% CI"
  ) %>%
  fp_append_row(mean  = 0.05876417,
                lower = 0.05306366,
                upper = 0.06454162,
                Provincia = expression(bold("Overall")),
                P = expression(bold("378")),
                tested = expression(bold("6440")),
                est = expression(bold("0.06")),
                CI = expression(bold("[0.05, 0.06]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 8), "royalblue"))


png(
  "forestplot2 20-09-2023.png",
  width     = 5.25,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 5
)
fplot2
dev.off() 



#TABELLE----
##TUTTO----
sigla_pro <- read_excel(here("sigle province.xlsx"))
pos <- read_excel("dati.xlsx")

pos <- pos %>%
  mutate(pr = case_when(
    repacc == "Sede Territoriale di Cremona" & pr == "ND" ~ "CR",
    repacc == "Sede Territoriale di Bergamo" & pr == "ND" ~ "BG",
    repacc == "Sede Territoriale di Pavia" & pr == "ND" ~ "PV",
    repacc == "Sede Territoriale di Brescia" & pr == "ND" ~ "BS",
    TRUE ~ paste0(pr)
  )) %>% 
  left_join(sigla_pro, by = c("pr" = "Sigla")) %>% 
  relocate(Provincia, .after = pr) %>% 
  filter(Regione == "Lombardia")

tab1 <- pos %>%
  mutate_if(is.character,as.factor) %>% 
  # filter(pr %in% c("BS", "CR")) %>% 
  # filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%
  filter(!specie %in% c(
    "MINILEPRE (SYLVILAGUS FLORIDANUS)",
    "CAMOSCIO",
    "CONIGLIO",
    "CONIGLIO SELVATICO",
    "STAMBECCO")) %>%
  mutate(sex_en = case_when(
    sex == "M" ~ "Males",
    sex == "F" ~ "Females",
    is.na(sex) ~ "Unknown"),
    .after = sex) %>% 
  mutate(age_class = case_when(
    specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "LEPRE" & age == "Giovane" ~ "Juvenile",
    specie == "LEPRE" & age == "Adulto" ~ "Adult",
    specie == "CERVO" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CERVO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CERVO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "CAPRIOLO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CAPRIOLO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    is.na(age) ~ "Unknown",
    TRUE ~ paste0("?")),
    .after = age) %>%
  mutate(comune = str_to_upper(comune)) %>% 
  mutate(pos = case_when(
    esito == "P" ~ 1,
    TRUE ~ 0
  ), .after = esito) %>%
  mutate(specie_en = case_when(
    specie == "CINGHIALE" ~ "Wild boar",
    specie == "LEPRE" ~ "Hare",
    specie == "DAINO" ~ "Fallow deer",
    specie == "CERVO" ~ "Red deer",
    specie == "CAPRIOLO" ~ "Roe deer",
    TRUE ~ paste0(specie)),
    .after = specie) %>% 
  # mutate(specie_en = firstup(specie_en)) %>% 
  group_by(specie_en) %>% 
  summarise(pos = sum(pos),
            tot = n(),
            prev = sum(pos)/n()) %>% 
  arrange(desc(tot)) %>% 
  select(specie_en, tot) %>% 
  # rbind(., data.frame(
  #   specie="Total",
  #   pos=sum(.$pos, na.rm=T),
  #   tot=sum(.$tot, na.rm=T),
  #   prev=""
  #   )) %>% View
  gt() %>%
  cols_label(
    specie_en = "Species",
    tot = "Sample size"
  ) %>%  
  cols_align(
    align = c("left"),
    columns = specie_en
  ) %>% 
  # tab_header(
  #   title = md("Species distribution of tested samples against *Brucella spp.*")
  #   #subtitle = "Jan 2017 - Dic 2021"
  # ) %>% 
  tab_style(
    style = list(
      cell_text(align = "justify",
                color = "black"
                #size = "12px"
      ),
      cell_fill(color = "#FFFFFF")
    ),
    locations = cells_title()) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D3D3D3", weight = px(4)),
    locations = cells_body(rows = 5)
  ) %>%
  opt_table_font(font = c("Arial")) %>%
  tab_options(table.width = px(300)) %>% 
  opt_css(
    css = "
    .gt_table {
    border-top-color: #D3D3D3 !important;
    border-top-width: 4px !important;
    }

    ")

# gtsave("Species distribution of tested samples against Brucella spp 20-09-2023.png")

library(htmlwidgets)
library(webshot2)
gtsave(tab1, "tab1.html")
webshot2::webshot("tab1.html",
                  file = "Species distribution of tested samples against Brucella spp 20-09-2023.png",
                  # delay = 10,
                  zoom = 5,
                  selector = c(".gt_table"))


##TUTTO per provincia OK----
tab2 <- pos %>%
  mutate_if(is.character,as.factor) %>% 
  # filter(pr %in% c("BS", "CR")) %>% 
  # filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%
  filter(!specie %in% c(
    "MINILEPRE (SYLVILAGUS FLORIDANUS)",
    "CAMOSCIO",
    "CONIGLIO",
    "CONIGLIO SELVATICO",
    "STAMBECCO")) %>%
  mutate(sex_en = case_when(
    sex == "M" ~ "Males",
    sex == "F" ~ "Females",
    is.na(sex) ~ "Unknown"),
    .after = sex) %>% 
  mutate(age_class = case_when(
    specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "LEPRE" & age == "Giovane" ~ "Juvenile",
    specie == "LEPRE" & age == "Adulto" ~ "Adult",
    specie == "CERVO" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CERVO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CERVO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    specie == "CAPRIOLO" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CAPRIOLO" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    is.na(age) ~ "Unknown",
    TRUE ~ paste0("?")),
    .after = age) %>%
  mutate(comune = str_to_upper(comune)) %>% 
  mutate(pos = case_when(
    esito == "P" ~ 1,
    TRUE ~ 0
  ), .after = esito) %>%
  mutate(specie_en = case_when(
    specie == "CINGHIALE" ~ "WILD BOAR",
    specie == "LEPRE" ~ "HARE",
    specie == "DAINO" ~ "FALLOW DEER",
    specie == "CERVO" ~ "RED DEER",
    specie == "CAPRIOLO" ~ "ROE DEER",
    TRUE ~ paste0(specie)),
    .after = specie) %>% 
  filter(specie_en %in% c("WILD BOAR", "HARE")) %>%   
  #mutate(Provincia = toupper(Provincia)) %>% 
  group_by(specie_en,Provincia) %>% 
  summarise(pos = sum(pos),
            tot = n(),
            prev = sum(pos)/n()) %>% 
  arrange(desc(tot)) %>% 
  select(specie_en, Provincia, tot) %>% 
  pivot_wider(names_from = "specie_en", values_from = "tot", values_fill = 0) %>%
  mutate(total = `WILD BOAR`+`HARE`) %>%
  arrange(desc(total)) %>%
  select(-total) %>%
  gt() %>% 
  cols_label(
    HARE = "Hare",
    `WILD BOAR` = "Wild Boar",
    Provincia = "Province"
  ) %>% 
  cols_align(
    align = c("left"),
    columns = Provincia
  ) %>% 
  # tab_header(
  #   title = md("Geographical distribution of tested samples against *Brucella spp.*")
  #   #subtitle = "Jan 2017 - Dic 2021"
  # ) %>% 
  # tab_style(
  #   style = list(
  #     cell_text(align = "left",
  #               color = "black"
  #               #size = "12px"
  #     ),
  #     cell_fill(color = "#FFFFFF")
#   ),
#     locations = cells_title()) %>% 
tab_style(
  style = list(
    cell_text(weight = "bold")
  ),
  locations = cells_column_labels()) %>%  
    # tab_style(
    #   style = cell_borders(sides = c("top", "bottom"),
    #                        color = "black", weight = px(2)),
    #   locations = cells_column_labels(everything())
    # ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#D3D3D3", weight = px(4)),
      locations = cells_body(rows = 8)
    ) %>%
opt_table_font(font = c("Arial")) %>%
  tab_options(table.width = px(300)) %>% 
    opt_css(
      css = "
    .gt_table {
    border-top-color: #D3D3D3 !important;
    border-top-width: 4px !important;
    }

    ")

tab2  

library(htmlwidgets)
library(webshot2)
gtsave(tab2, "tab2.html")
webshot2::webshot("tab2.html",
                  file = "Geographical distribution of tested samples against Brucella spp 20-09-2023.png",
                  # delay = 10,
                  zoom = 5,
                  selector = c(".gt_table"))

