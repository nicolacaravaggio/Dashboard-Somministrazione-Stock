options("repos" = c("CRAN" = "https://cran.rstudio.com"))

# ==============================================================================
# DASHBOARD - STOCK SOMMINISTRAZIONE
# ==============================================================================

# Required packages
library(rsconnect)
library(xts)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(rgdal)
library(colorspace)
library(plyr)
library(dplyr)
#library(maptools)
#library(rgeos)
#library(maps)
#library(reshape2)
#library(ggspatial)
library(scales)
#library(dummies)
library(data.table)
#library(RJDemetra)
library(sf)
#library(leaflet.providers)
library(RColorBrewer)
library(tidyverse)
library(highcharter)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(viridis)

# Work directory
#if (Sys.getenv('USERNAME')=='Nicola'){
#    setwd("C:/Users/Nicola Caravaggio/OneDrive/Desktop/Dati Ministeriali/Dashboard/Stock")
#} else if (Sys.getenv('USERNAME')=='cilu')
#{Sys.getenv('USERNAME')=='cilu'
#    setwd("C:/Users/NickS/OneDrive/Desktop/Dati Ministeriali/Dashboard/Stock")
#} else {choose.dir()}

# ==============================================================================
# IMPORT DATA
# ------------------------------------------------------------------------------

# Import Shape file
ita_map_reg <- readOGR('data/italy_shape_istat_2020_g/Limiti01012020_g/Reg01012020_g', 
                      'Reg01012020_g_WGS84', stringsAsFactors = FALSE)
ita_map_reg_sf  <- st_as_sf(ita_map_reg)

# Import Tabella mensile
df_m_all <- read.csv('df_stock_m_all.csv', header = TRUE, sep = ";") 
df_m_all$anno_mese <- paste0(df_m_all$anno,"-",df_m_all$mese)
df_m_all$trim <- ifelse(df_m_all$mese %in% c(1,2,3),1,ifelse(df_m_all$mese %in% c(4,5,6),2,ifelse(df_m_all$mese %in% c(7,8,9),3,4)))
df_m_all$trims <- ifelse(df_m_all$mese %in% c(1,2,3),'I trim.',ifelse(df_m_all$mese %in% c(4,5,6),'II trim.',ifelse(df_m_all$mese %in% c(7,8,9),'III trim.','IV trim.')))
df_m_all$anno_trim <- paste0(df_m_all$anno,"-",df_m_all$trim)
df_m_all$anno_trims <- paste0(df_m_all$anno,"-",df_m_all$trims)
df_m_all$ct_all <- ifelse(nchar(df_m_all$ct_cti) > 0, df_m_all$ct_cti, df_m_all$ct)
df_m_all$regione[which(df_m_all$regione == "Friuli-Venezia Giulia")] <- "Friuli Venezia Giulia"

# Raggruppamento per tipo di contratto
df_ct <- df_m_all %>%
    group_by(anno, mese, anno_mese, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), 
              new_entry = sum(new_entry), 
              miss_att = sum(miss_att),
              .groups = 'drop')

# Raggruppamento per tipo di contratto (senza regioni)
df_ct_nr <- df_m_all %>%
    group_by(anno, mese, anno_mese, ct_all) %>%
    summarise(lavoratori = sum(lavoratori), 
              new_entry = sum(new_entry), 
              miss_att = sum(miss_att),
              .groups = 'drop')

# Raggruppamento per tipo di contratto e genere
df_ct_gen <- df_m_all %>%
    group_by(anno, mese, anno_mese, genere, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per tipo di contratto e regione
df_ct_reg <- df_m_all[df_m_all$regione != "N.D.",] %>%
    group_by(anno, mese, anno_mese, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per regione
df_reg <- df_m_all[df_m_all$regione != "N.D.",] %>%
    group_by(anno, mese, anno_mese, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per classi di eta'
df_eta <- df_m_all[df_m_all$eta != "N.D.",] %>%
    group_by(anno, mese, anno_mese, eta, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per nazionalita'
df_cit <- df_m_all[df_m_all$citt != "N.D.",] %>%
  group_by(anno, mese, anno_mese, citt, ct_all, regione) %>%
  summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per settori economici
df_settori <- df_m_all[df_m_all$settori_7 != "N.D.",] %>%
    group_by(anno, mese, anno_mese, settori_7, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per gruppi professionali
df_prof <- df_m_all[df_m_all$prof_8 != "N.D.",] %>%
    group_by(anno, mese, anno_mese, prof_8, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Import Tabella trimestrale
df_t_all <- read.csv('df_stock_t_all.csv', header = TRUE, sep = ";") 
df_t_all$anno_trim <- paste0(df_t_all$anno,"-",df_t_all$trim)
df_t_all$trims <- ifelse(df_t_all$trim == 1,'I trim.',ifelse(df_t_all$trim == 2,'II trim.',ifelse(df_t_all$trim == 3,'III trim.','IV trim.')))
df_t_all$anno_trims <- paste0(df_t_all$anno,"-",df_t_all$trims)
df_t_all$ct_all <- ifelse(nchar(df_t_all$ct_cti) > 0, df_t_all$ct_cti, df_t_all$ct)
df_t_all$regione[which(df_t_all$regione == "Friuli-Venezia Giulia")] <- "Friuli Venezia Giulia"
#df_t_all <- df_t_all[df_t_all$anno_trim != '2021-4',]

# Raggruppamento per tipo di contratto
df_ct_t <- df_t_all %>%
    group_by(anno, trim, anno_trim, anno_trims, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), 
              new_entry = sum(new_entry), 
              miss_att = sum(miss_att),
              .groups = 'drop')

# Raggruppamento per tipo di contratto (senza regioni)
df_ct_t_nr <- df_t_all %>%
    group_by(anno, trim, anno_trim, anno_trims, ct_all) %>%
    summarise(lavoratori = sum(lavoratori), 
              new_entry = sum(new_entry), 
              miss_att = sum(miss_att),
              .groups = 'drop')

# Raggruppamento per tipo di contratto e genere
df_ct_gen_t <- df_t_all %>%
    group_by(anno, trim, anno_trim, anno_trims, genere, ct_all) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per tipo di contratto e regione
df_ct_reg_t <- df_t_all[df_t_all$regione != "N.D.",] %>%
    group_by(anno, trim, anno_trim, anno_trims, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per regione
df_reg_t <- df_t_all[df_t_all$regione != "N.D.",] %>%
    group_by(anno, trim, anno_trim, anno_trims, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per classi di eta
df_eta_t <- df_t_all[df_t_all$eta != "N.D.",] %>%
    group_by(anno, trim, anno_trim, anno_trims, eta, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per nazionalita
df_cit_t <- df_t_all[df_t_all$citt != "N.D.",] %>%
  group_by(anno, trim, anno_trim, anno_trims, citt, ct_all, regione) %>%
  summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per settori economici
df_settori_t <- df_t_all[df_t_all$settori_7 != "N.D.",] %>%
    group_by(anno, trim, anno_trim, anno_trims, settori_7, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Raggruppamento per gruppi professionali
df_prof_t <- df_t_all[df_t_all$prof_8 != "N.D.",] %>%
    group_by(anno, trim, anno_trim, anno_trims, prof_8, ct_all, regione) %>%
    summarise(lavoratori = sum(lavoratori), .groups = 'drop')

# Anni
years = unique(df_m_all$anno)
# Mesi
months = unique(df_m_all$mese)
# Trimestri
trims = unique(df_t_all$trim)
# Regioni
regions = unique(df_m_all$regione)
regions = regions[! regions %in% c("N.D.")]
regions = c("Italia",sort(regions))

# Variabili complete per serie storiche regionali
df_ct_nr_label = df_ct_nr[,1:4]
df_ct_t_nr_label = df_ct_t_nr[,1:5]

# Palette
mycolors_1 <- c('#FDE725FF','#7AD151FF','#22A884FF','#2A788EFF','#414487FF','#440154FF')

# ==============================================================================
# UI
# ------------------------------------------------------------------------------

ui <- dashboardPage(
    
    dashboardHeader(title = "",
                    tags$li(class = "dropdown",
                            tags$a(href = "https://www.assolavoro.eu/", target = "_blank", 
                                   tags$img(height = '20px', src = "assolavoro_logo_lite.png")
                            )),
                    tags$li(class = "dropdown",
                            tags$a(href = "https://economia.uniroma3.it/ricerca/laboratori-e-osservatori/il-lavoro-in-somministrazione-in-italia/", target = "_blank", 
                                   tags$img(height = '20px', src = "roma3_logo.png")
                            )),
                    tags$li(class = "dropdown",
                            tags$a(href = "http://shiny.rstudio.com", target = "_blank", 
                                   tags$img(height = '20px', src = "shiny_logo.png")
                            )),
                    tags$li(class = "dropdown",
                            tags$a(href = "https://www.highcharts.com/", target = "_blank", 
                                   tags$img(height = '20px', src = "highcharts_logo.png")
                            ))
    ),
    dashboardSidebar(
        sidebarMenu(
            #menuItem("Name_here", tabName = 'tab_name', icon = icon("caret-right")),
            # Selezione anno
            #selectInput(inputId = "year_1",
                        #label = "Seleziona anno:",
                        #choices = years,
                        #selected = max(years))
            #,
            # Selezione mese
            #selectInput(inputId = "month_1",
                        #label = "Seleziona mese:",
                        #choices = months,
                        #selected = max(months))
            #,
            # Selezione regione
            selectInput(inputId = "region_1",
                        label = "Seleziona regione:",
                        choices = regions,
                        selected = c("Italia"))
            #,
            # Selezione tipo contratto
            #selectInput(inputId = "ct_1",
                        #label = "Tipo contratto:",
                        #choices = c("Tutti",
                                    #"Tempo determinato", 
                                    #"Tempo indeterminato",
                                    #"Tempo indeterminato (staff leasing)",
                                    #"Tempo indeterminato (con missioni a termine)"),
                        #selected = c("Tutti"))

        )),
    dashboardBody(
        
        # Banner
        titlePanel(img(src = "assolavoro_logo.png", height = "70px", style = "padding-left: 20px; padding-bottom: 15px")),
        
        # Box row
        tabItem(tabName = 'tab_stock_1',
                fluidPage(
                    tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                    tags$head(tags$style(type = 'text/css',".selectize-input { word-wrap: break-word;}")),
                    fluidRow(valueBoxOutput("box_time", width = 3),
                             #valueBoxOutput("box_year", width = 1),
                             #valueBoxOutput("box_month", width = 1),
                             valueBoxOutput("box_stock", width = 2),
                             valueBoxOutput("box_genere", width = 3),
                             valueBoxOutput("box_ctd", width = 2),
                             valueBoxOutput("box_cti", width = 2)
                    ),
                    
                    # First row
                    fluidRow(
                        
                        # Serie storica dello stock
                        box(title = "Lavoratori in somministrazione",
                            width = 7, height = 530, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                                tabPanel("Serie storica mensile", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_ts_m", height = 400),
                                         downloadButton("d_tab_ts_m", "Download")),
                                tabPanel("Serie storica trimestrale", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_ts_t", height = 400),
                                         downloadButton("d_tab_ts_t", "Download"))
                            )),
                        
                        # Mappa delle regioni
                        box(title = "Distribuzione regionale",
                            width = 5, height = 530, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                                tabPanel("Dato mensile, mappa", 
                                         style = "border: 1px solid silver:",
                                         leafletOutput("tab_map_m"),
                                         downloadButton("d_tab_map_m", "Download")),
                                tabPanel("Dato mensile, barplot", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_reg_m", height = 400),
                                         downloadButton("d_tab_reg_m", "Download")),
                                tabPanel("Dato trimestrale, mappa", 
                                         style = "border: 1px solid silver:",
                                         leafletOutput("tab_map_t"),
                                         downloadButton("d_tab_map_t", "Download")),
                                tabPanel("Dato trimestrale, barplot", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_reg_t", height = 400),
                                         downloadButton("d_tab_reg_t", "Download"))
                            ))
                    ),
                    
                    # Second row
                    fluidRow(
                        
                        # Classi di eta'
                        box(title = "Composizione per classi di eta'",
                            width = 2, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                                tabPanel("Dato mensile", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_eta_m", height = 300),
                                         downloadButton("d_tab_eta_m", "Download")),
                                tabPanel("Dato trimestrale", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_eta_t", height = 300),
                                         downloadButton("d_tab_eta_t", "Download"))
                            )),
                        
                        # Nazionalita'
                        box(title = "Composizione per nazionalita'",
                            width = 2, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                              tabPanel("Dato mensile", 
                                       style = "border: 1px solid silver:", 
                                       highchartOutput("tab_cit_m", height = 300),
                                       downloadButton("d_tab_cit_m", "Download")),
                              tabPanel("Dato trimestrale", 
                                       style = "border: 1px solid silver:", 
                                       highchartOutput("tab_cit_t", height = 300),
                                       downloadButton("d_tab_cit_t", "Download"))
                            )),
                        
                        # Settori
                        box(title = "Composizione per settori economici",
                            width = 4, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                                tabPanel("Dato mensile", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_settori_m", height = 300),
                                         downloadButton("d_tab_settori_m", "Download")),
                                tabPanel("Dato trimestrale", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_settori_t", height = 300),
                                         downloadButton("d_tab_settori_t", "Download"))
                            )),
                        
                        # Professioni
                        box(title = "Composizione per grandi gruppi professionali",
                            width = 4, solidHeader = TRUE, status = 'primary',
                            tabsetPanel(
                                tabPanel("Dato mensile", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_prof_m", height = 300),
                                         downloadButton("d_tab_prof_m", "Download")),
                                tabPanel("Dato trimestrale", 
                                         style = "border: 1px solid silver:", 
                                         highchartOutput("tab_prof_t", height = 300),
                                         downloadButton("d_tab_prof_t", "Download"))
                            ))
                    )
                )
        )
    )
)

# ==============================================================================
# SERVER
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Definizione di dataframe reactive
    
    # Dataframe per serie storica
    
    # Mensile
    df_react_ts <- reactive({
         
        if (input$region_1 == "Italia") {
            assign("mydata_1", df_ct)
        } else if (input$region_1 != "Italia") {
            assign("mydata_1", df_ct[df_ct$regione == input$region_1,])
            
            mydata_1 <- left_join(df_ct_nr_label, mydata_1, by = c('anno' = 'anno', 'mese' = 'mese', 'anno_mese' = 'anno_mese', 'ct_all' = 'ct_all'))
            mydata_1$regione[is.na(mydata_1$regione)] <- input$region_1
            mydata_1[is.na(mydata_1)] <- 0
            
        } 
        
        #mydata_1$ct_all[which(mydata_1$ct_all == "CAP")] <- "Apprendistato (CAP)"
        #mydata_1$ct_all[which(mydata_1$ct_all == "CTD")] <- "Tempo Determinato (CTD)"
        #mydata_1$ct_all[which(mydata_1$ct_all == "CTI (multi)")] <- "Tempo indeterminato (con missioni a termine)"
        #mydata_1$ct_all[which(mydata_1$ct_all == "CTI (sl)")] <- "Tempo indeterminato (staff leasing)"
        
        mydata_1
    })
    # Trimestrale
    df_react_ts_t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_1t", df_ct_t)
        } else if (input$region_1 != "Italia") {
            assign("mydata_1t", df_ct_t[df_ct_t$regione == input$region_1,])
            
            mydata_1t <- left_join(df_ct_t_nr_label, mydata_1t, by = c('anno' = 'anno', 'trim' = 'trim', 'anno_trim' = 'anno_trim', 'anno_trims' = 'anno_trims', 'ct_all' = 'ct_all'))
            mydata_1t$regione[is.na(mydata_1t$regione)] <- input$region_1
            mydata_1t[is.na(mydata_1t)] <- 0
        } 
        
        #mydata_1t$ct_all[which(mydata_1t$ct_all == "CAP")] <- "Apprendistato (CAP)"
        #mydata_1t$ct_all[which(mydata_1t$ct_all == "CTD")] <- "Tempo Determinato (CTD)"
        #mydata_1t$ct_all[which(mydata_1t$ct_all == "CTI (multi)")] <- "Tempo indeterminato (con missioni a termine)"
        #mydata_1t$ct_all[which(mydata_1t$ct_all == "CTI (sl)")] <- "Tempo indeterminato (staff leasing)"
        
        mydata_1t
    })
    
    
    # Dataframe per classi di eta
    
    # Mensile
    df_react_eta <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_2", df_eta[df_eta$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_2", df_eta[df_eta$anno == max(years) & df_eta$regione == input$region_1,])
        } 
        
        mydata_2
    })
    # Trimestrale
    df_react_eta_t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_2t", df_eta_t[df_eta_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_2t", df_eta_t[df_eta_t$anno == max(years) & df_eta_t$regione == input$region_1,])
        } 
        
        mydata_2t
    })
    
    # Dataframe per nazionalita
    
    # Mensile
    df_react_cit <- reactive({
      
      if (input$region_1 == "Italia") {
        assign("mydata_3", df_cit[df_cit$anno == max(years),])
      } else if (input$region_1 != "Italia") {
        assign("mydata_3", df_cit[df_cit$anno == max(years) & df_cit$regione == input$region_1,])
      } 
      
      mydata_3
    })
    # Trimestrale
    df_react_cit_t <- reactive({
      
      if (input$region_1 == "Italia") {
        assign("mydata_3t", df_cit_t[df_cit_t$anno == max(years),])
      } else if (input$region_1 != "Italia") {
        assign("mydata_3t", df_cit_t[df_cit_t$anno == max(years) & df_cit_t$regione == input$region_1,])
      } 
      
      mydata_3t
    })
    
    # Dataframe per settori
    
    # Mensile
    df_react_settori <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_4", df_settori[df_settori$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_4", df_settori[df_settori$anno == max(years) & df_settori$regione == input$region_1,])
        } 
        
        mydata_4
    })
    # Trimestrale
    df_react_settori_t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_4t", df_settori_t[df_settori_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_4t", df_settori_t[df_settori_t$anno == max(years) & df_settori_t$regione == input$region_1,])
        } 
        
        mydata_4t
    })
    
    # Dataframe per professioni
    
    # Mensile
    df_react_prof <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_5", df_prof[df_prof$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_5", df_prof[df_prof$anno == max(years) & df_prof$regione == input$region_1,])
        } 
        
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 1,2")] <- "GGP 1, 2 - Legislatori, imprenditori e alta dirigenza; Professioni intellettuali, scientifiche e di elevata specializzazione"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 3")] <- "GGP 3 - Professioni tecniche"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 4")] <- "GGP 4 - Professioni esecutive nel lavoro d'ufficio"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 5")] <- "GGP 5 - Professioni qualificate nelle attivita' commerciali e nei servizi"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 6")] <- "GGP 6 - Artigiani, operai specializzati e agricoltori"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 7")] <- "GGP 7 - Conduttori di impianti, operai di macchinari fissi e mobili e conducenti di veicoli"
        mydata_5$prof_8[which(mydata_5$prof_8 == "GGP 8")] <- "GGP 8 - Professioni non qualificate"
        
        mydata_5
    })
    # Trimestrale
    df_react_prof_t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_5t", df_prof_t[df_prof_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_5t", df_prof_t[df_prof_t$anno == max(years) & df_prof_t$regione == input$region_1,])
        } 
        
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 1,2")] <- "GGP 1, 2 - Legislatori, imprenditori e alta dirigenza; Professioni intellettuali, scientifiche e di elevata specializzazione"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 3")] <- "GGP 3 - Professioni tecniche"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 4")] <- "GGP 4 - Professioni esecutive nel lavoro d'ufficio"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 5")] <- "GGP 5 - Professioni qualificate nelle attivita' commerciali e nei servizi"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 6")] <- "GGP 6 - Artigiani, operai specializzati e agricoltori"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 7")] <- "GGP 7 - Conduttori di impianti, operai di macchinari fissi e mobili e conducenti di veicoli"
        mydata_5t$prof_8[which(mydata_5t$prof_8 == "GGP 8")] <- "GGP 8 - Professioni non qualificate"
        
        mydata_5t
    })
    
    # Dataframe per genere
    
    # Mensile
    df_react_gen <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_6", df_ct_gen[df_ct_gen$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_6", df_ct_gen[df_ct_gen$anno == max(years) & df_ct_gen$regione == input$region_1,])
        } 
        
        mydata_6
    })
    # Trimestrale
    df_react_gen_t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_6t", df_ct_gen_t[df_ct_gen_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_6t", df_ct_gen_t[df_ct_gen_t$anno == max(years) & df_ct_gen_t$regione == input$region_1,])
        } 
        
        mydata_6t
    })
    
    # Dataframe per regioni
    
    # Mensile pt.1
    df_react_reg_1 <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_71", df_ct_reg[df_ct_reg$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_71", df_ct_reg[df_ct_reg$anno == max(years) & df_ct_reg$regione == input$region_1,])
        } 
        
        mydata_71
    })
    # Mensile pt.2
    df_react_reg_2 <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_72", df_reg[df_reg$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_72", df_reg[df_reg$anno == max(years) & df_reg$regione == input$region_1,])
        } 
        
        mydata_72
    })
    # Trimestrale pt.1
    df_react_reg_1t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_71t", df_ct_reg_t[df_ct_reg_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_71t", df_ct_reg_t[df_ct_reg_t$anno == max(years) & df_ct_reg_t$regione == input$region_1,])
        } 
        
        mydata_71t
    })
    # Trimestrale pt.2
    df_react_reg_2t <- reactive({
        
        if (input$region_1 == "Italia") {
            assign("mydata_72t", df_reg_t[df_reg_t$anno == max(years),])
        } else if (input$region_1 != "Italia") {
            assign("mydata_72t", df_reg_t[df_reg_t$anno == max(years) & df_reg_t$regione == input$region_1,])
        } 
        
        mydata_72t
    })
    
    # Dataframe per boxplot numero lavoratori
    
    # Mensile
    df_box_1 <- reactive({
        
        box_data_1 <- df_react_ts() %>%
            group_by(anno_mese, anno, mese) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        box_data_1 <- box_data_1[box_data_1$anno == max(box_data_1$anno),]
        box_data_1 <- box_data_1[box_data_1$mese == max(box_data_1$mese),]
        box_data_1
    })
    
    # Trimestrale
    df_box_1_t <- reactive({
        
        box_data_1t <- df_react_ts_t() %>%
            group_by(anno_trim, anno_trims, anno, trim) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        box_data_1t <- box_data_1t[box_data_1t$anno == max(box_data_1t$anno),]
        box_data_1t <- box_data_1t[box_data_1t$trim == max(box_data_1t$trim),]
        box_data_1t
    }) 
    
    # Dataframe per boxplot genere
    df_box_2 <- reactive({
        
        box_data_2 <- df_react_gen() %>%
            group_by(anno_mese, anno, mese, genere) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        box_data_2 <- box_data_2[box_data_2$anno == max(box_data_2$anno),]
        box_data_2 <- box_data_2[box_data_2$mese == max(box_data_2$mese),]
        box_data_2
    })

    # Dataframe per boxplot contratti
    df_box_3 <- reactive({
        
        box_data_3 <- df_react_ts() %>%
            group_by(anno_mese, anno, mese, ct_all) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        box_data_3 <- box_data_3[box_data_3$anno == max(box_data_3$anno),]
        box_data_3 <- box_data_3[box_data_3$mese == max(box_data_3$mese),]
        box_data_3
    })
    
    
    # Value box: Time range selezionato
    
    output$box_time = renderValueBox({
        
        valueBox(value = paste0(min(df_react_ts()$anno)," - ",tail(df_box_1_t()[,c("anno_trims")],1)),
                 subtitle = "Periodo di riferimento",
                 color = "yellow",
                 icon = icon("calendar-alt")
        )
    })
    
    # Value box: Anno selezionato
    
    output$box_year = renderValueBox({
        
        valueBox(value = input$year_1,
                 subtitle = "Anno",
                 color = "yellow",
                 icon = icon("calendar-alt")
        )
    })
    
    # Value box: Mese selezionato
    
    output$box_month = renderValueBox({
        
        valueBox(value = input$month_1,
                 subtitle = "Mese",
                 color = "yellow",
                 icon = icon("calendar-alt")
        )
    })
    
    # Value box: Numero di lavoratori
    
    output$box_stock = renderValueBox({
        
        valueBox(value = round(tail(df_box_1()[,c("lavoratori")],1),0),
                 subtitle = paste0("Lavoratori in somministrazione al ",tail(df_box_1()[,c("anno_mese")],1)),
                 color = "olive",
                 icon = icon("sticky-note")
        )
    })
    
    # Value box: Genere
    
    output$box_genere = renderValueBox({
        
        df_box_genere = tail(df_box_2()[,c("genere","lavoratori")],2)
        
        valueBox(value = 
                     paste0(100*round(df_box_genere[df_box_genere$genere == "Uomini",c("lavoratori")] / sum(df_box_genere$lavoratori),3),"% uomini, ",
                            100*round(df_box_genere[df_box_genere$genere == "Donne",c("lavoratori")] / sum(df_box_genere$lavoratori),3),"% donne"
                            ),
                 subtitle = paste0("Ripartizione per genere al ",tail(df_box_1()[,c("anno_mese")],1)),
                 color = "blue",
                 icon = icon("fa-regular fa-address-card")
        )
    })
    
    # Value box: Numero di lavoratori con CTD
    
    output$box_ctd = renderValueBox({
        
        df_box_ctd = tail(df_box_3()[,c("ct_all","lavoratori")],4)
        
        valueBox(value = paste0(100*round(df_box_ctd[df_box_ctd$ct_all == "CTD",c("lavoratori")] / sum(df_box_ctd$lavoratori),3),"%"),
                 subtitle = paste0("Contratto a tempo determinato al ",tail(df_box_1()[,c("anno_mese")],1)),
                 color = "teal",
                 icon = icon("book")
        )
    })
    
    # Value box: Numero di lavoratori con CTI
    
    output$box_cti = renderValueBox({
        
        df_box_cti = tail(df_box_3()[,c("ct_all","lavoratori")],4)
        
        valueBox(value = paste0(100*round(sum(df_box_cti[df_box_cti$ct_all %like% "^CTI",c("lavoratori")]) / sum(df_box_cti$lavoratori),3),"%"),
                 subtitle = paste0("Contratto a tempo indeterminato al ",tail(df_box_1()[,c("anno_mese")],1)),
                 color = "teal",
                 icon = icon("book")
        )
    })
    
    # Chart Stock
    
    # Mensile
    output$tab_ts_m = renderHighchart({ 
        
        df_miss_pre <- df_react_ts() %>%
            group_by(anno_mese, anno, mese) %>%
            summarise(lavoratori = sum(lavoratori),
                      new_entry = sum(new_entry),
                      miss_att = sum(miss_att),
                      .groups = 'drop')
        
        df_ct_miss <- df_miss_pre[order(df_miss_pre$anno, df_miss_pre$mese),]

        df_react_ts() %>%
            hchart(type = 'area', hcaes(x = anno_mese, y = lavoratori, group = 'ct_all')) %>%
            hc_plotOptions(series = list(stacking = "normal", marker = list(enabled = FALSE))) %>%
            hc_xAxis(title = list(text = "Periodo")) %>%
            hc_yAxis(title = list(text = "Numero di lavoratori")) %>%
            hc_colors(mycolors_1) %>%
            hc_caption(text = paste0("CAP = Apprendistato, 
                                      CTD = Tempo determinato, 
                                      CTI (multi) = Tempo indeterminato con missioni a termine,
                                      CTI (sl) = Tempo indeterminato con staff leasing")) %>%
            hc_add_series(name = "Con missione attiva",
                          data = df_ct_miss,
                          type = "line",
                          color = "red",
                          hcaes(x = anno_mese, y = miss_att)
            )
        
    })
    # Download (.csv) dei dati
    output$d_tab_ts_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_ts(), file, row.names = FALSE)
        }
    )

    # Trimestrale
    output$tab_ts_t = renderHighchart({ 
        
        df_miss_pre_t <- df_react_ts_t() %>%
            group_by(anno_trim, anno, trim) %>%
            summarise(lavoratori = sum(lavoratori),
                      new_entry = sum(new_entry),
                      miss_att = sum(miss_att),
                      .groups = 'drop')
        
        df_ct_miss_t <- df_miss_pre_t[order(df_miss_pre_t$anno, df_miss_pre_t$trim),]
        
        df_react_ts_t() %>%
            hchart(type = 'area', hcaes(x = anno_trim, y = lavoratori, group = 'ct_all')) %>%
            hc_plotOptions(series = list(stacking = "normal", marker = list(enabled = FALSE))) %>%
            hc_xAxis(title = list(text = "Periodo")) %>%
            hc_yAxis(title = list(text = "Numero di lavoratori")) %>%
            hc_colors(mycolors_1) %>%
            hc_caption(text = paste0("CAP = Apprendistato, 
                                      CTD = Tempo determinato, 
                                      CTI (multi) = Tempo indeterminato con missioni a termine,
                                      CTI (sl) = Tempo indeterminato con staff leasing")) %>%
            hc_add_series(name = "Con missione attiva",
                          data = df_ct_miss_t,
                          type = "line",
                          color = "red",
                          hcaes(x = anno_trim, y = miss_att)
            )
        
    })
    # Download (.csv) dei dati
    output$d_tab_ts_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_ts_t(), file, row.names = FALSE)
        }
    )
    
    # Map Regioni
    
    # Mensile
    output$tab_map_m = renderHighchart({ 
        
        df_reg_map <- df_react_reg_1()[df_react_reg_1()$mese == max(df_react_reg_1()$mese),] %>%
            group_by(regione) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        # Adjustements and merge with map
        df_reg_map[, "DEN_REG"] = df_reg_map[, c("regione")]
        map_reg  = left_join(ita_map_reg_sf, df_reg_map, by = "DEN_REG")
        
        # Create palette 
        palette_reg = colorBin("YlGnBu", domain = map_reg$lavoratori, na.color = "transparent", bins = 5)
        
        # Convert map format to a latitude-longitude format
        st_map_reg = st_transform(map_reg, crs = 4326)
        
        # Set labels (in JAVA)
        labels <- sprintf(
            "<b>Regione</b>: %s <br/> <b>Lavoratori</b>: %s <br/> <b>Quota</b>: %s <br/>",
            map_reg$DEN_REG,
            round(map_reg$lavoratori,0),
            paste0(round(map_reg$lavoratori / sum(map_reg$lavoratori),4)*100,"%")
        ) %>% lapply(htmltools::HTML)
        
        # Map
        st_map_reg %>%
            st_transform(crs = 4326) %>%
            leaflet() %>%
            addProviderTiles("Esri.WorldGrayCanvas") %>%
            setView(lat = 41.8719, lng = 12.5674, zoom = 5) %>%
            addPolygons( # Brackground
                data = st_map_reg,
                weight = 1,
                opacity = 0.8,
                color = "grey",
                smoothFactor = 0.2,
                fillOpacity = 0) %>%
            addPolygons( # Fill
                data = st_map_reg,
                fillColor = ~palette_reg(lavoratori),
                weight = 0.8,
                opacity = 0.4,
                color = "white",
                smoothFactor = 0.2,
                fillOpacity = 0.3,
                highlight = highlightOptions(
                    weight = 2,
                    color = "white",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto") 
            ) %>%
            addLegend(position = "bottomleft",
                      pal = palette_reg,
                      values = map_reg$lavoratori,
                      title = "Lavoratori",
                      opacity = 0.8)
        
    })
    # Download (.csv) dei dati
    output$d_tab_map_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_reg_1(), file, row.names = FALSE)
        }
    )
    
    # Trimestrale
    output$tab_map_t = renderHighchart({ 
        
        df_reg_map <- df_react_reg_1t()[df_react_reg_1t()$trim == max(df_react_reg_1t()$trim),] %>%
            group_by(regione) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop')
        
        # Adjustements and merge with map
        df_reg_map[, "DEN_REG"] = df_reg_map[, c("regione")]
        map_reg  = left_join(ita_map_reg_sf, df_reg_map, by = "DEN_REG")
        
        # Create palette 
        palette_reg = colorBin("YlGnBu", domain = map_reg$lavoratori, na.color = "transparent", bins = 5)
        
        # Convert map format to a latitude-longitude format
        st_map_reg = st_transform(map_reg, crs = 4326)
        
        # Set labels (in JAVA)
        labels <- sprintf(
            "<b>Regione</b>: %s <br/> <b>Lavoratori</b>: %s <br/> <b>Quota</b>: %s <br/>",
            map_reg$DEN_REG,
            round(map_reg$lavoratori,0),
            paste0(round(map_reg$lavoratori / sum(map_reg$lavoratori),4)*100,"%")
        ) %>% lapply(htmltools::HTML)
        
        # Map
        st_map_reg %>%
            st_transform(crs = 4326) %>%
            leaflet() %>%
            addProviderTiles("Esri.WorldGrayCanvas") %>%
            setView(lat = 41.8719, lng = 12.5674, zoom = 5) %>%
            addPolygons( # Background
                data = st_map_reg,
                weight = 1,
                opacity = 0.8,
                color = "grey",
                smoothFactor = 0.2,
                fillOpacity = 0) %>%
            addPolygons( # Fill
                data = st_map_reg,
                fillColor = ~palette_reg(lavoratori),
                weight = 0.8,
                opacity = 0.4,
                color = "white",
                smoothFactor = 0.2,
                fillOpacity = 0.3,
                highlight = highlightOptions(
                    weight = 2,
                    color = "white",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto") 
            ) %>%
            addLegend(position = "bottomleft",
                      pal = palette_reg,
                      values = map_reg$lavoratori,
                      title = "Lavoratori",
                      opacity = 0.8)
        
    })
    # Download (.csv) dei dati
    output$d_tab_map_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_reg_1t(), file, row.names = FALSE)
        }
    )

    # Chart Regioni
    
    # Mensile
    output$tab_reg_m = renderHighchart({ 
        
        df_react_reg_1()[df_react_reg_1()$mese == max(df_react_reg_1()$mese),] %>%
            group_by(regione, ct_all) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'column', hcaes(x = regione, y = lavoratori, group = ct_all)) %>% 
            hc_plotOptions(series = list(stacking = "normal")) %>%
            hc_xAxis(title = list(text = "Regioni")) %>%
            hc_yAxis(title = list(text = "Numero di lavoratori")) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1()[,c("anno_mese")],1),
                                     " - ",
                                     "CAP = Apprendistato, 
                                      CTD = Tempo determinato, 
                                      CTI (multi) = Tempo indeterminato con missioni a termine,
                                      CTI (sl) = Tempo indeterminato con staff leasing")) %>%
            hc_colors(mycolors_1)
    
    })
    # Download (.csv) dei dati
    output$d_tab_reg_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_reg_1(), file, row.names = FALSE)
        }
    )
        
    # Trimestrale
    output$tab_reg_t = renderHighchart({ 
            
        df_react_reg_1t()[df_react_reg_1t()$trim == max(df_react_reg_1t()$trim),] %>%
            group_by(regione, ct_all) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'column', hcaes(x = regione, y = lavoratori, group = ct_all)) %>% 
            hc_plotOptions(series = list(stacking = "normal")) %>%
            hc_xAxis(title = list(text = "Regioni")) %>%
            hc_yAxis(title = list(text = "Numero di lavoratori")) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1_t()[,c("anno_trims")],1),
                                     " - ",
                                     "CAP = Apprendistato, 
                                      CTD = Tempo determinato, 
                                      CTI (multi) = Tempo indeterminato con missioni a termine,
                                      CTI (sl) = Tempo indeterminato con staff leasing")) %>%
            hc_colors(mycolors_1)

    })
    # Download (.csv) dei dati
    output$d_tab_reg_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_reg_1t(), file, row.names = FALSE)
        }
    )
    
    # Chart Classi di eta'
    
    # Mensile
    output$tab_eta_m = renderHighchart({ 
        
        df_react_eta()[df_react_eta()$mese == max(df_react_eta()$mese),] %>%
            group_by(eta) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = eta, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1()[,c("anno_mese")],1)))
        
    })
    # Download (.csv) dei dati
    output$d_tab_eta_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_eta(), file, row.names = FALSE)
        }
    )

    # Trimestrale
    output$tab_eta_t = renderHighchart({ 
        
        df_react_eta_t()[df_react_eta_t()$trim == max(df_react_eta_t()$trim),] %>%
            group_by(eta) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = eta, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1_t()[,c("anno_trims")],1)))
        
    })
    
    # Download (.csv) dei dati
    output$d_tab_eta_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_eta_t(), file, row.names = FALSE)
        }
    )
    
    # Chart Nazionalita'
    
    # Mensile
    output$tab_cit_m = renderHighchart({ 
      
      df_react_cit()[df_react_cit()$mese == max(df_react_cit()$mese),] %>%
        group_by(citt) %>%
        summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
        hchart(type = 'treemap', hcaes(x = citt, value = lavoratori, color = lavoratori)) %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
        hc_legend(enabled = FALSE) %>%
        hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1()[,c("anno_mese")],1)))
      
    })
    # Download (.csv) dei dati
    output$d_tab_cit_m <- downloadHandler(
      filename = function() {
        paste("mydata.csv", sep = ";")
      },
      content = function(file) {
        write.csv(df_react_cit(), file, row.names = FALSE)
      }
    )
    
    # Trimestrale
    output$tab_cit_t = renderHighchart({ 
      
      df_react_cit_t()[df_react_cit_t()$trim == max(df_react_cit_t()$trim),] %>%
        group_by(citt) %>%
        summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
        hchart(type = 'treemap', hcaes(x = citt, value = lavoratori, color = lavoratori)) %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
        hc_legend(enabled = FALSE) %>%
        hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1_t()[,c("anno_trims")],1)))
      
    })
    
    # Download (.csv) dei dati
    output$d_tab_cit_t <- downloadHandler(
      filename = function() {
        paste("mydata.csv", sep = ";")
      },
      content = function(file) {
        write.csv(df_react_cit_t(), file, row.names = FALSE)
      }
    )

    # Chart Settori economici
    
    # Mensile
    output$tab_settori_m = renderHighchart({ 

        df_react_settori()[df_react_settori()$mese == max(df_react_settori()$mese),] %>%
            group_by(settori_7) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = settori_7, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1()[,c("anno_mese")],1)))
        
    })
    # Download (.csv) dei dati
    output$d_tab_settori_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_settori(), file, row.names = FALSE)
        }
    )

    # Trimestrale
    output$tab_settori_t = renderHighchart({ 
        
        df_react_settori_t()[df_react_settori_t()$trim == max(df_react_settori_t()$trim),] %>%
            group_by(settori_7) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = settori_7, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1_t()[,c("anno_trims")],1)))
    
    })
    # Download (.csv) dei dati
    output$d_tab_settori_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_settori_t(), file, row.names = FALSE)
        }
    )
    
    
    # Chart Gruppi professionali
    
    # Mensile
    output$tab_prof_m = renderHighchart({ 

        df_react_prof()[df_react_prof()$mese == max(df_react_prof()$mese),] %>%
            group_by(prof_8) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = prof_8, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1()[,c("anno_mese")],1)))
    
    })
    # Download (.csv) dei dati
    output$d_tab_prof_m <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_prof(), file, row.names = FALSE)
        }
    )

    # Trimestrale
    output$tab_prof_t = renderHighchart({ 
        
        df_react_prof_t()[df_react_prof_t()$trim == max(df_react_prof_t()$trim),] %>%
            group_by(prof_8) %>%
            summarise(lavoratori = sum(lavoratori), .groups = 'drop') %>%
            hchart(type = 'treemap', hcaes(x = prof_8, value = lavoratori, color = lavoratori)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
            hc_legend(enabled = FALSE) %>%
            hc_caption(text = paste0("I seguenti dati si riferiscono al ",tail(df_box_1_t()[,c("anno_trims")],1)))
    
    })
    # Download (.csv) dei dati
    output$d_tab_prof_t <- downloadHandler(
        filename = function() {
            paste("mydata.csv", sep = ";")
        },
        content = function(file) {
            write.csv(df_react_prof_t(), file, row.names = FALSE)
        }
    )
    
}

# ==============================================================================
# RUN APP
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

# ==============================================================================
# DEPLOY
# ------------------------------------------------------------------------------

#rsconnect::deployApp() 