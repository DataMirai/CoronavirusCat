rsconnect::setAccountInfo(name='mireia-camacho',
                          token='7BE4CFADE2C76C06D02E314D399F5F17',
                          secret='46bgAXawNhYKR//CPEP1OX6Bp026G6RKIc4B7S+k')

library(tidyverse)
library(RSocrata)
library(shiny) 
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(zoo)
library(usethis)
library(htmltools)
options(scipen=10000)
### Carga dataframes ----

### Incidencia ----
df <- as_tibble(read.socrata("https://analisi.transparenciacatalunya.cat/resource/c7sd-zy9j.json?$select=data,casos_confirmat,pcr,ingressos_total,ingressos_critic,ingressats_total,ingressats_critic,exitus")) 

df2 <- as.data.frame(df)
df3 <- df2 %>%
  mutate_at(vars(c(casos_confirmat, pcr, ingressos_total, ingressos_critic, ingressats_total, ingressats_critic, exitus)), as.numeric)%>%
  mutate(data = as.POSIXct(data))

df_agr<- aggregate(. ~data, df3, sum, na.rm = TRUE)

df_agr$casos7dies <- rollsum(df_agr$casos_confirmat, k=7, fill = NA, align = 'right')
df_agr$casos14dies <- rollsum(df_agr$casos_confirmat, k=14, fill=NA, align = 'right')
df_agr$ingressats7dies <- rollsum(df_agr$ingressats_total, k=7, fill=NA, align = 'right')
df_agr$ingressats14dies <- rollsum(df_agr$ingressats_total, k=14, fill=NA, align = 'right')
df_agr$critic7dies <- rollsum(df_agr$ingressats_critic, k=7, fill=NA, align = 'right')
df_agr$critic14dies <- rollsum(df_agr$ingressats_critic, k=14, fill=NA, align = 'right')
df_agr$morts14dies <- rollsum(df_agr$exitus, k=14, fill=NA, align = 'right')

casos14 <- tail(df_agr$casos14dies, n=2)
casos7 <- tail(df_agr$casos7dies, n=2)
ingressos14 <- tail(df_agr$ingressats14dies, n=2)
ingressos7 <- tail(df_agr$ingressats7dies, n=2)
critics14 <- tail(df_agr$critic14dies, n=2)
critics7 <- tail(df_agr$critic7dies, n=2)
morts14 <- tail(df_agr$morts14dies, n=2)

### Vacunació ----
vacunes <- as_tibble(read.socrata("https://analisi.transparenciacatalunya.cat/resource/cuwj-bh3b.json?$select=comarca,data,sexe,edat,recompte,fabricant,dosi"))

vacunes_filtered <- vacunes%>%
  filter(fabricant != "No administrada" & comarca != "No classificat")%>%
  mutate(recompte = as.numeric(recompte))

### Vacunes aggregate sexe ----
vacunes_agr<- aggregate(recompte ~comarca + data + sexe, vacunes_filtered, sum, na.rm = TRUE)

vacunes_catalunya <- vacunes_filtered%>%
  dplyr::select(-comarca)%>%
  group_by(data, sexe, edat, dosi, fabricant)
vacunes_cat<- aggregate(recompte ~ data + sexe, vacunes_catalunya, sum, na.rm = TRUE)

vacunes_cat$comarca <- "CATALUNYA"

vacunes_agr <- rbind(vacunes_agr, vacunes_cat)

### Vacunes aggregate edat ----
vacunes_agr2<- aggregate(recompte ~comarca + data + edat, vacunes_filtered, sum, na.rm = TRUE)

vacunes_cat2<- aggregate(recompte ~ data + edat, vacunes_catalunya, sum, na.rm = TRUE)

vacunes_cat2$comarca <- "CATALUNYA"

vacunes_agr2 <- rbind(vacunes_agr2, vacunes_cat2)

### Vacunes aggregate dosis ----

vacunes_agr3 <- aggregate(recompte ~comarca + sexe + edat + dosi, vacunes_filtered, sum, na.rm = TRUE)
vacunes_cat3 <- aggregate(recompte ~ sexe + edat + dosi, vacunes_catalunya, sum, na.rm = TRUE)

vacunes_cat3$comarca <- "CATALUNYA"

vacunes_agr3 <- rbind(vacunes_agr3, vacunes_cat3)

### Header ----
header <- shinydashboard::dashboardHeader(title = "Covid Catalunya",
                                          tags$li(class="dropdown", tags$a(href= "https://twitter.com/mireiacamacho75", icon("twitter"), target="_blank")),
                                          tags$li(class="dropdown", tags$a(href= "https://www.linkedin.com/in/mireia-camacho-695475143/", icon("linkedin"),target="_blank")),
                                          tags$li(class="dropdown", tags$a(href= "https://www.paypal.com/paypalme/mireiacamacho", icon("paypal"), target="_blank")))

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarID",
    menuItem("Incidència", tabName = "incidencia", icon = icon("map-marker-alt")),
    menuItem("Vacunació", tabName = "vacunacio", icon = icon("syringe")),
    menuItem("Sobre l'app", tabName = "app", icon = icon("info-circle"))
    
  )
)

### Body ----
body <- shinydashboard::dashboardBody(
  fluidRow( 
    
    shinydashboard::tabItems(
      ###INCIDENCIA ----
      shinydashboard::tabItem("incidencia",
                              h2("Incidència Catalunya", style="margin:15px;"),
                              h5(tags$a(href= "https://analisi.transparenciacatalunya.cat/Salut/Dades-di-ries-de-COVID-19-per-comarca/c7sd-zy9j", "Dades d'incidència del portal de Dades Obertes"),
                                 style="margin:15px;
                                 text-align: right;"),
                              
                              h3("Contagis", style="margin:15px;"),
                              shinydashboard::infoBox("Casos acumulats", prettyNum(sum(df_agr$casos_confirmat), big.mark = ".", decimal.mark=","), 
                                                      paste0( "+", prettyNum(tail(df_agr$casos_confirmat, n=1),big.mark = ".", decimal.mark=","), " Avui"),icon = icon("user-md"), color="aqua"),
                              shinydashboard::infoBox("Casos 7 dies", prettyNum(tail(df_agr$casos7dies, n=1), big.mark = ".", decimal.mark=","), 
                                                      paste0(ifelse(casos7[2]-casos7[1] >= 0, "+", ""), prettyNum(casos7[2]-casos7[1],big.mark = ".", decimal.mark=","), " Últims 7 dies"), icon = icon("file-medical"), color="aqua"),
                              shinydashboard::infoBox("Casos 14 dies", prettyNum(tail(df_agr$casos14dies, n=1),big.mark = ".", decimal.mark=","), 
                                                      paste0(ifelse(casos14[2]-casos14[1] >= 0, "+", ""), prettyNum(casos14[2]-casos14[1],big.mark = ".", decimal.mark=","), " Últims 14 dies"), icon = icon("file-medical"), color="aqua"),
                              shinydashboard::box( width = 12,
                                                   plotly::plotlyOutput("casos", height = 300),
                                                   radioButtons("radbutCont", "Selecciona:", c("Casos diaris" = "casos_diaris","Casos 7 dies" = "casos_7dies", "Casos 14 dies" = "casos_14dies"), selected = "casos_7dies")),
                              
                              
                              h3("Hospitalitzacions", style="margin:15px;"),
                              shinydashboard::infoBox("Ingressats actualment", prettyNum(tail(df_agr$ingressats_total, n=1),big.mark = ".", decimal.mark=","), 
                                                      paste0( "Hosp. acumulades: ", prettyNum(sum(df_agr$ingressats_total), big.mark = ".", decimal.mark=",")),icon = icon("user-md"), color="olive", width = 6),
                              shinydashboard::infoBox("Ingressos avui", prettyNum(tail(df_agr$ingressos_total, n=1),big.mark = ".", decimal.mark=","), icon = icon("procedures"), color="olive", width = 6), 
                              shinydashboard::infoBox("Hospitalitzacions 7 dies", prettyNum(tail(df_agr$ingressats7dies, n=1),big.mark = ".", decimal.mark=","), 
                                                      paste0(ifelse(ingressos7[2]-ingressos7[1] >= 0, "+", ""), prettyNum(ingressos7[2]-ingressos7[1],big.mark = ".", decimal.mark=","), " Últims 7 dies"), icon = icon("file-medical"), color="olive", width = 6),
                              shinydashboard::infoBox("Hospitalitzacions 14 dies", prettyNum(tail(df_agr$ingressats14dies, n=1),big.mark = ".", decimal.mark=","), 
                                                      paste0(ifelse(ingressos14[2]-ingressos14[1] >= 0, "+", ""), prettyNum(ingressos14[2]-ingressos14[1],big.mark = ".", decimal.mark=","), " Últims 14 dies"), icon = icon("file-medical"), color="olive", width = 6),
                              shinydashboard::box( width = 12,
                                                   plotly::plotlyOutput("incidencia_hosp", height = 300),
                                                   radioButtons("radbutHosp", "Selecciona:", c("Ingressats actualment" = "ingressats_actual","Ingressats 7 dies" = "ingressats_7dies","Ingressats 14 dies" = 
                                                                                                 "ingressats_14dies"), selected = "ingressats_7dies")),
                              
                              h3("UCI", style="margin:15px;"),
                              shinydashboard::infoBox("Ingressats UCI actualment", prettyNum(tail(df_agr$ingressats_critic, n=1),big.mark = ".", decimal.mark=","), 
                                                      paste0( "UCI acumulades: ", prettyNum(sum(df_agr$ingressats_critic), big.mark = ".", decimal.mark=",")),icon = icon("user-md"), color="yellow", width = 6),
                              shinydashboard::infoBox("Ingressos avui", prettyNum(tail(df_agr$ingressos_critic, n=1),big.mark = ".", decimal.mark=","), icon = icon("procedures"), color="yellow", width = 6), 
                              shinydashboard::infoBox("UCI 7 dies", prettyNum(tail(df_agr$critic7dies, n=1), big.mark = ".", decimal.mark=","), 
                                                      paste0( ifelse(critics7[2]-critics7[1] >= 0, "+", ""), prettyNum(critics7[2]-critics7[1], big.mark = ".", decimal.mark=","), " Últims 7 dies"), icon = icon("file-medical"), color="yellow", width = 6),
                              shinydashboard::infoBox("UCI 14 dies", prettyNum(tail(df_agr$critic14dies, n=1), big.mark = ".", decimal.mark=","), 
                                                      paste0( ifelse(critics14[2]-critics14[1] >= 0, "+", ""), prettyNum(critics14[2]-critics14[1], big.mark = ".", decimal.mark=","), " Últims 14 dies"), icon = icon("file-medical"), color="yellow", width = 6),
                              shinydashboard::box( width = 12,
                                                   plotly::plotlyOutput("incidencia_uci", height = 300),
                                                   radioButtons("radbutUci", "Selecciona:", c("Ingressats UCI actualment" = "uci_actual","Ingressats UCI 7 dies" = "uci_7dies","Ingressats UCI 14 dies" = "uci_14dies"), selected = "uci_7dies")),
                              
                              h3("Defuncions", style="margin:15px;"),
                              shinydashboard::infoBox("Defuncions acumulades", prettyNum(sum(df_agr$exitus), big.mark = ".", decimal.mark=","), 
                                                      paste0( "+", prettyNum(tail(df_agr$exitus, n=2)[1],big.mark = ".", decimal.mark=","), " Ahir"), icon = icon("user-md"), color="red"),
                              shinydashboard::infoBox("Defuncions 14 dies", prettyNum(tail(df_agr$morts14dies, n=1), big.mark = ".", decimal.mark=","), 
                                                      paste0( ifelse(morts14[2]-morts14[1] >= 0, "+", ""), prettyNum(morts14[2]-morts14[1], big.mark = ".", decimal.mark=","), " Últims 14 dies"), icon = icon("file-medical"), color="red"),
                              shinydashboard::infoBox("Taxa mortalitat", round((sum(df_agr$exitus)/sum(df_agr$casos_confirmat))*100, 2) ,"%", icon = icon("laptop-medical"),  color="red"),
                              
                              shinydashboard::box( width = 12,
                                                   plotly::plotlyOutput("incidencia_morts", height = 300),
                                                   radioButtons("radbutMorts", "Selecciona:", c("Morts diàries" = "morts_diaries","Morts 14 dies" = "morts_14dies"), selected = "morts_diaries"))
                              
                              
                              
                              ), 
      ### Vacunació body ----
      shinydashboard::tabItem("vacunacio",
                              h2("Vacunació", style="margin:15px;"),
                              h5(tags$a(href= "https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-dosis-administrades-per-c/cuwj-bh3b", "Dades de vacunació del portal de Dades Obertes"),
                                 style="margin:15px;
                                 text-align: right;"),
                              shinydashboard::box( width = 12, 
                                                   selectInput("comarca_select", "Selecciona CATALUNYA o una comarca:", 
                                                               choices = unique(vacunes_agr2$comarca), 
                                                               selected = "CATALUNYA"),
                                                   plotlyOutput("vacunacio_sexe"),
                                                   selectInput("edat_select", "Selecciona els rangs d'edat:", 
                                                               choices = unique(vacunes_filtered$edat), multiple = TRUE, selected = c("20 a 24", "45 a 49", "50 a 54")),
                                                   plotlyOutput("vacunacio_edat"),
                                                   plotlyOutput("dosis", height = 500)
                              )
                              ),
      ### Sobre l'app body ----
      shinydashboard::tabItem("app",
                              h2("Sobre l'app", style="margin:15px;"),
                              h4(style="margin:15px;",
                                 tags$div(class="alert alert-info",
                                          tags$p("Aquesta web ha estat desenvolupada a partir de les dades proporcionades pel", tags$a(href= "https://analisi.transparenciacatalunya.cat/", "portal de Dades Obertes de la Generalitat de Catalunya.")))),
                              h3("Fonts de les dades", style="margin:15px;"),
                              h4(style="margin:15px;", 
                                 tags$div(class="alert alert-warning",
                                          tags$p("* Pestanya d'incidència:", tags$a(href="https://analisi.transparenciacatalunya.cat/Salut/Dades-di-ries-de-COVID-19-per-comarca/c7sd-zy9j", "Dades diàries de COVID-19 per comarca")),
                                          tags$p("* Pestanya de vacunació:", tags$a(href="https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-dosis-administrades-per-c/cuwj-bh3b", "Vacunació per al COVID-19: dosis administrades per comarca"))))
                              
      )
  )))

### UI ----
ui <- dashboardPage(skin = "black", header, sidebar, body)

### Server ----
server <- function(input, output, session) {
  
  ### Incidència server ----
  date_breaks <- "2 months"
  `%>%` <- magrittr::`%>%`
  
  
  output$casos <- renderPlotly({
    if (input$radbutCont == "casos_diaris") {
      
      in_casos <-df_agr%>%
        ggplot(aes(x=data, y= casos_confirmat, group = 1)) +
        geom_point(size=0.1, color="skyblue", aes(text=sprintf("%s<br>Casos: %s<br>", data, prettyNum(casos_confirmat, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Casos diaris")
      
    } else if (input$radbutCont == "casos_7dies"){
      
      in_casos <-df_agr%>%
        ggplot(aes(x=data, y= casos7dies, group = 1)) +
        geom_point(size=0.1, color="skyblue", aes(text=sprintf("%s<br>Casos: %s<br>", data, prettyNum(casos7dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Casos 7 dies")
    } else if (input$radbutCont == "casos_14dies"){
      
      in_casos <-df_agr%>%
        ggplot(aes(x=data, y= casos14dies, group = 1)) +
        geom_point(size=0.1, color="skyblue", aes(text=sprintf("%s<br>Casos: %s<br>", data, prettyNum(casos14dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Casos 14 dies")
    }
    in_cases <- in_casos+ 
      ggplot2::geom_area( fill="skyblue", alpha=0.4) +
      ggplot2::geom_line(color="skyblue", size=0.7) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº casos")
    
    
    ggplotly(in_cases, source = "select", tooltip ='text')
  })
  
  output$incidencia_hosp <- renderPlotly({
    if (input$radbutHosp == "ingressats_actual") {
      
      in_hosp <-df_agr%>%
        ggplot(aes(x=data, y= ingressats_total, group = 1)) +
        geom_point(size=0.1, color="seagreen4", aes(text=sprintf("%s<br>Hospitalitzats: %s<br>", data, prettyNum(ingressats_total, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Hospitalitzacions actuals")
    } else if (input$radbutHosp == "ingressats_7dies") {
      
      in_hosp <-df_agr%>%
        ggplot(aes(x=data, y= ingressats7dies, group = 1)) +
        geom_point(size=0.1, color="seagreen4", aes(text=sprintf("%s<br>Hospitalitzats: %s<br>", data, prettyNum(ingressats7dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Hospitalitzats 7 dies")
    } else if (input$radbutHosp == "ingressats_14dies") {
      
      in_hosp <-df_agr%>%
        ggplot(aes(x=data, y= ingressats14dies, group = 1)) +
        geom_point(size=0.1, color="seagreen4", aes(text=sprintf("%s<br>Hospitalitzats: %s<br>", data, prettyNum(ingressats14dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Hospitalitzats 14 dies")
    }
    
    in_hosp<- in_hosp+
      ggplot2::geom_area( fill="seagreen4", alpha=0.4) +
      ggplot2::geom_line(color="seagreen4", size=0.7) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº casos")
    
    
    ggplotly(in_hosp, source = "select", tooltip ='text')
  })
  
  output$incidencia_uci <- renderPlotly({
    if (input$radbutUci == "uci_actual") {
      
      in_uci <-df_agr%>%
        ggplot(aes(x=data, y= ingressats_critic, group = 1)) +
        geom_point(size=0.1, color="darkorange", aes(text=sprintf("%s<br>Pacients UCI: %s<br>", data, prettyNum(ingressats_critic, big.mark = ".", decimal.mark=",")))) +
        ggtitle("UCI diàries")
    } else if (input$radbutUci == "uci_7dies") {
      
      in_uci <-df_agr%>%
        ggplot(aes(x=data, y= critic7dies, group = 1)) +
        geom_point(size=0.1, color="darkorange", aes(text=sprintf("%s<br>Pacients UCI: %s<br>", data, prettyNum(critic7dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("UCI 7 dies")
    } else if (input$radbutUci == "uci_14dies") {
      
      in_uci <-df_agr%>%
        ggplot(aes(x=data, y= critic14dies, group = 1)) +
        geom_point(size=0.1, color="darkorange", aes(text=sprintf("%s<br>Pacients UCI: %s<br>", data, prettyNum(critic14dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("UCI 14 dies")
    }
    
    in_uci <- in_uci+
      ggplot2::geom_area( fill="darkorange", alpha=0.4) +
      ggplot2::geom_line(color="darkorange", size=0.7) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº casos")
    
    
    ggplotly(in_uci, source = "select", tooltip ='text')
  })
  
  output$incidencia_morts <- renderPlotly({
    if (input$radbutMorts == "morts_diaries") {
      
      in_morts <-df_agr%>%
        ggplot(aes(x=data, y= exitus, group = 1)) +
        geom_point(size=0.1, color="red", aes(text=sprintf("%s<br>Defuncions: %s<br>", data, prettyNum(exitus, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Defuncions diàries")
    } else if (input$radbutMorts == "morts_14dies") {
      
      in_morts <-df_agr%>%
        ggplot(aes(x=data, y= morts14dies, group = 1)) +
        geom_point(size=0.1, color="red", aes(text=sprintf("%s<br>Defuncions: %s<br>", data, prettyNum(morts14dies, big.mark = ".", decimal.mark=",")))) +
        ggtitle("Defuncions 14 dies")
    }
    
    in_morts <- in_morts+
      ggplot2::geom_area( fill="red", alpha=0.4) +
      ggplot2::geom_line(color="red", size=0.7) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº casos")
    
    
    ggplotly(in_morts, source = "select", tooltip ='text')
  })
  ### Vacunació server ----
  
  vacuna_comarca_filtered <- reactive({
    vacuna_comarca <- vacunes_agr%>% filter(comarca == input$comarca_select)
    return(vacuna_comarca)
  })
  
  output$vacunacio_sexe <- renderPlotly({
    
    vac_sexe <- vacuna_comarca_filtered()%>%
      ggplot(aes(x=data, y= recompte, group= sexe, color= sexe)) +
      ggplot2::geom_line( size=0.7, aes(text=sprintf("%s<br>Dosis: %s<br>", data, prettyNum(recompte, big.mark = ".", decimal.mark=",")))) +
      facet_grid(rows = vars(sexe))+
      geom_smooth(se=FALSE, aes(fill = sexe, text=sprintf("Mitjana mòbil")))+
      ggplot2::theme_light() +
      ggtitle("Vacunació diària per sexe")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº dosis administrades")+
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      theme(legend.position="none")
    
    ggplotly(vac_sexe, source = "select", tooltip ='text')
  })
  
  vacuna_edat_filtered <- reactive({
    vacuna_edat <- vacunes_agr2%>% filter(comarca == input$comarca_select)%>%
      filter(edat %in% input$edat_select)
    return(vacuna_edat)
  })
  
  output$vacunacio_edat <- renderPlotly({
    
    vac_edat <- vacuna_edat_filtered()%>%
      ggplot(aes(x=data, y= recompte, group= edat, color= edat)) +
      ggplot2::geom_line( size=0.7, aes(text=sprintf("%s<br>Dosis: %s<br>", data, prettyNum(recompte, big.mark = ".", decimal.mark=",")))) +
      facet_grid(rows = vars(edat))+
      geom_smooth(se=FALSE, aes(fill = edat, text=sprintf("Mitjana mòbil")))+
      ggplot2::theme_light() +
      ggtitle("Vacunació diària per edats")+
      ggplot2::xlab("Data")+
      ggplot2::ylab("Nº dosis administrades")+
      ggplot2::scale_x_datetime( date_breaks = date_breaks, date_labels =  "%b %Y")+
      theme(legend.position="none")
    
    ggplotly(vac_edat, source = "select", tooltip ='text')
    
  })
  
  vacuna_dosi_filtered <- reactive({
    vacuna_dosi <- vacunes_agr3%>% filter(comarca == input$comarca_select)
    return(vacuna_dosi)
  })
  
  output$dosis <- renderPlotly({
    vac_dosi <- vacuna_dosi_filtered()%>%
      ggplot(aes(fill=sexe, x=dosi, y=recompte))+
      geom_bar(position="dodge", stat="identity", aes(text=sprintf("%s<br>Dosis: %s<br>", sexe, prettyNum(recompte, big.mark = ".", decimal.mark=","))))+
      ggplot2::theme_light() +
      facet_wrap(~edat)+
      scale_x_discrete(labels=c("1" = "Dosi 1", "2" = "Dosi 2",
                                "3" = "Dosi 3"))+
      ggtitle("Vacunació per dosi i edat")+
      ggplot2::xlab("")+
      ggplot2::ylab("Nº dosis administrades")+
      theme(legend.position="none")
    
    ggplotly(vac_dosi, source = "select", tooltip ='text')
  })
  
}

shinyApp(ui = ui, server = server)

