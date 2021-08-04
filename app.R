library(shiny)
library(shinydashboard)
library(ggplot2)
library(mapdata)
library(intrval)
library(DT)
library(ggthemes)
library(maps)
library(shinyWidgets)
library(scales)
library(tmap)
library(tmaptools)
library(lubridate)
library(forecast)
css <- "
.irs-from, .irs-to {font-size: 12px;color: black; background: transparent }
"
data<-read.table("g.txt",header=TRUE,sep="\t",dec=",")
datah<-read.table("h.txt",header=TRUE,sep="\t",dec=",")
data<- data.frame(data)
BJsales<-BJsales
dbHeader <- dashboardHeader(title ="FLOA Dashboard",htmlOutput("image"))
dbHeader$children[[2]]$children[[2]] <- dbHeader$children[[2]]$children[[1]]

dbHeader$children[[2]]$children[[1]] <- tags$a(href='http://www.mbie.govt.nz',
                                             
                                             tags$img(src='MBIELogo/logo_reserve_small_corp1.png'),
                                             
                                             target = '_blank') #,height='67',width='228.6', align = 'left'
ui <- dashboardPage(
    dbHeader,
   
    dashboardSidebar(fluidPage(sidebarLayout(sidebarPanel(),
                                             
                                             mainPanel(htmlOutput("picture")))),
                     
                     sidebarMenu(id='sidebar',
                                 menuItem("Indicateurs", tabName = "Indicateurs", icon = icon("dashboard")),
                                 menuItem("Indicateurs hebdomadaires", tabName = "Indicateurshebdomadaires", icon = icon("database")),
                                 menuItem("Cartographie ", tabName = "Cartographie",icon = icon("calendar")),
                                 menuItem("Reception et traitement", tabName = "Reception", icon = icon("cog"),
                                          menuSubItem("Tunis",
                                                      tabName = "Tunis"),
                                          menuSubItem("Maroc",
                                                      tabName = "Maroc")),
                                 
                                 uiOutput('style_tag')
                     )),
    dashboardBody(
        tags$head(
            tags$style(
                HTML(
                    '.skin-black .main-sidebar  {color: #332288; background-color: #332288;}
          .skin-black .span12 { background-color: #332288;}
          .skin-black .main-header .navbar  { background-color: #332288;}
          .skin-black .main-header > .logo { color: #33BBEE;background-color: #332288;}
          .skin-black .main-header > .logo:hover { background-color: #332288;}
          .skin-black .main-header .logo, .skin-black .main-header .navbar { transition: color 0s; }
          .skin-white .main-Body {color: #332288; background-color: #ffffff;}'
                )
            ),
            tags$style(HTML('
      .main-header .logo {color: white;
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 20px;
      }
      ')),
            tags$style(type="text/css",
                       ".test_type {color: Black;
                 font-size:50%;
                 font-style: normal;}"
            )),
        tabItems(
            tabItem("Indicateurs",
                    fluidPage(
                        infoBoxOutput("progressBox"),
                        infoBoxOutput("approvalBox",width = 5)),
                    fluidPage(
                        box(plotOutput("Indicateurstable",height=300),width = 10),
                        box(selectInput("Activite","Activite:",
                                        c("B2C","B2B")),width = 2,height=0),
                        div(class="test_type",
                            sliderInput("slider",label=h5("Choisissez la periode"),min=as.Date("2020-01-01","%Y-%d-%m"),max=as.Date("2020-08-09","%Y-%d-%m"),c(as.Date("2020-01-01"), as.Date('2020-08-09')))),
                        tags$style(type = "text/css", HTML(css)),
                        dataTableOutput(outputId="dataTable"),
                        dataTableOutput(outputId="dataTablej")
                                    )),
            tabItem("Indicateurshebdomadaires",
                    fluidPage(
                        infoBoxOutput("progressiBox"),
                        infoBoxOutput("approvaliBox")),
                    fluidPage(
                        box(tiltle="Graphique",solidHeader=TRUE,status="primary",background="maroon",
                            plotOutput("Indicateurshebdomadairestable",height=300),width = 10),
                        box(selectInput("Activiti","Activiti:",
                                        c("B2C","B2B")),width = 2,height=0),
                        div(class="test_type",
                            sliderInput("slideri",label=h5("Choisissez la pÃ©riode"),min=1,max=52,value=c(1,52))),
                        tags$style(type = "text/css", HTML(css)),
                        dataTableOutput(outputId="dataTablei")
                        
                        )
            ),
            tabItem("Cartographie",
                    fluidPage(
                        h1("yes"),
                        box(plotOutput("Cartographietable",height=300),width =8)
                    )
            ),
            tabItem("Reception",
                    fluidPage(
                        box(plotOutput("Receptiontable",height=300),width =10), 
                        box(
                            selectInput("Activites","Activites:",
                                        c("appelB2C","appelB2B","appelcasino","appelcdiscount","appellignedirecte")),width = 2,height=0),
                        box( chooseSliderSkin("Modern", color = "blue"),
                             sliderInput("slidert",label=h5("Check me !",height=0),min=1,max=252,value=c(1,252))),
                        tags$style(HTML("
                    #renderprint {
                                color: white;
                                background: green;
                                font-family: 'Times New Roman', Times, serif;
                                font-size: 15px;
                                }
                )")),
                        tags$style(HTML("
                    #renderprinti {
                                color: white;
                                background: red;
                                font-family: 'Times New Roman', Times, serif;
                                font-size: 15px;
                                }
                )")),
                        box(title = "statistiques",verbatimTextOutput("renderprint"),width =8),
                        box(verbatimTextOutput("renderprinti"),width =8)
                    )
            ),
            tabItem("Tunis",
                    fluidPage(
                        box(plotOutput("Tunisable",height=300),width =10)
                    )),
            tabItem("Maroc",
                    fluidPage(
                        box(plotOutput("Maroctable",height=300),width =10)
                    ))
        )),
    skin = "black"
)
server <- function(input, output,session) {
    output$picture <-
        renderText({
            c(
                '<img src="',
                "https://drive.google.com/file/d/1P9pCiSL5S9ZucjGKIKohfT_jxrMtbhhw/view?usp=sharing",
                '">'
            )
        })
    output$image <-
        renderText({
            c(
                '<img src="',
                "https://drive.google.com/file/d/1odBULQyH3zKXv1APSVrBeNzU15InmmwT/view?usp=sharing",
                '">'
            )
        })
    output$style_tag <- renderUI({
        if(input$sidebar=='Indicateurs')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
        if(input$sidebar=='Indicateurs hebdomadaires')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
        
        if(input$sidebar=='Cartographie')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
        if(input$sidebar=='Reception')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
        if(input$sidebar=='Tunis')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
        if(input$sidebar=='Maroc')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))))
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "Appels recus depuis le 1er Janvier",sum(data$appelB2C)+sum(data$appelB2B), icon = icon("chart-line"),
            color = "teal"
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Appels traites depuis le 1er Janvier",sum(data$appelrecu), icon = icon("chart-line"),
            color = "teal"
        )
    })
    output$Indicateurstable<- renderPlot({
        ggplot(data)+geom_line(aes(x=date,y=data[[input$Activite]],fill=Indicateurs,colour=Indicateurs),size=1,stat="identity",position=position_dodge())+scale_y_continuous(labels=percent)+scale_color_manual(values=c("#0b53C1", "#ff0055"))+
            ggtitle("Visualisation QP VS QS")+theme(plot.title = element_text(color="blue",size=16,face="bold",hjust = 0.5),
                                                    axis.title.x = element_text(color="blue", size=12, face="bold"),
                                                    axis.title.y = element_text(color="blue", size=12, face="bold"),
                                                    legend.title = element_text(colour="blue", size=12,face="bold"),
                                                    legend.text = element_text(colour="black", size=12,face="bold"))
        
        
    })
    output$Daterange <- renderPrint({input$Daterange})
    output$dataTable <- renderDataTable({data
    })
    output$dataTable <- renderDataTable({BJsales
    })
    output$progressiBox <- renderInfoBox({
        infoBox(
            "Appels reÃ§us depuis le 1er Janvier",sum(data$appelB2C)+sum(data$appelB2B), icon = icon("chart-line"),
            color = "yellow"
        )
    })
    output$approvaliBox <- renderInfoBox({
        infoBox(
            "Appels traitÃ©s depuis le 1er Janvier",sum(data$appelrecu), icon = icon("chart-line"),
            color = "aqua"
        )
    })
    output$Indicateurshebdomadairestable<- renderPlot({
        slideri <- seq(input$slideri[1],input$slideri[2])
        datah_new <- datah[which(datah$date %in% slideri), ]
        ggplot(datah_new,aes(x=date,y=datah_new[[input$Activiti]]))+geom_line(aes(fill=Indicateurs,colour=Indicateurs),size=1,stat="identity",position=position_dodge())+scale_y_continuous(labels=percent)+scale_color_manual(values=c("#0b53C1", "#ff0055"))+
            ggtitle("Visualisation QP VS QS")+theme(
                plot.title = element_text(color="blue",size=16,face="bold",hjust = 0.5),
                axis.title.x = element_text(color="blue", size=12, face="bold"),
                axis.title.y = element_text(color="blue", size=12, face="bold"),
                legend.title = element_text(colour="blue", size=12,face="bold"),
                legend.text = element_text(colour="black", size=12,face="bold")
            )
    })
    output$rangei <- renderPrint({input$slideri})
    output$dataTablei <- renderDataTable({datah
    })
    output$Cartographietable <- renderPlot({
        tm_shape(World)
    })
    
      
    
}
if (interactive())shinyApp(ui, server)
