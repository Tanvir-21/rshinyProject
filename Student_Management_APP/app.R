
library(shiny)
library(shinydashboard)
library(highcharter)
library(viridis)
library(plyr)
library(dplyr)
library(skimr)
library(plotly)
library(dashboardthemes)

#cleaning done
country_options <- c("No Filter", "India", "Bangladesh")
system_options<-c("No Filter","Yes","No","not specified")


    #Dashboard header carrying the title of the dashboard
    header <- dashboardHeader(title = "Softanbees Boot Analysis", titleWidth = 300,
                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/company/softanbees-technologies", icon("linkedin"), "LinkedIn", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.facebook.com/softanbees/?__tn__=%2Cd%2CP-R&eid=ARBnFzcbpG49Uv0Rqy20b50iJTj6ygqexf40CLzG8kV7EJczfr1NIBS_cL4a8y1Zkpp6imeKz0K5UBg6" ,icon("facebook"), "Facebook Page", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://api.whatsapp.com/send?phone=919614323078", icon("whatsapp"), "Contact us", target="_blank"))
                    
                    
    )
    #Sidebar content of the dashboard
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
                     href = "https://softanbees.com"),
            selectInput("Country", 
                        label = "Country:",
                        choices = c(country_options), 
                        selected = "No Filter"),
    
            selectInput("System", 
                        label = "System:",
                        choices = c(system_options), 
                        selected = "No Filter"),
            tags$head(tags$style(HTML('.logo {
                              background-color: #000000 !important;
                              }
                              .navbar {
                              background-color: #000000 !important;
                              }
                              .sidebar{
                              background-color: #000000 !important;
                              }
                              '))),
            
            # The dynamically-generated user panel
            uiOutput("userpanel")
        )
    )
    
        # Boxes need to be put in a row (or column)
        frow1 <- fluidRow(
            infoBox(paste("Status"),"Analysis upto Boot 5",color="olive",width=3),
            infoBoxOutput("value1",width=3)
            ,infoBoxOutput("value2",width=3)
            ,infoBoxOutput("value3",width=3)
        )
        frow2 <- fluidRow( 
            
            box(
                title = "Field of Study"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,highchartOutput("field", height = "250px"),
                width=5
            ),
            
            box(
                title = "Collegewise Participation"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,highchartOutput("college", height = "250px"),
                width=7
            ) ,
            # box(
            #     title = "Countrywise Participation"
            #     ,status = "primary"
            #     ,solidHeader = TRUE 
            #     ,collapsible = TRUE 
            #     ,highchartOutput("country", height = "250px"),
            #     width=4
            # ) ,
            
            box(
                title = "Bootwise Participation"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,highchartOutput("boot", height = "250"),
                width=4
            ) ,
            box(
                title = "Ratio of students having System"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotlyOutput("system", height = "250px"),
                width = 4
            )
        )
        # combine the two fluid rows to make the body
        body <- dashboardBody(frow1, frow2,tags$style(
            type = 'text/css', 
            '.bg-aqua {background-color: #005CB9!important; }'
        )
        # ,shinyDashboardThemes(
        #     theme = "onenote"
        # )
        )
        
        #completing the ui part with dashboardPage
        ui <- dashboardPage(title = 'Softanbees Market Analysis', header, sidebar, body, skin='blue')
    


        # create the server functions for the dashboard  
        server <- function(input, output) {
            
            
            
            students1 <- read.csv("students.csv")
            students1[sapply(students1, is.character)] <- lapply(students1[sapply(students1, is.character)], as.factor)
            #skim(students1)
            
            
            rv = reactiveValues()
            rv$students1 =students1
            
            observe({
                rv$students1 = students1 %>% filter(if(input$Country == 'No Filter'){Country %in% country_options} else {Country == input$Country}) %>%filter(if(input$System == 'No Filter'){System %in% system_options} else {System == input$System})})
            #some data manipulation to derive the values of KPI boxes
            
            #creating the valueBoxOutput content
            output$value1 <- renderInfoBox({
                    infoBox(
                       "Students",nrow(students1),  icon = icon("user"),
                        color = "maroon",fill=TRUE
                    )
            })
            
            output$value2 <- renderInfoBox({
                infoBox(
                    "Colleges",nrow(data.frame(table(students1$College.Name))), icon = icon("university")
                    ,color = "yellow",fill=TRUE)
            })
            
            output$value3 <- renderInfoBox({
                infoBox(
                    "Major Fields",nrow(data.frame(table(students1$Field.of.study))), icon = icon("pencil")
                    ,color = "aqua",fill=TRUE)
            })
            #creating the plotOutput content
            
            
            
            output$country <- renderHighchart2({
                
                country<-data.frame(table(rv$students1$Country))
                colors <- revalue(country$Var1, c("India"="#0f4c75","Bangladesh"="#81b214"))
                highchart() %>% 
                    hc_chart(type = "pie") %>% 
                    hc_add_series_labels_values(labels = country$Var1, values = country$Freq,color = colors)%>%
                    hc_tooltip(formatter = JS("function(){
   return (this.point.name + ':<br/>' + 
           this.y + ' students<br/>' +
           Highcharts.numberFormat(this.percentage, 2) + '%<br />' )
           }"))%>%hc_plotOptions(
               series = list(
                   borderWidth = 3,
                   dataLabels = list(enabled = TRUE)
               )
           )
                
            })
            
            
            output$college <- renderHighchart2({
                
                coul <- viridis(17,option="A")
                cols <- substr(coul, 0, 17)
                thm <- hc_theme(
                    colors = cols,
                    chart = list(
                        backgroundColor = NULL
                    )
                )  
                rv$students1%>% group_by(College.Name)%>%dplyr::summarise(count = n()) %>%
                    hchart(.,'column',hcaes(x=College.Name, y=count, group=count,pointWidth=10))%>% hc_xAxis(title = list(text = "Colleges")) %>%
                    hc_yAxis(title = list(text = "College participation"))%>%hc_add_theme(thm)
            })
            
            
            
            output$system <- renderPlotly({
                cols<-c( "	#ff0000", 'rgb(128,133,133)','#006400')
                rv$students1%>% group_by(System)%>%dplyr::summarise(count = n())%>%
                    plot_ly(labels = ~System, values = ~count,marker = list(colors = cols))%>% add_pie(hole = 0.6)%>%layout   (showlegend = F, xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE))%>%layout(plot_bgcolor="transparent")%>%layout(paper_bgcolor="transparent") })
            
            
            
            
            output$field <- renderHighchart2({
                #field<-data.frame(table(students1$Field.of.study))
                #names(field)<-c("name","value")
                #field<-cbind(field,drilldown=tolower(field$name))
                
                coul <- viridis(5,option="D")
                cols <- substr(coul, 1,9)
                thm <- hc_theme(
                    colors = cols,
                    chart = list(
                        backgroundColor = NULL
                    )
                ) 
                s<-rv$students1%>% group_by(Field.of.study)%>%dplyr::summarise(count = n())%>%mutate(drilldown=tolower(Field.of.study))
                hc <- highchart()%>%
                    hc_chart(type = "pie") %>%
                    #hc_add_series_labels_values(labels = field$Var1, values = field$Freq,color = colors)%>%
                    hc_xAxis(type = "category") %>%
                    hc_legend(enabled = FALSE) %>%
                    hc_plotOptions(
                        series = list(
                            borderWidth = 2,
                            dataLabels = list(enabled = TRUE)
                        )
                    ) %>%
                hc_add_series(
                    data = s,
                    type = "pie",
                    hcaes(name = Field.of.study, y = count),
                    name = "Primary Fields",
                    colorByPoint = TRUE
                )%>%hc_tooltip(formatter = JS("function(){
   return (this.point.name + ':<br/>' + 
           this.y + ' students<br/>' +
           Highcharts.numberFormat(this.percentage, 2) + '%<br />' )
           }"))%>%hc_colors(cols)
                dfan <- data_frame(
                    Field.of.study = c("b.com","economics","electrician","arts","mca","b. pharmacy","library and information science","graduate","dental surgery","h.s passed","independent cyber security researcher","diploma on travel and tourism management","not specified"),
                    count= c(5,3,1,2,2,2,1,2,1,1,1,1,15),
                    percentage=c(5/37,3/37,1/37,2/37,2/37,2/37,1/37,2/37,1/37,1/37,1/7,1/37,15/37)
                )
                 hc %>%
                    hc_drilldown(
                        allowPointDrilldown = TRUE,
                        series = list(
                            list(
                                id = "others",
                                data = list_parse2(dfan)
                            )
                        )
                    )
            })
            
            output$boot <- renderHighchart2({
                coul1<- viridis(5)
                cols <- substr(coul1, 0, 7)
                
                boot<-rv$students1%>% group_by(boot.no)%>%dplyr::summarise(count = n())
                rv$students1%>% group_by(boot.no)%>%dplyr::summarise(count = n())%>%hchart(.,'column',hcaes(x=boot.no,y=count,group=boot.no,color=boot.no,pointWidth=10))%>% hc_xAxis(title = list(text = "Boot No.")) %>%
                    hc_yAxis(title = list(text = "Student Participation"))%>%hc_colors(cols)%>% hc_add_series(data=boot, type = "line", hcaes(boot.no,y=count),color="red") 
            })
            
        }

shinyApp(ui, server)
