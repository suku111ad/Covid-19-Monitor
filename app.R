#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(shinythemes)
library(MASS)
library(reshape2)
library(gganimate)
library(gifski)
library(GGally)
library(cdparcoord)
library(networkD3)
library(treemap)
library(remotes)
library(d3treeR)
library(rsconnect)

# read data from generated csv dataset 
get_data <- function(){
    all.cases <- read_csv("dataWrangling/covid19-dataset.csv")
    return(all.cases)
}

# get the most recent updated data
get_date <- function(df){
    df %>% 
        mutate(Date = as.Date(Date, format= "%m/%d/%y")) %>% 
        filter(Date == max(Date))
}


# Define UI for application that draws a histogram
ui <- navbarPage(title = "COVID-19 Monitor", theme = shinytheme("flatly"), 
                 tabPanel("Global Crisis Map", 
                          tabsetPanel( 
                              tabPanel("Confirmed Cases", style = "color: darkskyblue", 
                                       leafletOutput("confirmedMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 75, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("confirmedTable"), 
                                                     style = "opacity: 0.75"
                                       )
                              ), 
                              tabPanel("Death Cases", 
                                       leafletOutput("deathsMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 75, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("deathsTable"), 
                                                     style = "opacity: 0.75"
                                       )
                              ), 
                              tabPanel("Recovered Cases", 
                                       leafletOutput("recoveredMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 75, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("recoveredTable"), 
                                                     style = "opacity: 0.75"
                                       )
                              ),
                              tabPanel("Active Cases", 
                                       leafletOutput("activeMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 75, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("activeTable"), 
                                                     style = "opacity: 0.75"
                                       )
                              )
                          ), 
                          absolutePanel(
                              top = 175, right = 25, 
                              class = "panel panel-default", 
                              width = 150, fixed = TRUE,
                              draggable = TRUE, height = "auto", 
                              h3(textOutput("confirmedCount"), align = "right", style = "color: deepskyblue"), 
                              h5("confirmed cases", align = "right", style = "color: deepskyblue"), 
                              h4(textOutput("deathsCount"), align = "right", style = "color: red"), 
                              h6("deaths", align = "right", style = "color: red"), 
                              h4(textOutput("recoveredCount"), align = "right", style = "color: green"), 
                              h6("recovered", align = "right", style = "color: green"), 
                              h4(textOutput("activeCount"), align = "right", style = "color: orange"), 
                              h6("active cases", align = "right", style = "color: orange"), 
                              h5(textOutput("countryCount"), align = "right", style = "color: purple"), 
                              h6("countries affected", align = "right", style = "color: purple"), 
                              textOutput("hitsCount"), 
                              style = "opacity: 0.75"
                          )
                          ),
                 tabPanel("DataViz Panel", 
                          fluidRow(
                              column(width = 6, plotOutput("treeMap")), 
                              column(width = 6, plotOutput("barPlot"))
                          ),
                          fluidRow(
                              column(width = 6, plotOutput("parallelPlot")), 
                              column(width = 6, plotOutput("radarPlot"))
                          )
                          ), 
                 tabPanel("Time-Series Forecast", 
                          column(width = 6, plotOutput("mlGraph")), 
                          column(width = 6, plotOutput("smoothPlot"))
                 ), 
                 tabPanel("Animated Graphics", 
                          column(width = 6, imageOutput("lineGrowthPlot")), 
                          column(width = 6, imageOutput("stickPlot"))
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- get_data() 
    
    output$countryCount <- renderText(nrow(get_date(df)) - 2) 
    
    output$confirmedCount <- renderText(sum(get_date(df)$Confirmed)) 
    
    output$deathsCount <- renderText(sum(get_date(df)$Deaths)) 
    
    output$recoveredCount <- renderText(sum(get_date(df)$Recovered)) 
    
    output$activeCount <- renderText(sum(get_date(df)$Active))
    
    output$confirmedMap <- renderLeaflet(
        leaflet(get_date(df), options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>%  
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Confirmed*4, label = ~as.character(paste(Country, "-", 
                                                                               "Confirmed: ", Confirmed)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "deepskyblue") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$deathsMap <- renderLeaflet(
        leaflet(get_date(df), options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Deaths*30, label = ~as.character(paste(Country, "-", 
                                                                            "Deaths: ", Deaths)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "red") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$recoveredMap <- renderLeaflet(
        leaflet(get_date(df), options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Recovered*20, label = ~as.character(paste(Country, "-", 
                                                                               "Recovered: ", Recovered)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "green") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$activeMap <- renderLeaflet(
        leaflet(get_date(df), options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Active*4, label = ~as.character(paste(Country, "-", 
                                                                            "Active: ", Active)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "orange") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$confirmedTable <- DT::renderDataTable(
        datatable(
            get_date(df) %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Confirmed)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), background = styleColorBar(df$Confirmed, 'deepskyblue'),
                color = 'black', backgroundColor = NULL, fontWeight = 'bold' 
            )
    )
    
    output$deathsTable <- DT::renderDataTable(
        datatable(
            get_date(df) %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Deaths)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), background = styleColorBar(df$Deaths, 'red'),
                color = 'black', backgroundColor = NULL, fontWeight = 'bold' 
            )
    )
    
    output$recoveredTable <- DT::renderDataTable(
        datatable(
            get_date(df) %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Recovered)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), background = styleColorBar(df$Recovered, 'green'),
                color = 'black', backgroundColor = NULL, fontWeight = 'bold' 
            )
    )
    
    output$activeTable <- DT::renderDataTable(
        datatable(
            get_date(df) %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Active)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), background = styleColorBar(df$Active, 'orange'),
                color = 'black', backgroundColor = NULL, fontWeight = 'bold' 
            )
    )
    
    output$treeMap <- renderPlot(
        treemap(get_date(df) %>% top_n(20, Confirmed),
                index= c("Country"),
                vSize= c("Confirmed"),
                vColor = c("Confirmed"), 
                type="value",
                palette = "RdYlBu",
                mapping = c(5000,200000,3000),
                fontsize.title = 8,
                # bg.labels=c("white"),
                align.labels=list(
                    c("center", "center"), 
                    c("right", "bottom")
                )  
        )
    )
    
    output$parallelPlot <- renderPlot(
        ggparcoord(temp <- get_date(df) %>% top_n(15, Confirmed),
                   columns = 2:5, groupColumn = 1,
                   showPoints = TRUE,
                   title = "Parallel Coordinate Plot for Top 15 Countries",
                   alphaLines = 0.5,
                   mapping = ggplot2::aes(size = 1)) +
            ggplot2::scale_size_identity()
    )
    
    output$lineGrowthPlot <- renderImage({
        temp.df <- df %>% 
            select(c(3:6)) %>% 
            group_by(Date) %>% 
            summarize_all(sum) %>% 
            gather(Deaths, Recovered, Active, key = "Type", value = "Count") %>% 
            ungroup()
        p <- ggplot(temp.df) + 
            geom_line(aes(x = Date, y = Count, color = Type)) + 
            geom_point(aes(x = Date, y = Count, color = Type, shape = Type), size = 3) +  
            geom_text(aes(x = Date, y = Count, color = Type, label = Count), vjust = -3) + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
            transition_reveal(Date) 
        anim_save("sP.gif", animate(p, duration = 5, fps = 5, renderer = gifski_renderer())) 
        list(src = "sP.gif",
             contentType = 'image/gif')
    }, deleteFile = TRUE)
    
    output$stickPlot <- renderImage({
        q <- ggplot(df %>% filter(Date > "2020-03-01")) + 
            geom_line(aes(x=factor(Date), y=Confirmed, group = Date) , color = "blue") +
            ggtitle("Coronavirus confirmed cases trend") +
            ylab("confirmed cases") +  scale_x_discrete(name ="Date")  +  theme (axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))+
            transition_reveal(along = Date) 
        anim_save("sticks.gif", animate(q, duration = 5, fps = 5, renderer = gifski_renderer()))
        list(src = "sticks.gif",
             contentType = 'image/gif')
    }, deleteFile = TRUE)
    
    output$barPlot <- renderPlot({
        world_confirm <- get_date(df)
        
        world_confirm <- top_n(world_confirm, 10, world_confirm$Confirmed)
        
        ggplot(world_confirm)+
            aes(reorder (factor(Country), Confirmed), Confirmed)+
            geom_bar(stat = "identity", fill = "cyan", color = "white")+
            geom_text(aes(Country, Confirmed, label = Confirmed), size = 3.0, hjust = -0.3)+
            coord_flip()+
            xlab ("Country") 
    })
    
    output$radarPlot <- renderPlot({
        world_confirm <- get_date(df)
        
        world_confirm <- top_n(world_confirm, 10, world_confirm$Confirmed)
        
        df.m <- melt(world_confirm,
                     id.vars= c("Country"),
                     measure.vars= c("Active"),
                     variable.name= "Color",
                     value.name=    "Active"
        )
        
        # plot
        ggplot(data=df.m,  aes(x=Country, y=Active, group= Color, colour=Color, fill=Color)) +
            geom_point(size=2) +
            geom_polygon(size = 1, alpha= 0.2)  + ggtitle("Radar")  +
            scale_x_discrete() +
            theme_light()+
            coord_polar() + scale_color_manual(values="Orange")+
            scale_fill_manual(values="Yellow")
    })
    
    output$mlGraph <- renderPlot({
        model <- lm(Confirmed ~ ., data = df)
        par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
        plot(model,sub.caption="Diagnostic Plots")
    })
    
    output$smoothPlot <- renderPlot(
        ggplot(data = df) + 
            geom_smooth(mapping = aes(x = Date, y = Confirmed))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
