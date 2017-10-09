library(DT)
library(shiny)
library(shinythemes)
library(d3heatmap)
library(leaflet.extras)
library(leaflet)
library(dplyr)
library(plotly)
library(shinyBS)
library(shinyjs)
library(quantmod)

shinyServer(function(input, output, session){
  
  # Output map of Tokyo
  output$scatterbox <- renderPlotly({
    
    # map style buttons
    basic <- list(method = "relayout",
                  args = list(list(mapbox.style = "mapbox://styles/madshelt/cj8camvm40bzu2tnxqwjflb4n")),
                  label = "Basic")
    
    dark <- list(method = "relayout",
                 args = list(list(mapbox.style = "dark")),
                 label = "Dark")
    
    satellite <- list(method = "relayout",
                      args = list(list(mapbox.style = "satellite")),
                      label = "Satellite")     
    
   # marker = list(colors = 'paired', opacity = 0.5, sizemode = 'diameter')) 
  
    tokyo %>%
      filter(., popularity == c(input$social)) %>%
      filter(., sex ==c(input$gender)) %>%
      filter(., weekday==c(input$weekd)) %>%
      plot_mapbox(lat = ~latitude, lon = ~longitude, split = ~timeofday,
                  mode = "scattermapbox", showlegend = TRUE,
                  marker = list(colors = 'paired', opacity = 0.5,
                                line = list(width = 2, color = '#FFFFFF')),
                  text = ~paste('Venue', venuecategory, '<br>Sex:', sex, '<br>Popularity:', popularity)) %>%
      layout(mapbox = list(style = 'mapbox://styles/madshelt/cj8camvm40bzu2tnxqwjflb4n', 
                           zoom = 10,
                           center = list(lat = ~median(latitude),
                                         lon = ~median(longitude)))) %>%
      layout(legend = list(orientation = 'h' , 
                           x = 0.55, y = 0.05,
                           font = list(size = 12),
                           bgcolor = 'rgba(250, 250, 250, 0.9)')) %>%
      layout(margin = list(l = 0, r = 0)) %>%
      layout(updatemenus = list(
        list(type='buttons',
             direction = "right",
             yanchor = "top",
             x = 0.6,
             y = 0.99,
             buttons=list(basic,dark,satellite)))) %>%
      add_annotations(x = 0.04, y = 0.05,
                      text = "Twitter check-ins in Tokyo (scroll to zoom)",
                      xref = "page",
                      yref = "page",
                      showarrow = FALSE,
                      font = list(color = 'magenta',
                                  size = 12))
    
    # worldmap: mapbox://styles/madshelt/cj8camvm40bzu2tnxqwjflb4n
    # dark map: mapbox://styles/mapbox/dark-v9
  })
  
  # Reactive stat information to plot
  countdf1 <- reactive({
    tokyo %>%
      group_by(., popularity, sex, weekday) %>%
      summarise(., check_in = n()) %>%
      filter(., popularity == c(input$social)) %>%
      filter(., sex ==c(input$gender)) %>%
      filter(., weekday==c(input$weekd))
      
  })
  
  countdf2 <- reactive({
    tokyo %>%
      filter(., popularity == c(input$social)) %>%
      filter(., sex ==c(input$gender)) %>%
      filter(., weekday==c(input$weekd))  %>%
    group_by(., venuecategory) %>%
      summarise(., checkin = n()) %>%
      arrange(., desc(checkin)) %>% top_n(5) 
    
  })
  
  # Bar chart plot 
  output$checkin <- renderPlotly({
    plot_ly(data = countdf1(), x = ~popularity, y = ~check_in, type = 'bar', alpha = 0.8,
            name = ~sex, 
            color = ~sex,
            marker = list(color = 'rgba(0, 132, 180, .7)', line = list(color = 'rgba(200, 200, 200, 0.7)', width = 1))) %>% 
      layout(barmode = 'stack',
             xaxis = list(title = "", color  = "rgb(255, 255, 255)", size = 8),
             yaxis = list(title = "", color = "rgb(255, 255, 255)", size = 8),
             legend = list(font = list(size = 8, color = "#FFFFFF")),
             title = "# check-ins <br> by popularity",
             titlefont = list(color = 'white'),
             margin = list(l = 35, r = 10, t = 100, b = 25)) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(legend = list(orientation = 'h', x = 0.0, y = 100))
    
  })
  
  output$top5 <- renderPlotly({
    plot_ly(data = countdf2(), x = ~venuecategory, y = ~checkin, type = 'bar', alpha = 0.8,
            name = ~venuecategory, 
            marker = list(color = 'rgba(0, 132, 180, .7)', line = list(color = 'rgba(200, 200, 200, 0.7)', width = 1))) %>%
      layout(
        xaxis = list(title = "", color  = "rgb(255, 255, 255)", size = 8),
        yaxis = list(title = "", color = "rgb(255, 255, 255)", size = 8),
        legend = list(font = list(size = 8, color = "#FFFFFF")),
        title = "Top 5 venues",
        titlefont = list(color = 'white'),
        margin = list(l = 35, r = 10, t = 100, b = 50)) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(legend = list(orientation = 'h', x = 0.0, y = 100))
  })
  
  # reset user inputs
  observeEvent(input$reset_input, {
    shinyjs::reset("panel")
  })
  
  ### Tab 2 - EDA ###    

  output$oneplot <- renderPlotly({
    # First graph 
    plot_ly(top30, x = top30$count, type = 'bar', orientation = 'h', 
            y = reorder(top30$venuecategory, top30$count),
            name = top30$timeofday, color = top30$timeofday,
            hoverinfo = "text",
            text = ~paste(count, ' check-ins'),
                         # marker = list(color = seq(0,39)),
                          colorscale = 'Viridis', reversescale = T) %>%
      layout(title = 'Top 30 venues based on # check-ins',
             yaxis = list(title = ''), barmode = 'stack',
             margin = list(l = 160)) 
  
  })
  
  output$twoplot <- renderPlotly({
    plot_ly(filtered, x = filtered$count, type = 'bar', orientation = 'h', 
            y = reorder(filtered$venuecategory, filtered$count),
            name = filtered$timeofday, color = filtered$timeofday,
            hoverinfo = "text",
            text = ~paste(count, ' check-ins')) %>%
      layout(title = 'Top venues without top 2',
             yaxis = list(title = ''), barmode = 'stack',
             margin = list(l = 160)) 
  })
  
  output$threeplot <- renderPlotly({
    plot_ly(tkdf) %>%
      add_trace(., x = ~timeseries, y = ~check_in, name = 'Month', type = 'bar', 
                marker = list(color = 'rgba(58, 71, 80, 0.6)',
                              line = list(color = 'rgba(58, 71, 80, 1.0)', width = 3)),  
                hoverinfo = "text",
                text = ~paste(check_in, ' check-ins')) %>%
      add_trace(x = ~timeseries, y = ~check_in, type = 'scatter', textposition = 'auto',
                mode = 'lines', name = '% change',
                line = list(color = '#F5A9A9',
                            width = 3, shape = "spline"),
                hoverinfo = "text") %>%
      layout(title = 'Number of venue check-ins in Tokyo by month',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE)) %>%
      layout(annotations = list(may, june, july, aug, sep, oct, nov, dec, jan, feb))
  })
  
  output$fourplot <- renderPlotly({
    plot_ly(tkdf2, x = ~timeseries, y = ~check_in, type = 'scatter', mode = 'lines', color = ~sex, line = list(width = 5, shape = "spline"),
            hoverinfo = "text", text = ~paste(check_in, ' check-ins')) %>%
      layout(title = 'Number of venue check-ins in by gender',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE))
  })
  
  output$fiveplot <- renderPlotly({
    plot_ly(tkdf3, x = ~timeseries, y = ~check_in, type = 'scatter', mode = 'lines', color = ~popularity, line = list(width = 5, shape = "spline"),
            hoverinfo = "text", text = ~paste(check_in, ' check-ins')) %>%
      layout(title = 'Number of venue check-ins by popularity',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE)) %>%
      layout(annotations = list(a, b, c))
  })
  
  ### Tab 3 - More ###  
  output$table <- DT::renderDataTable({
    datatable(tokyo, rownames=FALSE)
    #%>% formatStyle(., background="skyblue", fontWeight='bold')
  })
})

