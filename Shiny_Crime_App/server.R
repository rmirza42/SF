library(shiny)

shinyServer(function(input, output) {
  output$crimeDist <- renderPlot({
    cap <- labs(caption = paste0(
      "\nSource: https://www.kaggle.com/c/sf-crime/data (",
      min(str_c(SF$Year, "/", SF$month)),
      " to ",
      max(str_c(SF$Year, "/", SF$month)),
      ")\nData Science Powered by R"
    ))
    if(input$name=="NONE"){
    ggplot(SFsumm, aes(
      str_c(Year, "/", str_pad(month, 2, pad = 0)),
      Count,
      colour = Category,
      group = Category
    )) +
      geom_line() +
      facet_wrap( ~ PdDistrict, scales = "free_y", ncol = 5) +
      theme_economist() +
      scale_colour_economist(name = "Major Category") +
      scale_x_discrete(breaks = c("2003/01", "2005/11", "2008/10", "2011/08", "2014/02",
                                  "2017/07")) +
      ggtitle("San Francisco Crime by Police District\n") +
      labs(x = NULL, y = NULL, caption = cap) +
      theme(
        rect = element_rect(fill = "#f9f5f1"),
        plot.background = element_rect(fill = "#f9f5f1"),
        text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 0.4)
      )}
    else{
      SFsumm <- SFsumm %>% filter(PdDistrict==input$name)
      ggplot(SFsumm, aes(
        str_c(Year, "/", str_pad(month, 2, pad = 0)),
        Count,
        colour = Category,
        group = Category
      )) +
        geom_line() +
        # facet_wrap( ~ PdDistrict, scales = "free_y", ncol = 5) +
        theme_economist() +
        scale_colour_economist(name = "Major Category") +
        scale_x_discrete(breaks = c("2003/01", "2005/11", "2008/10", "2011/08", "2014/02",
                                    "2017/07")) +
        ggtitle("San Francisco Crime by Police District\n") +
        labs(x = NULL, y = NULL, caption = cap) +
        theme(
          rect = element_rect(fill = "#f9f5f1"),
          plot.background = element_rect(fill = "#f9f5f1"),
          text = element_text(size = 12),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.4)
    )}
  })
  
  output$heatmap <- renderLeaflet({
    
    SFdist <- SF %>% filter(PdDistrict==input$district)
      leaflet(SFdist) %>%
      setView(lng = -122.4194,lat = 37.7749,zoom = 12) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB.DarkMatter,group="Dark Matter") %>%
      addProviderTiles(providers$Thunderforest.TransportDark, group = "Transport Dark")%>%
      addProviderTiles(providers$Thunderforest.Transport, group ="Transport")%>%
      addProviderTiles(providers$OpenTopoMap, group ="Topo") %>% 
      addProviderTiles(providers$CartoDB.Positron, group="Positron") %>%
      addLayersControl(baseGroups = c("OSM (default)","Dark Matter","Transport Dark",
                                        "Transport","Topo","Positron"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
      addHeatmap(lng=~X, lat=~Y,
                   blur = input$blur, max = input$max, radius = input$radius )
  })
  
 
  
})



