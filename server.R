
server <- function(input, output, session) {
  
  
  # Retrieves Data for Value Presentation
  getData <- reactive({
    if(as.character(input$homeType) == "All Type"){
      d <- data  # No filter needed
    }
    else{
      d <- filter(data, type == as.character(input$homeType))
    }
    
    validate(
      need(nrow(d)>0, "No markets meet your search criteria.")
    )
    
    # Remove outlier?
    if(input$removeOutlier){
      d <- d[d$pps < median(d$pps) + 20*sd(d$pps),]
      d <- d[d$lot_size < median(na.omit(d$lot_size)) + 10*sd(na.omit(d$lot_size))  | is.nan(d$lot_size),]

    }
    
    return(d)
  })
  
  output$homeTypeUi <- renderUI({
    homeTypes <- sort(unique(d$type))
    selectInput("homeType", label = "Home Type:", choices = c("All Type", as.character(homeTypes)), selected = "All Type", selectize = FALSE)
  })
  
  output$zipUi <- renderUI({
    zips <- sort(unique(d$zip))
    selectInput("zip", label = "Zip Code:", choices = c("All", as.character(zips)), selected = "All", selectize = FALSE)
  })
  
  output$driverUi <- renderUI({
    drivers <- c("bedrooms", "bathrooms", "type", "size_sqft", "lot_size", "distance")
    selectInput("driver", label = "Price Driver:", choices = drivers, selected = "size_sqft", selectize = FALSE)
  })
  
  output$comparerUi <- renderUI({
    comparers <- c("price", "pps", "bedrooms", "size_sqft", "lot_size")
    selectInput("comparer", label = "What to see:", choices = comparers, selected = "pps", selectize = FALSE)
  })
  
  output$attrUi <- renderUI({
    
    attrs <- c("bedrooms", "bathrooms", "type", "size_sqft", "lot_size", "distance")
    selectInput("attr", label = "Attribute:", choices = attrs, selected = "bedrooms", selectize = FALSE)
  })
  
  output$removeOutlierUi <- renderUI({
    
    checkboxInput("removeOutlier", label = "Remove Outliers?", TRUE)
  })
  
  output$total_obs <- renderValueBox({
    
    d <- getData()
    
    valueBox(
      nrow(d),
      "Number of Records", 
      icon = icon("calculator"), color = "green"
    )
  })
  
  output$median_price <- renderValueBox({
    
    d <- getData()
    
    valueBox(
      paste0("$", round(median(d$price),0)),
      "Median Price", 
      icon = icon("dollar"), color = "blue"
    )
  })
  
  output$median_pps <- renderValueBox({
    
    d <- getData()
    
    valueBox(
      paste0("$", round(median(d$pps),0)),
      "Median Price/Sqft", 
      icon = icon("dollar"), color = "purple"
    )
  })
  
  output$top10 <- renderPlot({
    
    d <- getData()
    
    top10 <- d %>% group_by(zip) %>% summarise(med = median(price)) %>% arrange(desc(med)) %>% top_n(10)
    ggplot(top10) + geom_bar(aes(reorder(zip, -med), med, fill = as.factor(zip)), stat = "identity") +
      labs(x = "Zip Code", y = "Median Price") + 
      scale_y_continuous(labels = scales::dollar, breaks = seq(0,1000000,100000)) + 
      theme(legend.position="none")
  })
  
  output$top10pps <- renderPlot({
    
    d <- getData()
    
    top10pps <- d %>% group_by(zip) %>% summarise(med = median(pps)) %>% arrange(desc(med)) %>% top_n(10)
    ggplot(top10pps) + geom_bar(aes(reorder(zip, -med), med, fill = as.factor(zip)), stat = "identity") +
      labs(x = "Zip Code", y = "Median Price Per Sqft") + 
      scale_y_continuous(labels = scales::dollar, breaks = seq(0,10000,100)) + 
      theme(legend.position="none")
  })
  
  output$home_type <- renderPlot({
    
    ggplot(data, aes(x = "", fill = type)) +
      geom_bar(width = 1) + 
      coord_polar(theta = "y") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  output$regression <- renderPlotly({
    
    d <- getData()
    
    # create new bathroom field
    d <- d %>% mutate(bathrooms = full_bathrooms + half_bathrooms * 0.5)
    
    m_driver <- as.character(input$driver)
    
    if(m_driver == "type"){
      ggplotly(ggplot(d, aes_string(x = m_driver, y = "pps", label = "address")) +
                 geom_boxplot())
      
    }
    else if(m_driver == "size_sqft" | m_driver == "lot_size" | m_driver == "distance"){
      ggplotly(ggplot(d, aes_string(x = m_driver, y = "pps", label = "address")) +
                 geom_jitter(size = 0.5) +
                 geom_smooth())
    }
    else{
      ggplotly(ggplot(d, aes_string(x = m_driver, y = "pps", label = "address")) +
                 geom_boxplot(size = 0.25) + 
                 xlim(min(d[m_driver]), max(d[m_driver])))
    }
  })
  
  output$geoplot <- renderPlot({
    
    d <- getData()
    d <- d %>% mutate(pps = scale(pps), price = scale(price), size_sqft = scale(size_sqft), lot_size = scale(lot_size))
    
    ggmap(lv_map) %+% d + aes_string(x = "lon", y = "lat", z = as.character(input$comparer)) + 
      stat_summary_2d(fun = median,  binwidth = c(.02, .02), alpha = 0.6) +
      scale_fill_gradient2(high = "blue") + 
      coord_map()
    
  })
  
  output$zipAnalyzer <- renderPlot({
    
    d <- getData()
    d <- d %>% mutate(bathrooms = full_bathrooms + half_bathrooms * 0.5)
    
    attri <- input$attr
    
    if(attri != "type"){
      
    ggplot(d, aes(x = reorder(zip,-pps))) + 
      geom_boxplot(aes_string(y = attri )) +
      labs(x = "Zip Code Sorted by Price Per Square Feet") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
    }
    else{
      
      ggplot(d, aes(x = reorder(zip, -pps))) + 
        geom_bar(aes(y = type, fill = type), stat = "identity") +
        labs(x = "Zip Code Sorted by Price Per Square Feet") +
        scale_y_discrete(breaks = seq(0,10000, 1000)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
    }
  })
  
}