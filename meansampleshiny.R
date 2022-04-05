
library(tidyverse)
library(shiny)
ui <- fluidPage(
  
  # CSS styling
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .shiny-options-group { 
  height: auto;
  width: 600px;
  -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
  -moz-column-count: 2;    /* Firefox */ 
  column-count: 2; 
  -webkit-column-fill: balance;
  -moz-column-fill: balance;
  column-fill: balance;
  margin-top: 0px;
      } 

.shiny-plot-output {
	height: 170px !important;
}

.control-label {
  padding-bottom: 10px;
}

div.radio {
  margin-top: 0px;
  margin-bottom: 0px;
  padding-bottom: 5px;
}"))),
  
  # UI
  h4("Set The Population"),
  
  radioButtons(inputId = "espick",
               label = "Effect Size Examples",
               inline = F,
               choices = c("Zero effect d = 0" = 10,
                           "Average Educational Intervention
                           d = 0.16 (Kraft 2020)" = 9.68,
                           "Antidepressant Medication
                           d = .38 (Leuch et al. 2015)" = 9.24,
                           "Oxicodone + Aspirin Pain d = 1.04
                          (Moore et al. 2011)" = 7.92,
                           "Men vs Women Heigh d = 1.48
                          (Meier et al. 2011)" = 7.19)),
  
  sliderInput(inputId = "meandiff",step = .1, min = 5,max = 15,value = 10,label = "Mean Y"),
  
  plotOutput(outputId = "population"),
  
  actionButton(inputId = "sample",label = "Take a sample"),
  
  sliderInput(inputId = "sizesample",min = 2,max = 300,value = 5,label = ""),
  
  plotOutput(outputId = "sampleout")
)

server <- function(input, output, session) {
  
  # Precice  rnorm (Thanks Ben Bolker on Stack Overflow)
  rnorm2 <- function(n,mean = 10,sd = 2){
    x <- mean+sd*scale(rnorm(n))
    as.numeric(x)
  }
  
  # Population Parameters
  seed <- 10
  
  n <- 1000
  
  sd <- 2
  
  
  # Make reactive population values
  df <- reactive({
    tibble(
      x = rnorm2(n = n,mean = 10,sd = sd) %>% round(2),
      y = rnorm2(n = n,mean = input$meandiff,sd = sd) %>% round(2)
    ) %>% 
      pivot_longer(c(x,y)) %>% 
      filter(value >= 0)
    })
  
  # Update radio buttons
  observeEvent(input$espick,{
    x <- input$espick
    updateSliderInput(session, "meandiff",
                      value = x)
  })
  
  
  # Population plot
  output$population <- renderPlot(height = 150,{
    
    dfint = df()
    
    means = dfint %>% 
        group_by(name) %>% 
        summarise(mean = mean(value) %>% round(2)) %>% 
        pivot_wider(values_from = mean,names_from = name)
    
    dat = tibble(
        label = c(paste0("Mean X = ",means$x," sd = ",sd),
                  paste0("Mean Y = ",means$y," sd = ",sd)),
        name   = c("x", "y"),
        x = c(means$x,means$y)
      )
  
    ggplot(dfint,aes(x = value,y = "test",color = name)) + 
    geom_point(alpha = .2) +
    scale_x_continuous(breaks = seq(0,20,1),limits = c(0,20)) +
    facet_wrap(~name,ncol = 1) + 
    geom_text(data = dat,aes(x = x*.75,y = .70,label = label)) + 
    geom_vline(aes(xintercept = x,color = name),
               data = dat,
               linetype = 1,
               size = .3) + 
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())

  })
  
  # Sample
  observeEvent(input$sample,{
    output$sampleout = renderPlot(height = 160,isolate({
      
      dfint = df()
      sampledf = dfint
      sampledf = sampledf %>% 
        group_by(name) %>% 
        slice_sample(n = input$sizesample) %>% 
        ungroup()
      
      means = sampledf %>% 
        group_by(name) %>% 
        summarise(mean = mean(value) %>% round(2)) %>% 
        pivot_wider(values_from = mean,names_from = name)
      
      dat = tibble(
        label = c(paste0("Mean X = ",means$x),
                  paste0("Mean Y = ",means$y)),
        name   = c("x", "y"),
        x = c(means$x,means$y)
      )
      
      # t.test
      samplex <- sampledf[sampledf$name == "x",]$value
      sampley <- sampledf[sampledf$name == "y",]$value
      tlist <- t.test(samplex,sampley)
      
      # Cohens t sample
      cohen <- (means$x - means$y)/2
      cohen <- round(cohen,2)
      
      # Plot
      ggplot(sampledf,aes(x = value,y = "test",color = name)) + 
        geom_point(alpha = .9) +
        scale_x_continuous(breaks = seq(0,20,1),limits = c(0,20)) +
        facet_wrap(~name,ncol = 1) +
        geom_text(data = dat,aes(x = x*.75,y = .70,label = label)) + 
        geom_vline(aes(xintercept = x,color = name),
                   data = dat,
                   linetype = 1,
                   size = .3) + 
        labs(caption = sprintf("t test: p value = %s  t value = %s  cohens d = %s",
                              ifelse(tlist$p.value < .001,"< .001",round(tlist$p.value,3)),
                              round(tlist$statistic,2),cohen)) +
        theme(axis.line.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank(),
              legend.position="none",
              plot.caption = element_text(hjust = 0,size = 20),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
      
    }))
  })
}

shinyApp(ui, server)
