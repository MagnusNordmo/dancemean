library(tidyverse)
library(shiny)

ui <- fluidPage(
  # CSS styling
  tags$head(
    tags$style(HTML("
      .shiny-options-group { 
        height: auto;
        width: 600px;
        -webkit-column-count: 2; 
        -moz-column-count: 2;    
        column-count: 2; 
        -webkit-column-fill: balance;
        -moz-column-fill: balance;
        column-fill: balance;
        margin-top: 0px;
      } 
      .shiny-plot-output { height: 170px !important; }
      .control-label { padding-bottom: 10px; }
      div.radio {
        margin-top: 0px;
        margin-bottom: 0px;
        padding-bottom: 5px;
      }
    "))
  ),
  
  # UI
  h4("Set The Population"),
  radioButtons(
    inputId = "espick",
    label = "Effect Size Examples",
    inline = FALSE,
    choices = c(
      "Zero effect d = 0"                                        = 10,
      "Average Educational Intervention d = 0.16 (Kraft 2020)"   = 9.68,
      "Antidepressant Medication d = .38 (Leuch et al. 2015)"    = 9.24,
      "Oxicodone + Aspirin Pain d = 1.04 (Moore et al. 2011)"    = 7.92,
      "Men vs Women Height d = 1.48 (Meier et al. 2011)"         = 7.19
    )
  ),
  sliderInput(inputId = "meandiff", step = .1, min = 5, max = 15, value = 10, label = "Mean Y"),
  plotOutput(outputId = "population"),
  
  # --- NEW: inline checkbox to toggle CI ---
  div(
    style = "display: inline-block;",
    actionButton(inputId = "sample", label = "Take a sample")
  ),
  div(
    style = "display: inline-block; margin-left: 12px;",
    checkboxInput(inputId = "show_ci", label = "Show 95% CI", value = FALSE)
  ),
  
  sliderInput(inputId = "sizesample", min = 2, max = 300, value = 5, label = ""),
  plotOutput(outputId = "sampleout")
)

server <- function(input, output, session) {
  
  # --- Population parameters ---
  sd_pop <- 2
  mu_x   <- 10
  
  # Update slider when a preset is chosen
  observeEvent(input$espick, {
    updateSliderInput(session, "meandiff", value = input$espick)
  })
  
  # Population plot with two PDFs
  output$population <- renderPlot(height = 150, {
    mu_y <- input$meandiff
    xgrid <- seq(0, 20, length.out = 400)
    densdf <- tibble(
      name = rep(c("x", "y"), each = length(xgrid)),
      x    = rep(xgrid, times = 2)
    ) |>
      mutate(dens = dnorm(x, mean = if_else(name == "x", mu_x, mu_y), sd = sd_pop))
    
    means <- tibble(name = c("x", "y"), mean = c(mu_x, mu_y))
    labels <- tibble(
      name  = c("x", "y"),
      xlab  = c(mu_x, mu_y),
      lab   = c(
        paste0("Mean X = ", round(mu_x, 2), "  sd = ", sd_pop),
        paste0("Mean Y = ", round(mu_y, 2), "  sd = ", sd_pop)
      )
    )
    
    ggplot(densdf, aes(x = x, y = dens, color = name)) +
      geom_line() +
      facet_wrap(~name, ncol = 1, scales = "free_y") +
      geom_vline(data = means, aes(xintercept = mean, color = name), linewidth = .3) +
      geom_text(
        data = labels,
        aes(x = xlab * .75, y = Inf, label = lab, color = name),
        vjust = 1.3, size = 3
      ) +
      scale_x_continuous(breaks = seq(0, 20, 1), limits = c(0, 20)) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  })
  
  # Sampling with optional 95% CI overlay
  observeEvent(input$sample, {
    output$sampleout <- renderPlot(height = 160, isolate({
      mu_y <- input$meandiff
      n_i  <- input$sizesample
      
      # Draw samples from the PDFs
      sampledf <- tibble(
        name  = c(rep("x", n_i), rep("y", n_i)),
        value = c(
          rnorm(n_i, mean = mu_x, sd = sd_pop),
          rnorm(n_i, mean = mu_y, sd = sd_pop)
        )
      )
      
      # Per-group stats and CIs
      stats <- sampledf |>
        group_by(name) |>
        summarise(
          n    = n(),
          mean = mean(value),
          sd   = sd(value),
          .groups = "drop"
        ) |>
        mutate(
          se    = sd / sqrt(n),
          tcrit = qt(0.975, df = n - 1),
          lower = mean - tcrit * se,
          upper = mean + tcrit * se
        )
      
      # t-test + d
      samplex <- sampledf |> filter(name == "x") |> pull(value)
      sampley <- sampledf |> filter(name == "y") |> pull(value)
      tlist   <- t.test(samplex, sampley)
      cohen   <- round((stats$mean[stats$name == "x"] - stats$mean[stats$name == "y"]) / 2, 2)
      
      # Base plot (means and points)
      p <- ggplot(sampledf, aes(x = value, y = "test", color = name)) +
        geom_point(alpha = .9) +
        facet_wrap(~name, ncol = 1) +
        geom_vline(data = stats, aes(xintercept = mean, color = name), linewidth = .3) +
        geom_text(
          data = stats |>
            transmute(name, xlab = mean, lab = paste0("Mean ", toupper(name), " = ", round(mean, 2))),
          aes(x = xlab * .75, y = .70, label = lab, color = name),
          size = 3
        ) +
        scale_x_continuous(breaks = seq(0, 20, 1), limits = c(0, 20)) +
        labs(
          caption = sprintf(
            "t test: p value = %s   t value = %s   cohens d = %s%s",
            ifelse(tlist$p.value < .001, "< .001", round(tlist$p.value, 3)),
            round(as.numeric(tlist$statistic), 2),
            cohen,
            ifelse(isTRUE(input$show_ci), "\n95% CI of means shown as horizontal bars", "")
          )
        ) +
        theme(
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(hjust = 0, size = 12),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()
        )
      
      # Conditionally add the CI bars/mean dots at y = 0.6
      if (isTRUE(input$show_ci)) {
        p <- p +
          geom_segment(
            data = stats,
            aes(x = lower, xend = upper, y = 0.6, yend = 0.6, color = name),
            linewidth = 1
          ) +
          geom_point(
            data = stats,
            aes(x = mean, y = 0.6, color = name),
            size = 2.5
          )
      }
      
      p
    }))
  })
}

shinyApp(ui, server)
