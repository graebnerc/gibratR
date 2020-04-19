library(shiny)
library(tidyverse)
library(magrittr)
library(ggpubr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  withMathJax(),
  # Application title
  titlePanel("Das Gibrat Modell"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h3("Parameterauswahl"),
      sliderInput("m", 
                  label='Anzahl der Individuen \\( m \\)',
                  min = 200, max = 4000, step=200, value = 600),
      sliderInput("w_val", 
                  label='Anfangsausstattung \\( w_{0} \\)',
                  min = 1, max = 50, step=1, value = 10),
      sliderInput("n", 
                  label='Anzahl der Wiederholungen \\( n \\)',
                  min = 100, max = 2000, step=100, value = 400),
      sliderInput("log_log_limits", 
                  label='Grenzen für log-log Plot',
                  min = 0, max = 1.0, step=0.01, value = c(0.0, 1.0)),
      sliderInput("g", 
                  label='Wohlstandswachstum \\( g \\)',
                  min = 0.0, max = 0.9, step=0.05, value = 0.0),
      selectInput("dist_kind", 
                  label = "Verteilung des Investitutionsspiels",
                  choices = c("Normalverteilung", 
                              "Uniformverteilung",
                              "Exponentialverteilung"),
                  selected = "Normalverteilung"),
      conditionalPanel(
        condition = "input.dist_kind == 'Normalverteilung'",
        sliderInput("norm_mean", label = 'Parameter \\( \\mu \\)', 
                    min = -5, max = 5, step=0.5, value = 0),
        sliderInput("norm_sd", label = 'Parameter \\( \\sigma \\)', 
                    min = 0.1, max = 5, step=0.1, value = 1)
      ),
      conditionalPanel(
        condition = "input.dist_kind == 'Uniformverteilung'",
        sliderInput("unif_min", label = 'Parameter \\( min \\)',
                    min = -5, max = 5, step=0.5, value = 0),
        sliderInput("unif_max", label = 'Parameter \\( max \\)',
                    min = -5, max = 5, step=0.5, value = 1)
      ),
      conditionalPanel(
        condition = "input.dist_kind == 'Exponentialverteilung'",
        sliderInput("exp_rate", label = 'Parameter \\( \\lambda \\)',
                    min = 0.1, max = 5, step=0.1, value = 1)
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      h3("Ergebnisse"),
      plotOutput("distribution_plot"),
      downloadButton("downloadPlot", "Download der Abbildung im PDF Format"),
      h3("Funktion des Modells"),
      p("Alle Individuen starten mit der gleichen Anfangsausstattung \\( w_{0} \\).
        In \\( n \\) Zeitschritten wird deren Vermögen dann durch den folgenden 
        Mechanismus geupdatet:"),
      uiOutput('f1'),
      p("wobei \\( g \\) eine allgemeine Wachstumsrate darstellt und \\( r \\) 
        aus einer der drei Verteilungen mit beliebig zu bestimmenden
        Parametern gezogen wird:"), 
      uiOutput('f2'),
      p("Die entsprechende Verteilung ist oben in 
        Panel A) illustriert. Panel B) und C) zeigen die Verteilung von 
        \\( w_{0} \\) und \\( w_{n} \\). Panel D) zeigt die Verteilung von 
        \\( w_{n} \\) in einem Log-Log Plot (x- und y-Achse logarithmiert).
        Dabei können Sie über den Slider links auswählen welche Bereiche der
        Wohlstandsverteilung betrachtet werden sollen. Ein Intervall (0.9, 1.0)
        würde z.B. bedeuten, dass nur die größten 10% der Vermögenswerte
        abgebildet werden."),
      h3("Leitfragen"),
      p("1. Welchen Einfluss haben die unterschiedlichen Verteilungen von \\( r \\)
        auf die finale Verteilung?"),
      p("2. Welchen Einfluss hat die Anzahl der Agenten \\( m \\) und die 
        Anfangsausstattung \\( w_{0} \\) auf die finale Verteilung?"),
      p("3. Welchen Einfluss hat die allgemeine Wachstumsrate \\( g \\) auf die 
        finale Verteilung?"),
      p("4. Für welche Bereiche der finalen Wohlstandsverteilung findet man am 
        ehesten Evidenz für eine Potenzverteilung?"),
      p("5. Gibt es Parameterkonstellationen, die eine Ungleichverteilung am
        Ende der Simulation verhindern?")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$f1 <- renderUI({
    withMathJax(
      helpText('$$w_{t+1} = (1+g+r)\\cdot w_t $$'))
  })
  output$f2 <- renderUI({
    withMathJax(
      helpText('$$r\\propto \\mathcal{N}(\\mu, \\sigma),$$ $$r\\propto \\mathcal{U}(min, max),$$ oder $$r\\propto \\mathcal{E}(\\lambda)$$'))
  })
  
  simulation_data <- reactive({
    m <- input$m
    w_val <- input$w_val
    n <- input$n
    g <- input$g
    
    w <- rep(w_val, m)
    initial_w <- w
    
    if (input$dist_kind=="Normalverteilung"){
      norm_mean <- input$norm_mean
      norm_sd <- input$norm_sd
      for (i in 1:n){
        w <- w*(1+g+rnorm(m, norm_mean, norm_sd)/100)
      }
      
      dist_data <- tibble(x_vals=seq(-15, 15, 0.01), 
                          y_vals=dnorm(x_vals, 
                                       mean = norm_mean, 
                                       sd = norm_sd))
      
    } else if (input$dist_kind=="Uniformverteilung"){
      
      for (i in 1:n){
        w <- w*(1+g+runif(m, input$unif_min, input$unif_max)/100)
      }
      
      dist_data <- tibble(x_vals=seq(input$unif_min-0.5, 
                                     input$unif_max+0.5, 0.1), 
                          y_vals=dunif(x_vals, input$unif_min, input$unif_max))
      
    } else if (input$dist_kind=="Exponentialverteilung"){
      for (i in 1:n){
        w <- w*(1+g+rexp(m, input$exp_rate)/100)
      }
      
      dist_data <- tibble(x_vals=seq(0, 5, 0.1), 
                          y_vals=dexp(x_vals,rate = input$exp_rate) )
    } 
    
    final_w = sort(w)
    
    result_list <- list(
      "plot_data" = tibble(w_init=initial_w, final_w=final_w, Individuum=1:m),
      "dist_data" = dist_data
    )
    result_list
  })

  investment_game_returns <-  reactive({ggplot(
    data=simulation_data()[["dist_data"]], aes(x=x_vals, y=y_vals)
  ) +
    geom_line(color="#004c93") + geom_area(fill="#dfe4f2") + 
    scale_y_continuous(expand = expansion(c(0, 0.1), c(0,0))) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("Ergebnisverteilung im Investitionsspiel") +
    xlab("") + ylab("") +
    theme_minimal() 
  })
  
  investment_start_returns <-  reactive({ggplot(
    data = simulation_data()[["plot_data"]], 
    mapping = aes(x=Individuum, y=w_init)) +
    geom_line(color="#004c93") + geom_area(fill="#dfe4f2") + 
    xlab("") + ylab("") +
    scale_y_continuous(expand = expansion(c(0, 0), c(0,5))) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("Vermögensverteilung zu Beginn") +
    theme_minimal()
  })
  
  investment_end_returns<-  reactive({ggplot(
    data = simulation_data()[["plot_data"]], 
    mapping = aes(x=Individuum, y=final_w)) +
    geom_line(color="#004c93") + geom_area(fill="#dfe4f2") + 
    theme_minimal() + xlab("") + ylab("") +
    scale_y_continuous(expand = expansion(c(0, 0), c(0,5))) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("Vermögensverteilung am Ende") 
  })
  
  investment_end_log_returns<-  reactive({
    simul_data <- simulation_data()[["plot_data"]]
    threshold_min <- quantile(simul_data$final_w, input$log_log_limits[1])
    threshold_max <- quantile(simul_data$final_w, input$log_log_limits[2])
    simul_data %>%
      dplyr::filter(final_w>=threshold_min,
                    final_w<=threshold_max) %>%
    ggplot(
    data = ., 
    mapping = aes(x=log(Individuum), y=log(final_w))) +
    geom_line(color="#004c93") + 
    theme_minimal() + xlab("") + ylab("") +
    scale_y_continuous(expand = expansion(c(0, 0), c(0,0))) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("Vermögensverteilung am Ende (log-log)") 
  })
  
  plot_obj <- reactive({
    full_plot <- ggpubr::ggarrange(investment_game_returns(), 
                                   investment_start_returns(), 
                                   investment_end_returns(), 
                                   investment_end_log_returns(),
                                   ncol = 2, nrow = 2, 
                                   labels = paste0(LETTERS[1:4], ")"), 
                                   font.label = list(face="bold"))
    full_plot
  })
  
  output$distribution_plot <- renderPlot({
    plot_obj()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("m_", input$m, "_n_", input$n, "_w_", 
            input$w_val, "_", tolower(substr(input$dist_kind, 1,4)), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_obj(), width = 8, height = 6)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
