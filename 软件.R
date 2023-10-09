#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("术后肺部感染预测"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("cerebral_infarction", "脑梗死（是/否）:", 0),
      numericInput("coma", "昏迷（是/否）:", 0),
      numericInput("dim", "DD二聚体（mg/L）:", 0),
      numericInput("CRP_max", "C反应蛋白（mg/L）:", 0),
      numericInput("age", "年龄（岁）:", 0),
      numericInput("sbp", "收缩压（mmHg）:", 0),
      numericInput("icutime", "ICU时间（天）:", 0),
      numericInput("hxj", "呼吸机时间（天）:", 0),
      actionButton("predict", "预测")
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    z <- -0.0274 + 0.0163*input$cerebral_infarction + 0.0188*input$coma + 
      0.0294*input$dim + 0.0090*input$CRP_max - 0.0012*input$age - 
      0.0344*input$sbp + 0.3347*input$icutime - 5.235e-7*input$hxj
    
    probability <- 1 / (1 + exp(-z))
    
    output$prediction <- renderText({
      paste("术后肺部感染的可能性为:", round(probability * 100, 2), "%")
    })
  })
}

shinyApp(ui = ui, server = server)
