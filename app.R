# final-project-Developing-data-products
library(shiny)
library(datasets)
library(dplyr)

ui <- shinyUI(fluidPage(
      
      titlePanel("MTcars dataset exploration (Explaining MPG variable)"),
      
      sidebarLayout(
            sidebarPanel(
                  checkboxGroupInput("regressors", 
                                     label = h3("Choose regresors"), 
                                     choices=names(select(mtcars,-mpg)),
                                     selected = "wt"
                  ),
                  width=3
            ),
            
            
            mainPanel(
                  verbatimTextOutput("fit"),
                  plotOutput("mpgPlot")
            )
      )
))

mpgData <- mtcars

#Modify dataframe
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
#mpgData$cyl<-as.factor(mpgData$cyl)
#mpgData$gear<-as.factor(mpgData$gear)
#mpgData$carb<-as.factor(mpgData$carb)
#mpgData$vs<-as.factor(mpgData$vs)


server <- shinyServer(function(input, output) {
      
      checkedVal <- reactive({
            perm.vector <- as.vector(input$regressors)
            predForm<-ifelse(length(perm.vector)>0,
                             predictors<-paste(perm.vector,collapse="+"),
                             "1")
            lmForm<-paste("mpg~",predForm,sep="") 
            
      }) 
      
      fitModel<-reactive({
            fitFormula<-as.formula(checkedVal())
            lm(fitFormula,data=mpgData)
      })
      
      output$caption <- renderText({
            checkedVal()
      })
      
      #Print the coeffecients of the regression model
      output$fit <- renderPrint({
            summary(fitModel())$coef
      })
      
      #Plot Diagnostics for the generated regression model
      output$mpgPlot<-renderPlot({
            par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
            plot(fitModel(), sub.caption = "Regression details plot")
            
      })
})

shinyApp(ui, server)
