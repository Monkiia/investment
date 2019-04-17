#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



simplemodel = function(pv,r,t){
  return(pv*(1+r)^t)
}

fixedmodel = function(pv,contri,r,t){
  fva = contri*((1+r)^t-1)/r
  return(simplemodel(pv,r,t)+ fva)
}

grmodel = function(pv,contri,r,gr,t){
  fvga = contri*((1+r)^t-(1+gr)^t)/(r-gr)
  return(simplemodel(pv,r,t)+ fvga)
}


initial = sliderInput(inputId = "initial",
                      label = "Initial Amount",
                      value = 1000, min = 0, max = 100000, step = 500, pre = "$")

annual = sliderInput(inputId = "annual",
                     label = "Annual Contribution",
                     value = 2000, min = 0, max = 50000, step = 500, pre = "$")

rr = sliderInput(inputId = "return",
                 label = "Return Rate",
                 value = 5, min = 0, max = 20, step = 0.1)

gr = sliderInput(inputId = "growth",
                 label = "growth rate",
                 value = 2, min = 0, max = 20, step = 0.1)

years = sliderInput(inputId = "years",
                    label = "Years",
                    value = 20 , min = 0, max = 50, step = 1)

reportedtable = dataTableOutput("table")

reportedplot = plotOutput("plot")

yesno = selectInput(inputId = "yesno",
                    label = "Facet?", choices = c("Yes","No"), selected = "No")
ui = fluidPage("Yo Yo qiekenao",fluidRow(column(4,initial),column(4,rr),column(4,years)),fluidRow(column(4,annual),column(4,gr),column(4,yesno)),reportedplot, reportedtable)

server = function(input,output) {
  df = data.frame(year = 0:10)
  output$plot = renderPlot({
    x = 0:10
    plot(x = x ,y = simplemodel(input$initial,input$return/100,x),type = "l",col = "red", ylim = c(0,30000),xlab = "Years",   ylab = "balances")
    lines(x = x ,y = fixedmodel(input$initial,input$annual,input$return/100,x),col = "green")
    lines(x = x ,y = grmodel(input$initial,input$annual,input$return/100,input$growth/100,x), col = "blue")
    
  })
  output$table = renderDataTable({
    df$no_contrib = floor(simplemodel(input$initial,input$return/100,df$year))
    df$fixed_contrib = floor(fixedmodel(input$initial,input$annual,input$return/100,df$year))
    df$growing_contrib = floor(grmodel(input$initial,input$annual,input$return/100,input$growth/100,df$year))
    return(df)
  })
  
}
shinyApp(ui = ui, server = server)

