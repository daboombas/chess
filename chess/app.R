library(shiny)
ui <- fluidPage(img(src = 'wins_color.png', height = '700px', width = '850px'))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)