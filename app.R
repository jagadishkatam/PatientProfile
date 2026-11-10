library(shiny)
library(bslib)

options(shiny.maxRequestSize = 200 * 1024^2)

shinyApp(ui = app_ui, server = app_server)