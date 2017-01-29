
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "NFL Draft Analysis"),
  dashboardSidebar(
    sidebarUserPanel("Marc Fridson"),
    sidebarMenu(
    menuItem("NFL Team Draft Success", tabName = "nfl_team", icon = icon("dashboard")),
    menuItem("Pro Bowls by Round/Position", tabName = "probowl_rnd", icon = icon("th")),
    menuItem("Years in the NFL by Age", tabName = "age_years", icon = icon("th")),
    menuItem("NFL Success by School", tabName = "coll_success", icon = icon("th"))
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidPage(
      fluidRow(plotOutput("team_plot")),
      fillRow(box(tableOutput("Rnd_Summary"),checkboxGroupInput("check_pos", NULL, c("QB","RB","FB","WR","TE","T","G","C","DE","DT","LB","DB","K","P"),selected = c("QB","RB","FB","WR","TE","T","G","C","DE","DT","LB","DB","K","P"),inline = TRUE))
    )
    )
  )
)
)  
