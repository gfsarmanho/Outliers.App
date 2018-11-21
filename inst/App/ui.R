#====================#
# App:  Outliers.App #
#                    #
# File: ui.R         #
#====================#

# Load packages
library(shiny)
library(shinyjs)
library(shinyWidgets) # Nice inputs/buttons

# library(shinythemes)

library(readxl)       # Read/write .xls, .xlsx files
library(outliers)     # Functions:
library(rmarkdown)    # Report generation
library(tinytex)      # PDF Report generation

# Load functions - R
source("R-functions/FUN.R", encoding="utf-8")

#================#
# Begin Shiny UI #
#================#
shinyUI(fluidPage(

  # shinythemes::themeSelector(),

  # Load shinyjs package inside app
  useShinyjs(),

  # App title
  titlePanel("Outliers"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel
    sidebarPanel(

      h3("Dados"),

      fluidRow(

        # Select a file
        column(8, fileInput(
          inputId="file1", label="Escolher dados (.csv, .txt, .xls ou .xlsx)",
          multiple=FALSE,  accept = c("text/csv", ".txt", ".csv", ".xls", ".xlsx",
                                      "text/comma-separated-values,text/plain")
        )),
        # File contain header?
        column(4, shinyWidgets::awesomeCheckbox( #checkboxInput( #
          inputId="checkHeader", label="Inclui cabeçalho", value=TRUE
        ))
      ),
      shinyWidgets::awesomeCheckbox( #checkboxInput( #
        inputId="checkManual", label="Inserir dados manualmente", value=FALSE
      ),
      # textInput(inputId="data1", label="Inserir dados manualmente:"),

      actionButton(inputId="loadFile", label="Carregar", class="btn-primary",
                   icon=icon(name="angle-double-right", lib="font-awesome")),
      br(), br(), br(),

      shinyjs::hidden(
        div(id="showReportBtn",
            wellPanel(
              # h3("Relatório"),
              # radioButtons(inputId="format", label="Formato do documento",
              #              c('PDF', 'HTML', 'Word'), inline = TRUE),
              # shinyWidgets::radioGroupButtons(
              #   inputId="format", label="Formato do documento",
              #   choices=c('PDF', 'HTML', 'Word'), selected="PDF",
              #   checkIcon = list(yes = tags$i(class = "fa fa-check-square",
              #                                 style = "color: steelblue"),
              #                    no = tags$i(class = "fa fa-square-o",
              #                                style = "color: steelblue"))
              # ),
              # downloadButton(outputId="downReportBtn", label="Baixar relatório",
              #                class="btn-default"), #style="background-color: black; color: white;")
              actionButton(inputId="modalReportBtn", label="Gerar relatório",  class="btn-default",
                           icon=icon(name="file-alt", lib="font-awesome"))
            ) #endof wellPanel()
        )
      )
    ), #endof sidebarPanel()

    # Main Panel (right side)
    mainPanel(

      shinyjs::hidden(
        div(id="mainPanel",
            tabsetPanel(

              # Outliers ------------------------------------------
              tabPanel(
                title="Outliers",
                br(),
                verbatimTextOutput(outputId="print_dados"),

                shinyWidgets::radioGroupButtons(
                  inputId="tests", label="Selecione o teste", justified=FALSE,
                  choices=c('Intervalo', 'Grubbs one', 'Grubbs two', 'Grubbs'),
                  checkIcon = list(yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue"))
                ),

                fluidRow(
                  column(6, plotOutput(outputId="dados")),
                  column(6, plotOutput(outputId="out_test"))
                ),

                fluidRow(
                  column(6, verbatimTextOutput(outputId="t_IQR")),
                  column(6, verbatimTextOutput(outputId="t_grubbs_10"))
                ),
                fluidRow(
                  column(6, verbatimTextOutput(outputId="t_grubbs_11")),
                  column(6, verbatimTextOutput(outputId="t_grubbs_20"))
                )

              ), #endof tabpanel()

              # Normality ----------------------------------------
              tabPanel(
                title="Diagnóstico",

                fluidRow(
                  column(4, plotOutput(outputId="histogram")),
                  column(4, plotOutput(outputId="qqplot")),
                  column(4, plotOutput(outputId="boxplot"))
                )

              ) #endof tabpanel()

            )
        )

      ) #endof tabsetPanel()

    ) # endof mainPanel()

  ) #endof sidebarLayout()



))

#==============#
# End Shiny UI #
#==============#
