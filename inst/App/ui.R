#====================#
# App:  Outliers.App #
#                    #
# File: ui.R         #
#====================#

# Load packages
library(shiny)
library(shinyjs)
library(shinyWidgets) # Nice inputs/buttons
library(formattable)  # Nice table
library(kableExtra)   # Extends knitr::kable options

library(readxl)       # Read/write .xls, .xlsx files
library(outliers)     # Functions:
library(rmarkdown)    # Report generation
library(tinytex)      # PDF Report generation

library(moments)      # Moments and Jarque-Bera normality test
library(nortest)      # Normality tests
library(robustbase)   # Adjusted boxplot

# Load functions - R
# source("R-functions/FUN.R", encoding="utf-8")

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
        # Check if contain header
        column(4, shinyWidgets::awesomeCheckbox( #materialSwitch( #checkboxInput( #
          inputId="checkHeader", label="Inclui cabeçalho", value=TRUE # right=FALSE, status="primary"
        ))
      ),

      actionButton(inputId="loadFile", label="Carregar", class="btn-primary",
                   icon=icon(name="angle-double-right", lib="font-awesome")),
      br(), br(), br(),

      shinyjs::hidden(
        div(id="showReportBtn",
            wellPanel(
              # h3("Relatório"),
              # radioButtons(inputId="format", label="Formato do documento",
              #              c("PDF", "HTML", "Word"), inline = TRUE),
              # shinyWidgets::radioGroupButtons(
              #   inputId="format", label="Formato do documento",
              #   choices=c("PDF", "HTML", "Word"), selected="PDF",
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

              # Diagnosis panel (Normality, Asymmetry)
              tabPanel(
                title="Diagnóstico",

                fluidRow(
                  column(4, plotOutput(outputId="histogram")),
                  column(4, plotOutput(outputId="boxplot")),
                  column(4, plotOutput(outputId="qqplot"))
                ),
                fluidRow(
                  column(3, formattable::formattableOutput(outputId="table_summary")),
                  column(5, formattable::formattableOutput(outputId="table_normtest")),
                  column(4, formattable::formattableOutput(outputId="table_stats"))
                )

              ), #endof tabpanel()

              # Outliers
              tabPanel(
                title="Outliers",
                br(),
                # verbatimTextOutput(outputId="print_dados"),

                fluidRow(
                  column(4, shinyWidgets::radioGroupButtons(
                    inputId="outlierTest", label="Selecione o teste:",
                    justified=FALSE, size="normal", direction="vertical", individual=FALSE,
                    choices=c("Intervalo Interquartil", "Grubbs 1 outlier",
                              "Grubbs 2 outliers (lados opostos)", "Grubbs 2 outliers (mesma cauda)",
                              "Dixon para outliers", "Qui-quadrado para outliers",
                              "Boxplot ajustado"),
                    checkIcon = list(yes = tags$i(class = "fa fa-check-square",
                                                  style = "color: steelblue"),
                                     no = tags$i(class = "fa fa-square-o",
                                                 style = "color: steelblue"))
                  )),
                  # column(8, verbatimTextOutput(outputId="table_tests"))
                  column(8,
                         # h3("Teste de Outlier"),
                         plotOutput(outputId="dados")
                  )

                ),
                fluidRow(
                  column(4, formattable::formattableOutput(outputId="table_outres", width="50%")),
                  # column(4, tableOutput(outputId="table_results")),
                  column(1),
                  column(4, formattable::formattableOutput(outputId="table_outtest")),
                  # column(6, tableOutput(outputId="table_tests")),
                  column(3)
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
