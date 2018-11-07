#====================#
# App:  Outliers.App #
#                    #
# File: ui.R         #
#====================#

# Load packages
library(shiny)
library(readxl)
library(outliers)
library(rmarkdown)
library(tinytex)

# Load functions - R
source("R-functions/FUN.R", encoding="utf-8")

#================#
# Begin Shiny UI #
#================#
shinyUI(fluidPage(

  titlePanel("Outliers"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel
    sidebarPanel(

      h3("Dados"),

      # Select a file
      fileInput(inputId="file1", label="Escolher dados (.csv, .txt, .xls ou .xlsx)",
                            multiple=FALSE,  accept = c("text/csv", ".txt", ".csv", ".xls", ".xlsx",
                                                        "text/comma-separated-values,text/plain")),
      checkboxInput(inputId="header", label="Arquivo inclui cabecalho", value=TRUE),
      # textInput(inputId="text1", label="Inserir dados manualmente"),

      actionButton(inputId="loadFile", label="Carregar",
                   icon=icon(name="check-square-o", lib="font-awesome"), class="btn-primary"),

      hr(),
      h3("Relatorio"),
      radioButtons(inputId="format", label="Formato do documento",
                   c('PDF', 'HTML', 'Word'), inline = TRUE),
      downloadButton(outputId="downReportBtn", label="Baixar relatório",
                     class="btn-default") #style="background-color: black; color: white;")

    ), #endof sidebarPanel()

    # Main Panel (right side)
    mainPanel(

      tabsetPanel(

        # Outliers ------------------------------------------
        tabPanel(
          title="Outliers",
          br(),
          verbatimTextOutput(outputId="print_dados"),
          fluidRow(
            column(4, plotOutput(outputId="dados")),
            column(4, plotOutput(outputId="boxplot")),
            column(4, plotOutput(outputId="histogram"))
          ),

          fluidRow(
            column(6, verbatimTextOutput(outputId="t_IQR")),
            column(6, verbatimTextOutput(outputId="t_grubbs_10"))
          ),
          fluidRow(
            column(6, verbatimTextOutput(outputId="t_grubbs_11")),
            column(6, verbatimTextOutput(outputId="t_grubbs_20"))
          )

        ) #endof tabpanel()

        # Normality ----------------------------------------
        # tabPanel(
        #   title="Normality",
        #
        # ) #endof tabpanel()

      ) #endof tabsetPanel()

    ) # endof mainPanel()

  ) #endof sidebarLayout()



))

#==============#
# End Shiny UI #
#==============#
