
#==============#
# Begin server #
#==============#

shinyServer(function(input, output, session){

  # Reactive variables
  RV <- reactiveValues(
    dados = NULL,
    measu = "",
    res_grubbs_10 = NULL,
    res_grubbs_11 = NULL,
    res_grubbs_20 = NULL
  )

  observeEvent(
    eventExpr={
      input$loadFile
    },
    handlerExpr={

      # Load data file
      arq_names <- input$file1$datapath
      arq_ext   <- tail(unlist(strsplit(x=input$file1$name, split="\\.")), n=1)

      if(arq_ext == "txt")  dados <- read.table(arq_names, sep="\t", header=input$checkHeader)
      if(arq_ext == "csv")  dados <- read.csv(arq_names, sep=",", dec=".", header=input$checkHeader)
      if(arq_ext %in% c("xls", "xlsx")) dados <- readxl::read_excel(arq_names, sheet=1)


      dados <- as.data.frame(dados)
      RV$dados <- as.numeric(dados[, ncol(dados)])

      if(input$checkHeader) RV$measu <- names(dados)[ncol(dados)] else RV$measu <- ""

      RV$n.dados <- length(RV$dados)

      # Evaluate tests
      RV$res_grubbs_10 <- grubbs.test(x=RV$dados, type=10)
      RV$res_grubbs_11 <- grubbs.test(x=RV$dados, type=11)
      RV$res_grubbs_20 <- grubbs.test(x=RV$dados, type=20)

      # RV$res_iqr <- IQR.test(x=RV$dados)

      shinyjs::show(id="showReportBtn")
      shinyjs::show(id="mainPanel")

    }
  ) #endof observeEvent()

  #------#
  # Data #
  #------#
  output$print_dados <- renderPrint({
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      cat(
        paste("Dados carregados (n=", RV$n.dados, "):\n",
                paste(as.character(RV$dados), collapse=", "), sep="")
      )
    }
  })

  #-------#
  # Tests #
  #-------#

  output$out_table <- renderFormattable({
    tab_dados <- data.frame(Replica=1:RV$n.dados,
                            Medicao=RV$dados,
                            Resultado=rep("OK", RV$n.dados))
    formattable(tab_dados, list())
  })

  output$out_test <- renderPrint({

    if(is.null(RV$dados)){
      return(invisible())
    } else {
      switch(input$outlierTest,
             "Intervalo" = IQR.test(x=RV$dados),
             "Grubbs one" = RV$res_grubbs_10,
             "Grubbs two" = RV$res_grubbs_11,
             "Grubbs two (opostos)" = RV$res_grubbs_20
      )
    }
  })



  # IQR
  output$t_IQR <- renderPrint({
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      IQR.test(x=RV$dados)
    }
  })

  # Grubbs
  output$t_grubbs_10 <- renderPrint({
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      RV$res_grubbs_10
    }
  })
  output$t_grubbs_11 <- renderPrint({
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      RV$res_grubbs_11
    }
  })
  output$t_grubbs_20 <- renderPrint({
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      RV$res_grubbs_20
    }
  })

  #-------#
  # Plots #
  #-------#

  # Plot - data
  output$dados <- renderPlot({
    if(is.null(RV$dados)){
      return()
    } else {

      p_name <- "plot_dados"
      assign(x=p_name, envir=.GlobalEnv, value= function(){
        plot(sort(RV$dados), pch=19, col=plot_colors[1],
             xlab="Dados ordenados", ylab="", main=RV$measu)
      })
      get(p_name)()

    }
  })

  # Plot - boxplot
  output$boxplot <- renderPlot({
    if(is.null(RV$dados)){
      return()
    } else {

      p_name <- "plot_boxplot"
      assign(x=p_name, envir=.GlobalEnv, value= function(){
        boxplot(RV$dados, col=plot_colors[2], xlab="", ylab="Dados", main=RV$measu)
      })
      get(p_name)()
    }
  })

  # Plot - histogram
  output$histogram <- renderPlot({
    if(is.null(RV$dados)){
      return()
    } else {

      p_name <- "plot_histograma"
      assign(x=p_name, envir=.GlobalEnv, value= function(){

        hist(RV$dados, col=plot_colors[1], prob=TRUE,
             xlab="Dados", ylab="Frequencia", main=RV$measu)
        lines(density(RV$dados), col=plot_colors[2], lwd=2)

      })
      get(p_name)()

    }
  })

  # Plot - qqPlot
  output$qqplot <- renderPlot({
    if(is.null(RV$dados)){
      return()
    } else {

      p_name <- "plot_qqplot"
      assign(x=p_name, envir=.GlobalEnv, value= function(){

        qqnorm(RV$dados, col=plot_colors[1], pch=19,
               xlab="Quantis Teóricos", ylab="Quantis amostrais", main=RV$measu)
        qqline(RV$dados, col=plot_colors[2], lwd=2)

      })
      get(p_name)()

    }
  })

  # shapiro.test(x)
  # qqnorm(, main="Normal QQ Plot")
  # qqline(, col="")

  observeEvent(input$modalReportBtn, {

    showModal(modalDialog(easyClose=TRUE, footer=NULL,
      title = "Informações para gerar relatório técnico",

      textInput(inputId="personModal", label="Responsável"),

      # checkboxGroupButtons(inputId="testsModal", label="Incluir testes:",
      #                      choices=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs"),
      #                      selected=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs")
      # ),
      shinyWidgets::awesomeCheckboxGroup(
        inputId="testsModal", label="Incluir testes:",
        choices=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs"),
        selected=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs")
      ),
      shinyWidgets::awesomeCheckboxGroup(
        inputId="diagsModal", label="Incluir Diagnósticos:",
        choices=c("Histograma", "qqplot", "boxplot"),
        selected=c("Histograma", "qqplot", "boxplot")
      ),
      textAreaInput(inputId="obsModal", label="Observações:", value = "",
                    placeholder="Insira aqui comentários gerais."),
      br(),
      shinyWidgets::radioGroupButtons(
        inputId="format", label="Formato do documento",
        choices=c("PDF", "HTML", "Word"), selected="PDF",
        checkIcon = list(yes = tags$i(class = "fa fa-check-square",
                                      style = "color: steelblue"),
                         no = tags$i(class = "fa fa-square-o",
                                     style = "color: steelblue"))
      ),
      # icon=icon(name="file-pdf", lib="font-awesome")
      # icon=icon(name="file-word", lib="font-awesome")
      # icon=icon(name="html5", lib="font-awesome")

      downloadButton(outputId="downReportBtn", label="Gerar relatório",
                      class="btn-default") #style="background-color: black; color: white;")

    ))

  })


  #
  output$downReportBtn <- downloadHandler(
    filename = function() {
      paste("relatorio", sep=".",
            switch(input$format, PDF="pdf", HTML="html", Word="docx")
      )
    },

    content = function(file) {
      src <- normalizePath("relatorio.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(from=src, to="relatorio.Rmd", overwrite=TRUE)

      library(rmarkdown)
      out <- render(input="relatorio.Rmd",
                    output_format=switch(input$format, PDF=pdf_document(),
                                         HTML=html_document(), Word=word_document())
      )

      file.rename(out, file)
    }
  ) #endof downloadHandler()


})

#===============#
# End of server #
#===============#
