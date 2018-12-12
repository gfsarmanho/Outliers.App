
#==============#
# Begin server #
#==============#
shinyServer(function(input, output, session){

  # Reactive variables
  RV <- reactiveValues(
    dados = NULL,
    measu = "",

    res_fun_out = NULL,
    tab_test    = NULL,
    out.ind     = NULL,

    res_iqr       = NULL,
    res_grubbs_10 = NULL,
    res_grubbs_11 = NULL,
    res_grubbs_20 = NULL,
    res_dixon     = NULL,
    res_chisq     = NULL
  )

  observeEvent(
    eventExpr={
      input$loadFile
    },
    handlerExpr={

      # Ensure there is an input file
      req(input$file1)

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
      RV$res_iqr_0 <- IQR.test(x=RV$dados)
      RV$res_iqr   <- fun_outlier(RV$res_iqr_0, x.data=RV$dados)

      RV$res_grubbs_10_0 <- grubbs.test(x=RV$dados, type=10)
      RV$res_grubbs_10   <- fun_outlier(RV$res_grubbs_10_0, x.data=RV$dados)

      RV$res_grubbs_11_0 <- grubbs.test(x=RV$dados, type=11)
      RV$res_grubbs_11   <- fun_outlier(RV$res_grubbs_11_0, x.data=RV$dados)

      RV$res_grubbs_20_0 <- grubbs.test(x=RV$dados, type=20)
      RV$res_grubbs_20   <- fun_outlier(RV$res_grubbs_20_0, x.data=RV$dados)

      RV$res_dixon_0 <- dixon.test(x=RV$dados, type=0)
      RV$res_dixon   <- fun_outlier(RV$res_dixon_0, x.data=RV$dados)

      RV$res_chisq_0 <- chisq.out.test(x=RV$dados)
      RV$res_chisq   <- fun_outlier(RV$res_chisq_0, x.data=RV$dados)

      shinyjs::show(id="showReportBtn")
      shinyjs::show(id="mainPanel")

    }
  ) #endof observeEvent()

  #----------------------------------------------#
  #                  Data                        #
  #----------------------------------------------#
  # output$print_dados <- renderPrint({
  #   if(is.null(RV$dados)){
  #     return(invisible())
  #   } else {
  #     cat(
  #       paste("Dados carregados (n=", RV$n.dados, "):\n",
  #               paste(as.character(RV$dados), collapse=", "), sep="")
  #     )
  #   }
  # })

  #------------------------------------------------#
  #                  Tables                        #
  #------------------------------------------------#

  observe(
    RV$res_fun_out <- switch(input$outlierTest,
                             "Intervalo Interquartil"            = RV$res_iqr,
                             "Grubbs 1 outlier"                  = RV$res_grubbs_10,
                             "Grubbs 2 outliers"                 = RV$res_grubbs_11,
                             "Grubbs 2 outliers (lados opostos)" = RV$res_grubbs_20,
                             "Dixon para outliers"               = RV$res_dixon,
                             "Qui-quadrado para outliers"        = RV$res_chisq
    )
  )

  #------------------------#
  # TABLE: Normality tests #
  #------------------------#
  output$table_norm <- renderFormattable({

    # Functions to be applied
    fun_norm <- list(shapiro.test, function(x) ks.test(x, "pnorm"),
                     nortest::lillie.test, nortest::ad.test,
                     moments::jarque.test)
    # nortest::cvm.test, nortest::pearson.test, nortest::sf.test
    res_norm <- sapply(fun_norm, do.call, args = list(RV$dados))
    res_norm.stats <- sapply(res_norm, with, c(statistic, p.value))

    # Table to be saved
    tab_norm <- data.frame(
      "Teste" = c("Shapiro-Wilk", "Kolmogorov-Smirnov (K-S)",
                  "Lilliefors K-S", "Anderson-Darling", "Jarque-Bera"),
      # "Cramer-von Mises", "Qui-quadrado de Pearson", "Shapiro-Francia"
      "Estatística"  = res_norm.stats[1, ] ,
      "P.valor"      = formattable::scientific( res_norm.stats[2, ] )
    )

    # Table to be show
    formattable(tab_norm, align=c("c","c", "c"), list(
      Teste = formatter("span", style = ~ style(color="grey", font.weight="bold")),
      "P.valor" = formatter("span", style = x ~ style(color=ifelse(x>=0.05, "green", "red")))
    ))

  })

  #------------------------#
  # TABLE: Data Statistics #
  #------------------------#
  output$table_stat <- renderFormattable({

    # Table to be saved
    tab_stat <- data.frame(
      Medida = c("Mínimo", "1o Quartil", "Mediana", "Média", "3o Quartil", "Máximo",
                 "Coef. Curtose", "Coef. assimetria"),
      Valor  = c(summary(RV$dados), moments::kurtosis(RV$dados),
                 moments::skewness(RV$dados))
    )

    # Table to be show
    formattable(tab_stat, align=c("c","c"), list(
      Medida = formatter("span", style = ~ style(color="grey", font.weight="bold"))
      #Valor = comma("span")
    ))

  })

  #------------------------#
  # TABLE: Outlier results #
  #------------------------#
  output$table_results <- renderFormattable({
    # output$table_results <- function(){

    # Table to be saved
    tab_dados <- data.frame(stringsAsFactors=FALSE,
                            Réplica=1:RV$n.dados,
                            Medição=RV$dados, #accounting(RV$dados),
                            Resultado=rep(TRUE, RV$n.dados)
    )

    RV$tab_test <- RV$res_fun_out$tab_test
    RV$out.ind  <- RV$res_fun_out$out.ind
    if(!is.null(RV$out.ind)) tab_dados$Resultado[RV$out.ind] <- FALSE

    # Table to be show (using kable)
    # kable(tab_dados, format="html") %>%
    #   kable_styling(full_width=FALSE, position = "center",
    #                 bootstrap_options=c("hover", "condensed"))
    # # gsub("<thead>.*</thead>", "", tt) # Remove header

    # Table to be show
    formattable(tab_dados, align=c("c","c","c"), list(
      Réplica = formatter("span", style = ~ style(color="grey", font.weight="bold")),
      # Medição = color_tile("white", plot_colors[1]),
      Resultado = formatter("span",
                            style = x ~ style(color=ifelse(x, "green", "red")),
                            x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Ok", "Outlier"))
      )
    ))

  })

  #------------------------#
  # TABLE: Outlier results #
  #------------------------#
  output$table_tests <- renderPrint({
    # req(!is.null(RV$dados))
    if(is.null(RV$dados)){
      return(invisible())
    } else {
      tab_tests <- kable(RV$tab_test, format="html") %>%
        kable_styling(bootstrap_options=c("hover", "condensed"),
                      full_width=FALSE, position = "center") #%>%
      #gsub("<thead>.*</thead>", "", tt) # Remove first line
      tab_tests
    }

  })

  #-----------------------------------------------#
  #                  Plots                        #
  #-----------------------------------------------#

  # Plot - data
  output$dados <- renderPlot({
    if(is.null(RV$dados)){
      return()
    } else {

      p_name <- "plot_dados"
      assign(x=p_name, envir=.GlobalEnv, value= function(){

        xx <- RV$dados
        cores <- rep(plot_colors[1], length(RV$dados))
        if(!is.null(RV$out.ind)){ cores[RV$out.ind] <- plot_colors[2] }

        plot(xx[order(xx)], col=cores[order(xx)], pch=19, cex=1.5,
             xlab="Dados ordenados", ylab="", main=RV$measu)
        # points(RV$out.ind[order(xx)])

        if(!is.null(RV$out.ind)){
          legend("bottomright", pch=c(19,19), col=plot_colors[1:2],
                 c("Data", "Outlier"), bty="n", cex=1.2, box.col="black")
        }

      })
      get(p_name)()

    }
  })

  #---------------#
  # PLOT: BoxPlot #
  #---------------#
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

  #-----------------#
  # PLOT: Histogram #
  #-----------------#
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

  #--------------#
  # PLOT: QQplot #
  #--------------#
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


  #-----------------------------------------------#
  #                  REPORT                       #
  #-----------------------------------------------#
  # Modal
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
                            choices=c("Intervalo Interquartil", "Grubbs 1 outlier",
                                      "Grubbs 2 outliers", "Grubbs 2 outliers (lados opostos)",
                                      "Dixon para outliers", "Qui-quadrado para outliers"),
                            selected=c("Intervalo Interquartil", "Grubbs 1 outlier",
                                       "Grubbs 2 outliers", "Grubbs 2 outliers (lados opostos)",
                                       "Dixon para outliers", "Qui-quadrado para outliers")
                            # choices=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs", "Dixon", "Chi-Square"),
                            # selected=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs", "Dixon", "Chi-Square")
                          ),
                          shinyWidgets::awesomeCheckboxGroup(
                            inputId="diagsModal", label="Incluir gráficos diagnóstico:",
                            choices=c("Histograma", "QQ-plot", "Boxplot"),
                            selected=c("Histograma", "QQ-plot", "Boxplot")
                          ),
                          textAreaInput(inputId="obsModal", label="Observações:", value="",
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
    )) #endofshowModal()

  })

  # Donload report mechanism
  output$downReportBtn <- downloadHandler(
    filename = function() {
      paste("report", sep=".",
            switch(input$format, PDF="pdf", HTML="html", Word="docx")
      )
    },

    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(from=src, to="report.Rmd", overwrite=TRUE)

      library(rmarkdown)
      out <- rmarkdown::render(input="report.Rmd",
                               encoding="UTF-8",
                               output_format=switch(input$format,
                                                    PDF=pdf_document(),
                                                    HTML=html_document(),
                                                    Word=word_document())
      )

      file.rename(out, file)
    }
  ) #endof downloadHandler()


})

#===============#
# End of server #
#===============#
