
#==============#
# Begin server #
#==============#
shinyServer(function(input, output, session){

  # Add report file to temporary directory
  temp_dir_report <- file.path(tempdir(), "logo.png")
  file.copy("www/logo.png", temp_dir_report, overwrite=TRUE)

  # Reactive variables
  RV <- reactiveValues(
    dados = NULL,
    measu = "",

    res_fun_out = NULL,
    out.ind     = NULL,

    res_iqr       = NULL,
    res_grubbs_10 = NULL,
    res_grubbs_11 = NULL,
    res_grubbs_20 = NULL,
    res_dixon     = NULL,
    res_chisq     = NULL,

    res_adj       = NULL
  )

  RVTAB <- reactiveValues(
    tab_summary=NULL,
    tab_normtest=NULL,
    tab_stats=NULL,

    tab_outres=NULL,
    tab_outtest=NULL
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

      RV$res_adj_0 <- adjbox.test(x=RV$dados)
      RV$res_adj   <- fun_outlier(RV$res_adj_0, x.data=RV$dados)

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
                             "Grubbs 2 outliers (lados opostos)" = RV$res_grubbs_11,
                             "Grubbs 2 outliers (mesma cauda)"   = RV$res_grubbs_20,
                             "Dixon para outliers"               = RV$res_dixon,
                             "Qui-quadrado para outliers"        = RV$res_chisq,
                             "Boxplot ajustado"                  = RV$res_adj
    )
  )

  #------------------------#
  # TABLE: Data Statistics #
  #------------------------#
  output$table_summary <- renderFormattable({

    # Table to be saved
    tab_summary <- data.frame(
      Medida = c("Mínimo", "Mediana", "Média", "Desvio-padrão", "Máximo"),
      Valor  = sapply(list(min, mean, median, sd, max),
                      function(fun, x) fun(x, na.rm=TRUE), x=RV$dados)
    )

    # Store dynamic table
    RVTAB$tab_summary <- tab_summary

    # Table to be show
    formattable(tab_summary, align=c("c","c"), list(
      Medida = formatter("span", style = ~ style(color="grey", font.weight="bold"))
    ))

  })

  #------------------------#
  # TABLE: Normality tests #
  #------------------------#
  output$table_normtest <- renderFormattable({

    # Functions to be applied
    fun_norm <- list(shapiro.test, function(x) ks.test(x, "pnorm"),
                     nortest::lillie.test, nortest::ad.test,
                     moments::jarque.test)
    # nortest::cvm.test, nortest::pearson.test, nortest::sf.test
    res_norm       <- sapply(fun_norm, do.call, args = list(RV$dados))
    res_norm.stats <- sapply(res_norm, with, c(statistic, p.value))

    # Table to be saved
    tab_normtest <- data.frame(
      "Teste" = c("Shapiro-Wilk", "Kolmogorov-Smirnov (K-S)",
                  "Lilliefors K-S", "Anderson-Darling", "Jarque-Bera"),
      # "Cramer-von Mises", "Qui-quadrado de Pearson", "Shapiro-Francia"
      "Estatística"  = res_norm.stats[1, ] ,
      "P.valor"      = formattable::scientific( res_norm.stats[2, ] )
    )

    # Store dynamic table
    RVTAB$tab_normtest <- tab_normtest

    # Table to be show
    formattable(tab_normtest, align=c("c","c", "c"), list(
      Teste     = formatter("span", style = ~ style(color="grey", font.weight="bold")),
      "P.valor" = formatter("span", style = x ~ style(color=ifelse(x>=0.05, "green", "red")))
    ))

  })

  #-------------------------------#
  # TABLE: Assymetry and Kurtosis #
  #-------------------------------#
  output$table_stats <- renderFormattable({

    # Table to be saved
    tab_stats <- data.frame(
      Medida = c("Coef. Curtose", "Coef. assimetria"),
      Valor  = c(moments::kurtosis(RV$dados), moments::skewness(RV$dados))
    )

    # Store dynamic table
    RVTAB$tab_stats <- tab_stats

    # Table to be show
    formattable(tab_stats, align=c("c","c"), list(
      Medida = formatter("span", style = ~ style(color="grey", font.weight="bold"))
    ))

  })

  #------------------------#
  # TABLE: Outlier results #
  #------------------------#
  output$table_outres <- renderFormattable({
    # output$table_results <- function(){

    tab_outres  <- RV$res_fun_out$tab_outres
    RV$out.ind  <- RV$res_fun_out$out.ind   # Could be anywhere...

    # Store dynamic table
    RVTAB$tab_outres  <- tab_outres

    # Table to be show
    formattable(tab_outres, align=c("c","c","c"), list(
      Réplica = formatter("span", style = ~ style(color="grey", font.weight="bold")),
      # Medição = color_tile("white", plot_colors[1]),
      Resultado = formatter("span",  style = x ~ style(color=ifelse(x, "green", "red")),
                            x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Ok", "Outlier"))
      )
    ))

  })

  #----------------------#
  # TABLE: Outlier Tests #
  #----------------------#
  output$table_outtest <- renderFormattable({

    tab_outtest <- RV$res_fun_out$tab_outtest

    # Store dynamic table
    RVTAB$tab_outtest <- tab_outtest

    formattable(tab_outtest, align=c("l","r"), list(
    "Parâmetro" = formatter("span", style = ~ style(color="grey", font.weight="bold"))
    ))
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
                 c("Dados", "Outlier sugerido"), bty="n", cex=1.2, box.col="black")
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
                          shinyWidgets::awesomeCheckboxGroup(
                            inputId="testsModal", label="Incluir testes:",
                            choices=c("Intervalo Interquartil", "Grubbs 1 outlier",
                                      "Grubbs 2 outliers (lados opostos)", "Grubbs 2 outliers (mesma cauda)",
                                      "Dixon para outliers", "Qui-quadrado para outliers",
                                      "Boxplot ajustado"),
                            selected=c("Intervalo Interquartil")
                                       # "Grubbs 1 outlier",
                                       # "Grubbs 2 outliers (lados opostos)", "Grubbs 2 outliers (mesma cauda)",
                                       # "Dixon para outliers", "Qui-quadrado para outliers",
                                       # "Boxplot ajustado")
                            # choices=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs", "Dixon", "Chi-Square"),
                            # selected=c("Intervalo", "Grubbs one", "Grubbs two", "Grubbs", "Dixon", "Chi-Square")
                          ),
                          fluidRow(
                            column(6, shinyWidgets::awesomeCheckboxGroup(
                              inputId="diagsPlotModal", label="Incluir gráficos diagnóstico:",
                              choices=c("Histograma", "QQ-plot", "Boxplot"),
                              selected=c("Histograma", "QQ-plot", "Boxplot")
                            )),
                            column(6, shinyWidgets::awesomeCheckboxGroup(
                              inputId="diagsTableModal", label="Incluir tabelas diagnóstico:",
                              choices=c("Sumário dos dados", "Testes de Normalidade", "Assimetria e Curtose"),
                              selected=c("Sumário dos dados", "Testes de Normalidade", "Assimetria e Curtose")
                            ))
                          ),
                          textAreaInput(inputId="obsModal", label="Observações:", value="",
                                        width='100%',
                                        placeholder="Insira aqui comentários gerais."),
                          br(),
                          shinyWidgets::radioGroupButtons(
                            inputId="format", label="Formato do documento",
                            choices=c("PDF", "HTML"), #, "Word"),
                            selected="PDF",
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
      # formato <- switch(input$format, PDF="pdf", HTML="html", Word="docx")
      report_name <- paste("report_", input$format, ".Rmd", sep="")

      src <- normalizePath(report_name)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(from=src, to=report_name, overwrite=TRUE)

      library(rmarkdown)
      # out <- rmarkdown::render(input=paste("report_", input$format, ".Rmd"),
      out <- rmarkdown::render(input=report_name,
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
