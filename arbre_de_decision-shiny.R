library(shiny)
library(rpart)

# Shiny App
shinyApp(
  ui = fluidPage(
    titlePanel("INF 3712 Arbre de Decision"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Charger le fichier CSV",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        actionButton("run_analysis", "Lancer l'analyse"),
        hr()
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Visualisation des Données", 
                   tabsetPanel(
                     tabPanel("Résumé", verbatimTextOutput("model_summary")),
                     tabPanel("Str", verbatimTextOutput("str_output")),
                     tabPanel("Données", tableOutput("data_table")),
                     tabPanel("Graphique", plotOutput("tree_plot")),
                     tabPanel("Encoded Data", verbatimTextOutput("encoded_data_output"))
                   )
          ),
          tabPanel("Évaluations", 
                   tabsetPanel(
                     tabPanel("Entraînement", verbatimTextOutput("model_evaluation_train_output")),
                     tabPanel("Test", verbatimTextOutput("model_evaluation_test_output"))
                   )
          )
        )
      )
    )
  ),
  
  server = function(input, output) {
    
    data <- reactive({
      req(input$file)
      read.table(input$file$datapath, sep = ",", header = TRUE)
    })
    
    observeEvent(input$run_analysis, {
      withProgress(message = 'Traitement en cours...', value = 0, {
        
        incProgress(0.2)
        
        # CHANGER LES VARIABLES CATEGORIEBLES EN VARIABLE NUMERIQUE A L'AIDE DU ONE HOT ENCODING
        encoded_data <- model.matrix(~ Gender + family_history_with_overweight + FAVC + CAEC + SMOKE + SCC + CALC + MTRANS - 1, data = data())
        non_encoded_columns <- data()[, !(names(data()) %in% c("Gender", "family_history_with_overweight","FAVC","CAEC","SMOKE","SCC","CALC","MTRANS"))]
        encoded_data <- cbind(encoded_data, non_encoded_columns)
        
        # Appliquer la fonction sample pour les données d'entraînement
        tr <- sample(1:nrow(encoded_data), 1000)
        train <- encoded_data[tr, ]
        test <- encoded_data[-tr, ]
        
        incProgress(0.3)
        
        # Construction du modèle d'arbre
        tree <- rpart(NObeyesdad ~ ., train)
        
        incProgress(0.5)
        
        output$tree_plot <- renderPlot({
          plot(tree)
          text(tree)
        })
        
        output$model_summary <- renderPrint({
          summary(data())
        })
        
        output$str_output <- renderPrint({
          str(data())
        })
        
        output$data_table <- renderTable({
          head(data())
        })
        
        # Afficher le résultat encoded_data
        output$encoded_data_output <- renderPrint({
          head(encoded_data)
        })
        
        incProgress(0.2)
        
        # Evaluation du modèle sur les données d'entraînement
        pred_train <- predict(tree, train, type = c("class"))
        tab_train <- table(pred_train, train$NObeyesdad[][])
        precision_train <- round(sum(diag(tab_train)) / sum(tab_train), 2)
        erreur_train <- 1 - precision_train
        
        # Evaluation du modèle sur les données de test
        pred_test <- predict(tree, test, type = c("class"))
        tab_test <- table(pred_test, test$NObeyesdad[][])
        # Précision
        precision <- round(tab_test[2, 2] / (tab_test[2, 2] + tab_test[1, 2]), 2)
        precision
        #Taux d'erreur
        erreur = 1-precision
        erreur
        # Rappel (Sensibilité)
        rappel <- round(tab_test[2, 2] / (tab_test[2, 2] + tab_test[2, 1]), 2)
        rappel
        # Spécificité
        specificite <- round(tab_test[1, 1] / (tab_test[1, 1] + tab_test[1, 2]), 2)
        specificite
        # Pureté
        purete <- round(sum(diag(tab_test)) / sum(tab_test), 2)
        purete
        
        output$model_evaluation_test_output <- renderPrint({
          cat("\nMatrice de confusion (Données de test):", tab_test)
          cat("\nPrécision (Données de test):", precision)
          cat("\nTaux d'Erreur (Données de test):", erreur)
          cat("\nRappel (Données de test):", rappel)
          cat("\nspecifite (Données de test):", specificite)
        })
      })
    })
  }
)
