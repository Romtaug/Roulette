library(ggplot2)
library(shiny)

nombre_tests <- 1000
solde_initial <- 75
mise_initiale <- 5

simule_martingale <- function(solde_initial, mise_initiale) {
  mise = mise_initiale
  resultats = c("rouge", "noir", "vert") # On suppose une roulette europÃ©enne
  probabilites = c(18/37, 18/37, 1/37)
  
  # Initialisation d'un vecteur pour enregistrer l'Ã©volution du solde
  evolution_solde <- c(solde_initial)
  
  while (solde_initial >= mise) {
    resultat_tour = sample(resultats, size = 1, prob = probabilites)
    if (resultat_tour == "rouge") {
      solde_initial = solde_initial + mise
      mise = mise_initiale
    } 
    else {
      solde_initial = solde_initial - mise
      mise = mise * 2
    }
    # Ajoute le solde actuel Ã  l'Ã©volution du solde
    evolution_solde <- c(evolution_solde, solde_initial)
  }
  
  return(list(evolution_solde = evolution_solde,
              nombre_tours = length(evolution_solde),
              montant_max = max(evolution_solde),
              montant_min = min(evolution_solde)))
}

######################################################################################################################################
#Affiche le plot
affiche_martingale <- function(solde_initial, mise_initiale){
  # ExÃ©cutez une simulation
  resultat_simulation <- simule_martingale(solde_initial, mise_initiale)
  # Utilisez la fonction `plot()` pour tracer l'Ã©volution du solde au fil des tours
  # CrÃ©e un graphique montrant l'Ã©volution du solde au fil des tours
  plot(resultat_simulation$evolution_solde, type = "l", col = "#006400", main = "Evolution du solde au fil des tours avec la technique de la martingale", xlab = "Nombre de tours", ylab = "Solde (en euros)")
}

#####################################################################################################################################
#Afficher le texte
tests_martingale <- function(nombre_tests, solde_initial, mise_initiale) {
  nombre_tours_total = 0
  montant_max_total = 0
  montant_min_total = solde_initial
  
  nombre_tours_max = 0
  nombre_tours_min = Inf
  
  # Create a vector to store the number of turns from each test
  nombre_tours = c()
  
  for (i in 1:nombre_tests) {
    resultats = simule_martingale(solde_initial, mise_initiale)
    nombre_tours_total = nombre_tours_total + resultats$nombre_tours
    montant_max_total = montant_max_total + resultats$montant_max
    montant_min_total = min(montant_min_total, resultats$montant_min)
    
    nombre_tours_max = max(nombre_tours_max, resultats$nombre_tours)
    nombre_tours_min = min(nombre_tours_min, resultats$nombre_tours)
    
    # Store the number of turns from this test
    nombre_tours <- c(nombre_tours, resultats$nombre_tours)
  }
  
  nombre_tours_moyen = round(nombre_tours_total / nombre_tests, 2)
  montant_max_moyen = round(montant_max_total / nombre_tests, 2)
  montant_min_moyen = round(montant_min_total / nombre_tests, 2)
  gain_moyen = round((montant_max_moyen + montant_min_moyen) / 2, 2)
  
  # Calculate variance, standard deviation, median, Q1 and Q3
  variance_nombre_tours = round(var(nombre_tours), 2)
  ecart_type_nombre_tours = round(sd(nombre_tours), 2)
  mediane_nombre_tours = round(median(nombre_tours), 2)
  quartile1_nombre_tours = round(quantile(nombre_tours, 0.25), 2)
  quartile3_nombre_tours = round(quantile(nombre_tours, 0.75), 2)
  
  text_to_display <- paste(
    "<ul>",
    "<li><span>Le nombre moyen de tours : ", nombre_tours_moyen, ". Le gain moyen : ", gain_moyen, " €</span></li>",
    "<li><span>Le nombre maximum de tours : ", nombre_tours_max, ". Le gain maximum : ", montant_max_moyen, " €</span></li>",
    "<li><span>Le nombre minimum de tours : ", nombre_tours_min, ". Le gain minimum : ", montant_min_moyen, " €</span></li>",
    "<li><span>Variance des tours : ", variance_nombre_tours, ". Écart-type : ", ecart_type_nombre_tours, "</span></li>",
    "<li><span>Médiane du nombre de tours : ", mediane_nombre_tours, ". Premier quartile : ", quartile1_nombre_tours, ". Troisième quartile : ", quartile3_nombre_tours, "</span></li>",
    "</ul>"
  )
  
  return(text_to_display)
}


################################################################################################################
#Test
solde_initial <- 1000
mise_initiale <- 5
nombre_tests <- 1000

#Affiche le plot
affiche_martingale(solde_initial, mise_initiale)
#Afficher le texte
tests_martingale(nombre_tests, solde_initial, mise_initiale)

###############################################################################################################
library(shiny)

# Define UI for application 
ui <- fluidPage(
  
  # Adding CSS styles
  tags$head(
    tags$link(rel = "shortcut icon", href = "roulette.png", type = "image/png"),
    tags$style(HTML("
      #go2 {
        background-color: black; 
        color: white;
      }
      #go1 {
        background-color: red;
        color: black;
      }
      #reset {
        background-color: green; 
        color: white;
      }
      .sidebar {
        background-color: gold;
      }
      .title {
        font-weight: bold;
        color: #8B4513; /* Marron foncé en code hexadécimal */
      }
      .skin-blue .main-header .logo { background-color: #3c8dbc; }
      .skin-blue .main-header .navbar { background-color: #3c8dbc; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #367fa9; }
      .skin-blue .main-sidebar { background-color: #222d32; }
      .content-wrapper, .right-side { background-color: #ffffff; }
      .skin-blue .sidebar a { color: #b8c7ce; }
      .skin-blue .sidebar-menu > li.header { color: #4b646f; background: #1a2226; }
      .skin-blue .sidebar-menu > li > a:hover { background: #1e282c; }
      .skin-blue .sidebar-menu > li.active > a { background: #1e282c; color: #fff; }
      h2 { color: #8B4513; }
      h4 { color: #8B4513; }
      .btn { background-color: #3c8dbc; color: #fff; }
      .btn:hover { background-color: #204d74; color: #fff; }
    "))
  ),
  
  absolutePanel(
    top = 10, right = 5, width = 100, height = 100,
    img(src = "roulette.png", width = "75%", height = "75%")
  ),
  
  # Application title
  titlePanel("Martingale Master"),
  
  # Welcome message
  p(tags$b("Bienvenue dans le Simulateur Martingale Master ! Ici, vous pouvez explorer l'évolution de la stratégie de Martingale.")),
  
  # Instructions
  p(
    tags$ul(
      tags$li("Pour commencer, indiquez votre solde initial et votre mise initiale."),
      tags$li("Indiquez ensuite la taille de l'échantillon pour la simulation, ce qui représente le nombre de jeux à effectuer pour chaque test."),
      tags$li("Cliquer sur le bouton 'Martingale' lance la simulation et vous pouvez observer comment votre solde change au fil du temps."),
      tags$li("Le bouton 'Statistiques' affiche des informations statistiques sur les résultats de la simulation.")
    )
  ),
  p("Pour en savoir plus sur la stratégie de Martingale, visitez la plateforme numéro 1 des casinos en ligne : ",
    tags$a(href="https://www.cresuscasino.com/fr/", "Cresus", target="_blank"),
    "."
  ),
  
  # Sidebar with input and output definitions 
  sidebarLayout(
    sidebarPanel(
      numericInput("solde_initial", "Solde initial :", 75, min=1),
      numericInput("mise_initiale", "Mise initiale :", 5, min=1),
      numericInput("nombre_tests", "Taille d'échantillon de test :", 10000, min=1),
      actionButton("go2", "Martingale"),
      actionButton("go1", "Statistiques"),
      actionButton("reset", "Réinitialiser") # Reset button
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput("plotOutput"),
      htmlOutput("textOutput"),
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$go1, {
    output$textOutput <- renderUI({
      HTML(paste("<h4 style='color:gold;'>Statistiques</h4>",
                 tests_martingale(input$nombre_tests, input$solde_initial, input$mise_initiale)))
    })
  })
  
  observeEvent(input$go2, {
    output$plotOutput <- renderPlot({
      affiche_martingale(input$solde_initial, input$mise_initiale)
    })
  })
  
  observeEvent(input$reset, {
    output$plotOutput <- renderPlot({ NULL })
    output$textOutput <- renderUI({ NULL })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


