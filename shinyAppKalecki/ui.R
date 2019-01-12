library(shiny)
library(shinyjs)

# make sure it is same as in server.R
y0 <- 10 # Produtividade
a0 <- 20 # Gasto autonomo
w0 <- 8  # Salario
p0 <- 1  # Nível de Preços
L0 <- a0 / (y0 - (w0 / p0))     # Eq. 5.17 :: Nivel de Emprego
q0 <- y0 * L0                   # nivel do produto
W0 <- w0/p0 * L0                # Nivel Salarial (Massa salarial)

AD0 <- w0 * L0 + a0 * p0;     # Demanda Agregada do Cenário Inicial (Cen 00)
AS0 <- p0 * L0 * y0;          # Oferta Agregada do Cenário Inicial (Cen 00)
P0 <- q0 - W0                 #   ou Profit = q - W (Prduto total - Massa Salarial)
theta0 <- p0 * (y0 / w0) - 1   # theta := taxa do markup


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(),  # Include shinyjs

    # Application title
    titlePanel("Análise Gráfica do Modelo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("",width = 3,
                     # inputs
                     sliderInput("a", actionButton("res_a", "Gasto Autônomo"),
                                 min = round(a0/2), max = round(a0*1.5), value = a0),
                     sliderInput("y", actionButton("res_y", "Produtividade"),
                                 min = round(y0/1.2), max = round(y0*1.2), value = y0, step = 0.1),
                     sliderInput("w", actionButton("res_w", "Salário"),
                                  min = round(w0/1.2), max = round(w0*1.2), value = w0, step = 0.05),
                     sliderInput("p", actionButton("res_p", "Preço"),
                                 min = p0*0.9, max = p0*1.1, value = p0, step = 0.005)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # h1("First level title"),
            plotOutput("outputPlot", height=500),
            h3("Resultados"),


            tableOutput("resultsTable")

        )
    )
))
