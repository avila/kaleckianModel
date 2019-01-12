library(shiny)

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  inputVar <- reactive({

    y = input$y            # Produtividade
    a = input$a            # Gasto autonomo
    p = input$p            # Nível de Preços
    w = input$w            # salario

    L <- a / (y - (w / p))     # Eq. 5.17 :: Nivel de Emprego
    q <- y * L                 # nivel do produto
    #w <- y - (a / L)           # Salario Real na situação inicial
    theta <- p * (y / w) - 1   # theta := taxa do markup
    W <- w/p * L               # Nivel Salarial (Massa salarial)

    # P <- W * theta             # Profit = Wages * Mark Up,
    P <- q - W                 #   ou Profit = q - W (Prduto total - Massa Salarial)

    w3 <- y - (a / L0) # w/p no pleno emprego (cenario 3)

    AD <- w * L + a * p;  # Demanda Agregada do Cenário 1
    AS <- p * L * y;      # Oferta Agregada do Cenário 1



    list(
      y = y,           # Produtividade
      a = a,           # Gasto autonomo
      p = p,           # Nível de Preços
      w = w,           # salario
      L = L,
      q = q,
      theta = theta,
      W = W,
      P = P,
      AS=AS,
      AD=AD
    )
  })

  # events ----
  observeEvent(input$res_a, reset("a"))
  observeEvent(input$res_y, reset("y"))
  observeEvent(input$res_w, reset("w"))
  observeEvent(input$res_p, reset("p"))

  ## PLOT
  output$outputPlot <- renderPlot( {

    d <- inputVar()
    #di <- isolate(inputVar())

    y <- d$y  # Produtividade
    a <- d$a  # Gasto autonomo
    p <- d$p  # Nível de Preços
    w <- d$w  # salario

    L <- d$L

    # L <<- a / (y - (w / p))     # Eq. 5.17 :: Nivel de Emprego
    # q <- y * L                 # nivel do produto
    # #w <- y - (a / L)           # Salario Real na situação inicial
    # theta <- p * (y / w) - 1   # theta := taxa do markup
    # W <<- w/p * L               # Nivel Salarial (Massa salarial)
    # P <<- W * theta             # Profit = Wages * Mark Up,
    # P <<- q - W                 #   ou Profit = q - W (Prduto total - Massa Salarial)

    # w3 <- y - (a / L0) # w/p no pleno emprego (cenario 3)


    ylimpct <- 1.66
    ylim <- c(0,y0) * ylimpct
    xlim <- c(0, L0) * ylimpct
    par(mar = c(2,2,0,-2.2) +2.2)
    curve(y0 - (a0 / L0), from = 0, to = 30,
          ylim = ylim,
          xlim = xlim,
          xname = "L0",
          main = "Demanda Efetiva por Trabalho",
          xlab="Emprego (L)",
          ylab="salário real (w/p) | produtividade (y)",
          lty=3)

    # Labels e linhas Adicionais
    abline(h=0) # linha do eixo L

    abline(v = L0, lty=2, col = "gray")    # linha do full employment incial

    # EMPREGO
    text(x = L0*1.05, y = .5, labels = expression("L"[0]^"D"), cex = 1, col="gray")
    text(x = L0 *0.5, y = 0.9*y0 - (a0 / (L0*0.5)), col = "gray",
         labels = expression(paste("L"^"D")["eff"]))


    # SALARIO
    lines(x = c(par('usr')[1], L0), y = c(w0/p0,w0/p0), lty = 3, col = "gray")    # linha do salario
    text(x = 0, y = w0 / p0 - 1/2, labels = expression("w/p"[0]), cex = 1, col="gray")

    # PRODUTIVIDADE
    abline(h = y0, lwd=1, lty=2, col = "gray")
    text(x = 0, y = y0 * 1.05, labels = expression(y[0]), cex = 1, col = "gray")

    # Plot da Demanda Efeitiva por Trabalho: cenário 02 ############################

    par(new=T)                                                                     #
    curve(y - (a / L), from = 0,
          ylim = ylim,
          xlim = xlim,
          xname = "L", ylab ="", lty=1, lwd=2, xlab = "", col = "red")
    ################################################################################

    # PRODUTIVIDADE
    abline(h = y, lty=2)
    text(x = 0, y = y*1.05, labels = expression(y[1]), cex = 1)


    # SALARIO
    lines(x = c(par('usr')[1], L), y = c(w/p,w/p), lty = 2)    # linha do salario
    text(x = 0, y = w / p - 1/2, labels = expression("w/p"[1]), cex = 1)

    # EMPREGO
    abline(v = L, lwd = 3)    # linha do full employment
    text(x = L*1.05, y = 1.5-1, labels = expression("L"[1]^"D"), cex = 1)
    text(x = L *0.5, y = 0.9*y - (a / (L*0.5)), col = "red",
         labels = expression(paste("L"^"D")["eff"]))

  })

  ## RESULTS
  output$resultsTable <- renderTable(
    rownames = T,hover = T,digits = 1,
    {
      d <- inputVar()

      attach(d)
      on.exit(detach(d))

      P <- q - W                 #   ou Profit = q - W (Prduto total - Massa Salarial)



      Cen00 <- c(y      = y0,
                 L      = L0,
                 p      = p0,
                 AS     = AS0,
                 AD     = AD0,
                 W      = w0*L0,
                 P      = P0,
                 theta  = theta0*100,
                 ExOfer = AS0-AD0)

      Cen01 <- c(y      = y,
                 L      = L,
                 p      = p,
                 AS     = AS,
                 AD     = AD,
                 W      = w*L,
                 P      = P,
                 theta  = theta*100,
                 ExOfer = AS-AD)
      Diff <- ((Cen01 - Cen00) / Cen00) * 100

      rbind(
        "Cen00"   = Cen00,
        "Cen01"   = Cen01,
        "Dif(%)"  = Diff
      )
    })
})
