library(shiny)

logistic1.fn <- function(len,a,b) {
  neglog19 <- -1*log(19)
  denom <- 1.+exp(neglog19*(len-a)/b)
  sel = 1/denom
  return(sel)
}

doubleNorm24.fn <- function(x,a,b,c,d,e,f) {
#NOT CORRECT
  sel <- rep(NA, length(x))
  startbin <- 1
  peak <- a
  upselex <- exp(c)
  downselex <- exp(d)
  final <- f
  if (e < -1000) {
      j1 <- -1001 - round(e)
      sel[1:j1] <- 1e-06
  }
  if (e >= -1000) {
      j1 <- startbin - 1
      if (e > -999) {
        point1 <- 1/(1 + exp(-e))
        t1min <- exp(-(x[startbin] - peak)^2/upselex)
      }
  }
  if (f < -1000)
      j2 <- -1000 - round(f)
  if (f >= -1000)
      j2 <- length(x)
  peak2 <- peak + 2 + (0.99 * x[j2] - peak - 2)/(1 +
      exp(-b))
  if (f > -999) {
      point2 <- 1/(1 + exp(-final))
      t2min <- exp(-(x[j2] - peak2)^2/downselex)
  }
  t1 <- x - peak
  t2 <- x - peak2
  join1 <- 1/(1 + exp(-(20/(1 + abs(t1))) * t1))
  join2 <- 1/(1 + exp(-(20/(1 + abs(t2))) * t2))
  if (e > -999)
      asc <- point1 + (1 - point1) * (exp(-t1^2/upselex) -
        t1min)/(1 - t1min)
  if (e <= -999)
      asc <- exp(-t1^2/upselex)
  if (f > -999)
      dsc <- 1 + (point2 - 1) * (exp(-t2^2/downselex) -
        1)/(t2min - 1)
  if (f <= -999)
      dsc <- exp(-(t2)^2/downselex)
  sel[(j1 + 1):j2] <- asc * (1 - join1) + join1 * (1 -
      join2 + dsc * join2)
  if (startbin > 1 && e >= -1000) {
      sel[1:startbin] <- (x[1:startbin]/x[startbin])^2 *
        sel[startbin]
  }
  if (j2 < length(x))
      sel[(j2 + 1):length(x)] <- sel[j2]
  return(sel)
}






# Define server logic required to plot selectivity
shinyServer(function(input, output, session) {

  observe({
    xS <- input$par2
    updateNumericInput(session, "par2N", value=xS)
  })
  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    xN <- input$par2N
    # Similar to number and text. only label and value can be set for slider
    updateSliderInput(session, "par2", value = xN)
  })

  # output$DoubleNormPars <- renderUI({
  #   val <- mean(as.numeric(input$range))
  #   sliderInput("par1", "PEAK:",
  #               as.numeric(input$range[1]), as.numeric(input$range[2]), val, 0.1)
  #   sliderInput("par2", "TOP:", -5, 5, 0, 0.1)
  #   sliderInput("par3", "ASC-WIDTH:", -5, 10, 3, 0.1)
  #   sliderInput("par4", "DESC-WIDTH:", -5, 10, 3, 0.1)
  #   sliderInput("par5", "INIT:", 0, 1, 0.1, 0.05)
  #   sliderInput("par6", "FINAL:", 0, 1, 0.9, 0.05)
  # })

  len <- reactive({
    seq(as.numeric(input$range[1]),as.numeric(input$range[2]),0.1)
  })

	#Tell it what the equation is based on user input
	selex <- reactive({
		switch(input$type,
				   "Logistic (1)" = logistic1.fn(len(),input$par1,input$par2),
		       "Double Normal (24)" = doubleNorm24.fn(len(),input$par1,input$par2,
                                                        input$par3,input$par4,
                                                        input$par5,input$par6))
	})

	output$caption <- renderText({
    input$type
  })


  output$selPlot <- renderPlot({
    plot(len(),selex(),type="l",lwd=3,xlab="Length",ylab="Selectivity",ylim=c(0,1))
  })
})