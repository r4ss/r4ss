library(shiny)

logistic1.fn <- function(len,a,b) {
  neglog19 <- -1*log(19)
  denom <- 1.+exp(neglog19*(len-a)/b)
  sel = 1/denom
  return(sel)
}

doubleNorm24.fn <- function(x,a,b,c,d,e,f) {
#UPDATED: - input e and f on 0 to 1 scal and transfrom to logit scale
#         - changed bin width in peak2 calculation
#         - updated index of sel when j2 < length(x)
#	  - renamed input parameters, cannot have same names as the logitstic function
#         - function not handling f < -1000 correctly
  if(e == 0) {#Avoid errors on the bounds
    e <- 1-0.999955 #an input that results in approx -10
  } 
  if(e == 1) {
      e <- 0.999955  #an input that results in approx 10
  }
  e <- log(e/(1-e)) #transform input to logit
  
  if(f == 0) {#Avoid errors on the bounds
    f <- 1-0.999955 #an input that results in approx -10
  } 
  if(f == 1) {
      f <- 0.999955  #an input that results in approx 10
  }
  f <- log(f/(1-f)) #transform input to logit

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
  bin_width <- x[2] - x[1]
  peak2 <- peak + bin_width + (0.99 * x[j2] - peak - bin_width)/(1 +
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
  idx.seq <- (j1 + 1):j2
  sel[idx.seq] <- asc[idx.seq] * (1 - join1[idx.seq]) + join1[idx.seq] * (1 -
      join2[idx.seq] + dsc[idx.seq] * join2[idx.seq])
  if (startbin > 1 && e >= -1000) {
      sel[1:startbin] <- (x[1:startbin]/x[startbin])^2 *
        sel[startbin]
  }
  if (j2 < length(x))
      sel[(j2 + 1):length(x)] <- sel[j2]
  return(sel)
}






# Define server logic required to plot selectivity
server <- function(input, output, session) {

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

 #Input for e and f: numeric input on logit scale, slider input on real scale
  observe({
    #x.eS is the actual value from the slider input and will be dispalyed in the number input
    x.eS <- input$par.e   
    updateNumericInput(session, "par.eN", value=x.eS)
  })
  observe({
    #x.eN is the actual value from the number input and will replace the slider input
    x.eN <- input$par.eN
    updateSliderInput(session, "par.e", value = x.eN)
  })
  
  observe({
    #x.fS is the actual value from the slider input and will be dispalyed in the number input
    x.fS <- input$par.f   
    updateNumericInput(session, "par.fN", value=x.fS)
  })
  observe({
    #x.fN is the actual value from the number input and will replace the slider input
    x.fN <- input$par.fN
    updateSliderInput(session, "par.f", value = x.fN)
  })
  
  len <- reactive({
    seq(as.numeric(input$range[1]),as.numeric(input$range[2]),0.1)
  })

	#Tell it what the equation is based on user input
	selex <- reactive({
		switch(input$type,
				   "Logistic (1)" = logistic1.fn(len(),input$par1,input$par2),
		       "Double Normal (24)" = doubleNorm24.fn(len(),input$par.a,input$par.b,
                                                        input$par.c,input$par.d,
                                                        input$par.e,input$par.f))
	})

	output$caption <- renderText({
    input$type
  })


  output$selPlot <- renderPlot({
    plot(len(),selex(),type="l",lwd=3,xlab="Length",ylab="Selectivity",ylim=c(0,1))
  })
}
