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
    updateNumericInput(session, "par2N", value=input$par2)
  })
  observe({
    updateSliderInput(session, "par2", value = input$par2N)
  })
  observe({
    updateNumericInput(session, "par1N", value=input$par1)
  })
  observe({
    updateSliderInput(session, "par1", value = input$par1N)
  })

 #Input for double normal parameters: all parameters on the scale the user enters in SS
  observe({
    updateNumericInput(session, "par.aN", value=input$par.a)
  })
  observe({
    updateSliderInput(session, "par.a", value = input$par.aN)
  })
  observe({
    updateNumericInput(session, "par.bN", value=input$par.b)
  })
  observe({
    updateSliderInput(session, "par.b", value = input$par.bN)
  })
  observe({
    updateNumericInput(session, "par.cN", value=input$par.c)
  })
  observe({
    updateSliderInput(session, "par.c", value = input$par.cN)
  })
  observe({
    updateNumericInput(session, "par.dN", value=input$par.d)
  })
  observe({
    updateSliderInput(session, "par.d", value = input$par.dN)
  })
  observe({
    updateNumericInput(session, "par.eN", value=input$par.e)
  })
  observe({
    updateSliderInput(session, "par.e", value = input$par.eN)
  })
  observe({
    updateNumericInput(session, "par.fN", value=input$par.f)
  })
  observe({
    updateSliderInput(session, "par.f", value = input$par.fN)
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
