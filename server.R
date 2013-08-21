library(shiny)

# Some plotting functions used below
plotMu <- function(mu){
	      	arrows(mu, 99, mu, -99,length=0.0,lwd=5,col="red")
      		mtext(expression(mu),side=3,at=mu,cex=3,col="red")
      		}
plot1SD_pmf <- function(mu,sigma,max_height){
      		arrows(mu, max_height/2, mu+sigma, max_height/2,length=0.15,lwd=5,col="black",code=3)
      		arrows(mu-sigma, max_height/2, mu, max_height/2,length=0.15,lwd=5,col="black",code=3)
      		mtext(expression(sigma),side=1,at=mu+sigma/2,cex=3,col="black",padj=1)
      		mtext(expression(sigma),side=1,at=mu-sigma/2,cex=3,col="black",padj=1)
      		}
plot2SD_pmf <- function(mu,sigma,max_height){
      		arrows(mu, max_height/2, mu+2*sigma, max_height/2,length=0.15,lwd=3,col="orange",code=3)
      		arrows(mu-2*sigma, max_height/2, mu, max_height/2,length=0.15,lwd=3,col="orange",code=3)
      		mtext(expression(2*sigma),side=1,at=mu+1.2*sigma,cex=3,col="orange",padj=1)
      		mtext(expression(2*sigma),side=1,at=mu-1.2*sigma,cex=3,col="orange",padj=1)
      		}
plot1SD_cdf <- function(mu,sigma){
      		arrows(mu, 0.5, mu+sigma, 0.5,length=0.15,lwd=5,col="black",code=3)
      		arrows(mu-sigma, 0.5, mu, 0.5,length=0.15,lwd=5,col="black",code=3)
      		mtext(expression(sigma),side=1,at=mu+sigma/2,cex=3,col="black",padj=1)
      		mtext(expression(sigma),side=1,at=mu-sigma/2,cex=3,col="black",padj=1)
      		}
plot2SD_cdf <- function(mu,sigma){
      		arrows(mu, 0.5, mu+2*sigma, 0.5,length=0.15,lwd=3,col="orange",code=3)
      		arrows(mu-2*sigma, 0.5, mu, 0.5,length=0.15,lwd=3,col="orange",code=3)
      		mtext(expression(2*sigma),side=1,at=mu+1.2*sigma,cex=3,col="orange",padj=1)
      		mtext(expression(2*sigma),side=1,at=mu-1.2*sigma,cex=3,col="orange",padj=1)
      		}

shinyServer(function(input,output){

# Probability Density Function Plots	
  output$pdf_plot <- renderPlot({
  	if (input$typeDist == "uniform") {	
      x <- 0:20 # dummy inputs
      y <- rep(-99,length(x)) # dummy inputs
      max_height <- 1/(input$theta2-input$theta1)
      plot(x,y,type="n",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(input$min_y_unif,input$max_y_unif),ylim=c(0,1.04*max_height))
      lines(c(input$theta1,input$theta2),c(1/(input$theta2-input$theta1),1/(input$theta2-input$theta1)),lwd=3,col="blue")
      points(input$theta1,1/(input$theta2-input$theta1),pch=19,col="blue")
      points(input$theta2,1/(input$theta2-input$theta1),pch=19,col="blue")
      lines(c(input$min_y_unif-1,input$theta1),c(0,0),lwd=3,col="blue")
      lines(c(input$theta2,input$max_y_unif+1),c(0,0),lwd=3,col="blue")
      
      mu <- (input$theta1+input$theta2)/2
      sigma <- sqrt((input$theta2-input$theta1)^2/12)
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu) 
      
      mtext(bquote(U(theta[1]==.(input$theta1),theta[2]==.(input$theta2))),,side=3,padj=0,cex=2,at=(input$min_y_unif+input$max_y_unif)/2)   
      }  
      
    if (input$typeDist == "normal") {	  
      x <- seq(input$min_y_normal,input$max_y_normal,0.01)
      y <- dnorm(x,input$mean,input$sd)
      max_height <- dnorm(input$mean,input$mean,input$sd)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(input$min_y_normal,input$max_y_normal),ylim=c(0,1.04*max_height),lwd=3,col="blue")
      
      mu <- input$mean
      sigma <- input$sd
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu)    
      
      mtext(bquote(N(mu==.(input$mean),sigma^2==.(input$sd^2))),,side=3,padj=0,cex=2,at=(input$min_y_normal+input$max_y_normal)/2)   
      }   
      
    if (input$typeDist == "gamma") {	
      x <- seq(0,input$max_y_gamma,0.01)
      y <- dgamma(x,input$alpha_gamma,1/input$beta_gamma)
      max_height <- max(dgamma(x,input$alpha_gamma,1/input$beta_gamma))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_gamma),ylim=c(0,1.04*max_height),lwd=3,col="blue")
      
      mu <- input$alpha_gamma*input$beta_gamma
      sigma <- sqrt(input$alpha_gamma*input$beta_gamma^2)
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu)    
      
      mtext(bquote(G(alpha==.(input$alpha_gamma),beta==.(input$beta_gamma))),side=3,padj=0,cex=2,at=input$max_y_gamma/2)         
      }   
         
    if (input$typeDist == "chisq") {	
      x <- seq(0,input$max_y_chisq,0.01)
      y <- dchisq(x,input$df)
      if(input$df==1){max_height <- 4}
      if(input$df>1){max_height <- max(dchisq(x,input$df))}
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_chisq),ylim=c(0,1.04*max_height),lwd=3,col="blue")     
            
      mu <- input$df
      sigma <- sqrt(2*input$df)
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu)    
      
      mtext(bquote(chi^2 ~ (nu==.(input$df))),side=3,padj=0,cex=2,at=input$max_y_chisq/2)        
      }
                
    if (input$typeDist == "exponential") {	  
      x <- seq(0,input$max_y_exp,0.01)
      y <- dexp(x,1/input$beta_exp)
      max_height <- max(dexp(x,1/input$beta_exp))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_exp),ylim=c(0,1.04*max_height),lwd=3,col="blue")
            
      mu <- input$beta_exp
      sigma <- input$beta_exp
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu)    

      mtext(bquote(Exp ~ (beta==.(input$beta_exp))),side=3,padj=0,cex=2,at=input$max_y_exp/2)      
      }   
      
    if (input$typeDist == "beta") {	  
      x <- seq(0,1,0.01)
      y <- dbeta(x,input$alpha_beta,input$beta_beta)
      max_height <- max(dbeta(x,input$alpha_beta,input$beta_beta))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,1),ylim=c(0,1.04*max_height),lwd=3,col="blue")
            
      mu <- input$alpha_beta/(input$alpha_beta+input$beta_beta)
      sigma <- sqrt((input$alpha_beta*input$alpha_beta)/((input$alpha_beta+input$beta_beta)^2*(input$alpha_beta+input$beta_beta+1)))
      if(input$show1SD==TRUE) plot1SD_pmf(mu,sigma,max_height)
      if(input$show2SD==TRUE) plot2SD_pmf(mu,sigma,max_height)
      if(input$showMean==TRUE) plotMu(mu)    
      
      mtext(bquote(B(alpha==.(input$alpha_beta),beta==.(input$beta_beta))),side=3,padj=0,cex=2,at=1/2)         
      }   
  })
  
# Cumulative Distribution Function Plots	
  output$cdf_plot <- renderPlot({
    if (input$typeDist == "uniform") {	
      x <- 0:20
      y <- rep(-99,length(x))
      plot(x,y,type="n",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(input$min_y_unif,input$max_y_unif),ylim=c(0,1))
      lines(c(input$theta1,input$theta2),c(0,1),lwd=3,col="blue")
      lines(c(input$min_y_unif-1,input$theta1),c(0,0),lwd=3,col="blue")
      lines(c(input$theta2,input$max_y_unif+1),c(1,1),lwd=3,col="blue")
            
      mu <- (input$theta1+input$theta2)/2
      sigma <- sqrt((input$theta2-input$theta1)^2/12)
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)  
      
      mtext(bquote(U(theta[1]==.(input$theta1),theta[2]==.(input$theta2))),,side=3,padj=0,cex=2,at=(input$min_y_unif+input$max_y_unif)/2)     
      }   
      
    if (input$typeDist == "normal") {	  
      x <- seq(input$min_y_normal,input$max_y_normal,0.01)
      y <- pnorm(x,input$mean,input$sd)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(input$min_y_normal,input$max_y_normal),ylim=c(0,1),lwd=3,col="blue")
      
      mu <- input$mean
      sigma <- input$sd
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)    
      
      mtext(bquote(N(mu==.(input$mean),sigma^2==.(input$sd^2))),,side=3,padj=0,cex=2,at=(input$min_y_normal+input$max_y_normal)/2)         
      }   
      
    if (input$typeDist == "gamma") {	  
      x <- seq(0,input$max_y_gamma,0.01)
      y <- pgamma(x,input$alpha_gamma,1/input$beta_gamma)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(0,input$max_y_gamma),ylim=c(0,1),lwd=3,col="blue")
      
      mu <- input$alpha_gamma*input$beta_gamma
      sigma <- sqrt(input$alpha_gamma*input$beta_gamma^2)
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)  
      
      mtext(bquote(G(alpha==.(input$alpha_gamma),beta==.(input$beta_gamma))),side=3,padj=0,cex=2,at=input$max_y_gamma/2)        
      }   
         
    if (input$typeDist == "chisq") {	 
      x <- seq(0,input$max_y_chisq,0.01)
      y <- pchisq(x,input$df)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(0,input$max_y_chisq),ylim=c(0,1),lwd=3,col="blue")
                  
      mu <- input$df
      sigma <- sqrt(2*input$df)
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)  
      
      mtext(bquote(chi^2 ~ (nu==.(input$df))),side=3,padj=0,cex=2,at=input$max_y_chisq/2)     
      }  
                
    if (input$typeDist == "exponential") {	  
      x <- seq(0,input$max_y_exp,0.01)
      y <- pexp(x,1/input$beta_exp)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(0,input$max_y_exp),ylim=c(0,1),lwd=3,col="blue")
                  
      mu <- input$beta_exp
      sigma <- input$beta_exp
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu) 
      
      mtext(bquote(Exp ~ (beta==.(input$beta_exp))),side=3,padj=0,cex=2,at=input$max_y_exp/2)        
      }   
       
    if (input$typeDist == "beta") {	  
      x <- seq(0,1,0.01)
      y <- pbeta(x,input$alpha_beta,input$beta_beta)
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(F(y)),xlim=c(0,1),ylim=c(0,1),lwd=3,col="blue")
                  
      mu <- input$alpha_beta/(input$alpha_beta+input$beta_beta)
      sigma <- sqrt((input$alpha_beta*input$beta_beta)/((input$alpha_beta+input$beta_beta)^2*(input$alpha_beta+input$beta_beta+1)))
      if(input$show1SD==TRUE) plot1SD_cdf(mu,sigma)
      if(input$show2SD==TRUE) plot2SD_cdf(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)
      }  
  })
  
})


