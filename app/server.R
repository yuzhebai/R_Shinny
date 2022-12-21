shinyServer(function(input, output) {

  output$main_plot <- renderPlot({
    
    input$refreshButton
    
    library(grid)
    library(gridExtra)
    library(sf)
    incept <- function(p1,p2,p3,p4) {
      f_line <- st_linestring(rbind(p1, p2))
      s_line <- st_linestring(rbind(p3, p4))
      st_intersection(s_line, f_line)
    }
    
    draw_outline <- function(s){
      T = c(0.5,1)
      B = c(0.5,0)
      R = c(1,0.5)
      L = c(0,0.5)
      t = c(0.5,0.5+s)
      b = c(0.5,0.5-s)
      r = c(0.5+s,0.5)
      l = c(0.5-s,0.5)
      
      incpt <- incept(T,l,L,t)
      l1 <- segmentsGrob(x0 = T[1], y0 = T[2], x1 = incpt[1], y1 = incpt[2]) 
      l2 <- segmentsGrob(x0 = L[1], y0 = L[2], x1 = incpt[1], y1 = incpt[2]) 
      
      incpt <- incept(L,b,B,l)
      l3 <- segmentsGrob(x0 = L[1], y0 = L[2], x1 = incpt[1], y1 = incpt[2]) 
      l4 <- segmentsGrob(x0 = B[1], y0 = B[2], x1 = incpt[1], y1 = incpt[2]) 
      
      incpt <- incept(B,r,R,b)
      l5 <- segmentsGrob(x0 = B[1], y0 = B[2], x1 = incpt[1], y1 = incpt[2]) 
      l6 <- segmentsGrob(x0 = R[1], y0 = R[2], x1 = incpt[1], y1 = incpt[2]) 
      
      
      incpt <- incept(T,r,R,t)
      l7 <- segmentsGrob(x0 = T[1], y0 = T[2], x1 = incpt[1], y1 = incpt[2]) 
      l8 <- segmentsGrob(x0 = R[1], y0 = R[2], x1 = incpt[1], y1 = incpt[2]) 
      
      l9 <- segmentsGrob(0,0.5,1,0.5)
      l10 <- segmentsGrob(0.5,0,0.5,1)
      g <- gTree(children = gList(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10),gp = gpar(col = "gray", lty = 3))
      grid.draw(g) 
    }
    
    
    draw <- function(w1,x1,y1,z1){
      s = 0.15
      T = c(0.5,1)
      B = c(0.5,0)
      R = c(1,0.5)
      L = c(0,0.5)
      t = c(0.5,0.5+s)
      b = c(0.5,0.5-s)
      r = c(0.5+s,0.5)
      l = c(0.5-s,0.5)
      angle = atan((incept(T,l,L,t)[2]-0.5)/0.5)
      convert <- function(w1,x1,y1,z1){
        c(0.5+w1*0.5, 0.5-x1*0.5, 0.5-y1*0.5, 0.5+z1*0.5)
      }
      
      w <- convert(w1,x1,y1,z1)[1]
      x <- convert(x1,x1,y1,z1)[2]
      y <- convert(w1,x1,y1,z1)[3]
      z <- convert(w1,x1,y1,z1)[4] 
      
      p1 <- c(0.5,w)
      p2 <- c(x,0.5)
      p3 <- c(0.5,y)
      p4 <- c(z,0.5)
      
      p1_1 <- c(0.5-sin(angle),w-cos(angle))
      p2_1 <- c(x+cos(angle),0.5+sin(angle))
      incpt <- incept(p1,p1_1,p2,p2_1)
      l1 <- segmentsGrob(x0 = p1[1], y0 = p1[2], x1 = incpt[1], y1 = incpt[2]) 
      l2 <- segmentsGrob(x0 = p2[1], y0 = p2[2], x1 = incpt[1], y1 = incpt[2]) 
      
      p2_2 <- c(x+cos(angle),0.5-sin(angle))
      p3_1 <- c(0.5-sin(angle),y+cos(angle))
      incpt <- incept(p2,p2_2,p3,p3_1)
      l3 <- segmentsGrob(x0 = p2[1], y0 = p2[2], x1 = incpt[1], y1 = incpt[2]) 
      l4 <- segmentsGrob(x0 = p3[1], y0 = p3[2], x1 = incpt[1], y1 = incpt[2]) 
      
      p3_2 <- c(0.5+sin(angle),y+cos(angle))
      p4_1 <- c(z-cos(angle),0.5-sin(angle))
      incpt <- incept(p3,p3_2,p4,p4_1)
      l5 <- segmentsGrob(x0 = p3[1], y0 = p3[2], x1 = incpt[1], y1 = incpt[2]) 
      l6 <- segmentsGrob(x0 = p4[1], y0 = p4[2], x1 = incpt[1], y1 = incpt[2]) 
      
      p4_2 <- c(z-cos(angle),0.5+sin(angle))
      p1_2 <- c(0.5+sin(angle),w-cos(angle))
      incpt <- incept(p1,p1_2,p4,p4_2)
      l7 <- segmentsGrob(x0 = p1[1], y0 = p1[2], x1 = incpt[1], y1 = incpt[2]) 
      l8 <- segmentsGrob(x0 = p4[1], y0 = p4[2], x1 = incpt[1], y1 = incpt[2]) 
      
      g <- gTree(children = gList(l1,l2,l3,l4,l5,l6,l7,l8))
      grid.draw(g)
      draw_outline(0.15)
    }


    grid.newpage()
    
    draw(input$w,input$x,input$y,input$z)
    grid.text("Accuracy",x = 0.2,y=0.8, rot = 45, gp = gpar(col = "gray"))
    grid.text("Sentivity",x = 0.8,y=0.8,rot = -45, gp = gpar(col = "gray"))
    grid.text("Area under the curve",x = 0.2,y=0.2,rot = -45, gp = gpar(col = "gray"))
    grid.text("Specificity",x = 0.8,y=0.2,rot = 45, gp = gpar(col = "gray"))
  })
})
