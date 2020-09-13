require ( "magrittr" )
require ( "data.table" )
require ( "animation" )
require ( "latex2exp" )

setwd ( paste0 ( "C:/Users/David D'Haese/Documents/_TEACHING/",
  "ARTIFICIAL_INTELLIGENCE/COURSE/2021/" ))

dyad <- readRDS("r/Palette_5YR_5B_Dyad.RDS")
palette(dyad[c(9, 20, 66)])

rmse <- function ( y, x ){
  sqrt ( mean (( y - x ) ^ 2 ))
}

data(mtcars)

model2 <- lm(disp ~ poly(wt, 5, raw = TRUE), dat = mtcars)
plot(disp ~ wt, xlab="Car Weight (1000 lbs)", ylab="Displacement (cu.in.)", pch=19,
  data = mtcars )
curve(2.678*x^5 -40.025*x^4+206.509*x^3-399.128*x^2 + 217.394*x + 146.905,
  add=TRUE, col = 2, lwd= 2 )

poly_rmse <- rmse(model2$fitted.values, mtcars$disp)
acc_txt <- paste0 ( "RMSE = ", sprintf ( "%.5f", poly_rmse ))
text ( 4.0, 100, acc_txt, col = 2, adj = c ( 0, .5 ), cex = 1.3 )

model3 <- lm(disp ~ poly(wt, 7, raw = TRUE), dat = mtcars)
plot(disp ~ wt, xlab="Car Weight (1000 lbs)", ylab="Displacement (cu.in.)", pch=19,
  data = mtcars )
curve(38733.02-96555.93*x+100138.32*x^2-55900.92  *x^3+18139.90*x^4-3421.07*x^5+347.44*x^6-14.68*x^7,
  add=TRUE, col = 3, lwd= 2 )

poly_rmse <- rmse(model3$fitted.values, mtcars$disp)
acc_txt <- paste0 ( "RMSE = ", sprintf ( "%.5f", poly_rmse ))
text ( 4.0, 100, acc_txt, col = 3, adj = c ( 0, .5 ), cex = 1.3 )

--------------
  
# Vizualize fit

B <- 200
M <- 200
H <- 2
X0 <- 2

params <- data.table ( B, M, H, X0, 55 ) %>%
  `colnames<-` ( c ( "B", "M", "H", "X0", "RMSE" ))

hill <- function ( x ) {
  B=x[1]
  M=x[2]
  H=x[3]
  X0=x[4]
  
  out <- rmse ( mtcars$disp,
         ( B + ( M * ( mtcars$wt - X0 ) ^ 2 ) / ( H ^ 2 + ( mtcars$wt - X0 ) ^ 2 )))
  params <<- params %>% rbind ( x %>% c ( out %>% set_names ( "RMSE" )) %>% t )
  
  return ( out )
}


optim( par = c( B=B, M=M, H=H, X0=X0 ), hill, control=list(trace=6))


saveGIF({
  for (i in seq ( 1, nrow ( params ), 5 )){
    plot(disp ~ wt, xlab="Car Weight  (1000 lbs)", ylab="Displacement (cu.in.)", pch=19,
         data = mtcars )
    
    p <- params [ i ]
    curve_num <- curve ( p$B + ( p$M * ( x - p$X0 ) ^ 2 ) / ( p$H ^ 2 + ( x - p$X0 ) ^ 2 ),
            add = TRUE, col = 1, lwd = 3 )
    
    formula_txt <- paste0 ( "$disp = ",
        sprintf ( "%.0f", p$B ), " + \\frac{",
        sprintf ( "%.2f", p$M ), "\\cdot \\left(wt - ",
        sprintf ( "%.2f", p$X0 ), "\\right)^2}{\\left(",
        sprintf ( "%.2f", p$H ), "^2 + \\left(wt - ",
        sprintf ( "%.2f", p$X0 ), "\\right)^2 \\right)}"
        )
    acc_txt <- paste0 ( "RMSE = ", sprintf ( "%.5f", p$RMSE ))
    
    text ( 1.6, 430, TeX ( formula_txt ), col = 1, adj = c ( 0, .5 ), cex = 1.3 )
    text ( 4.0, 100, acc_txt, col = 1, adj = c ( 0, .5 ), cex = 1.3 )
  }
}, "img/regressie.gif", interval = .01, ani.width = 500, ani.height = 500 )


----------------
  nieuwe data
dat <- fread("dat/auto.csv")
dat[, weight := weight/1000]
plot(displacement ~ weight, xlab="Car Weight (1000 lbs)", ylab="Displacement (cu.in.)", pch=19,
  data = dat )

dat[, wt := weight]
dat[, disp := displacement]

curve(2.678*x^5 -40.025*x^4+206.509*x^3-399.128*x^2 + 217.394*x + 146.905,
  add=TRUE, col = 2, lwd= 2 )
curve(38733.02-96555.93*x+100138.32*x^2-55900.92  *x^3+18139.90*x^4-3421.07*x^5+347.44*x^6-14.68*x^7,
  add=TRUE, col = 3, lwd= 2 )

rmsen2 <- rmse(predict(model2, newdata = dat), dat$displacement)
rmsen3 <- rmse(predict(model3, newdata = dat), dat$displacement)

acc_txt2 <- paste0 ( "RMSE = ", sprintf ( "%.5f", rmsen2 ))
text ( 4.0, 100, acc_txt2, col = 2, adj = c ( 0, .5 ), cex = 1.3 )

acc_txt3 <- paste0 ( "RMSE = ", sprintf ( "%.5f", rmsen3 ))
text ( 4.0, 80, acc_txt3, col = 3, adj = c ( 0, .5 ), cex = 1.3 )
