
head(X)


library(ggExtra)
library(ggplot2)
library(gridExtra)

X2 = X[X$Tp <200 & X$Ts < 200, ]
ggMarginal(data = X2, x = 'Tp', y = 'Ts') + facet_wrap(~MAE)


P = ggplot(X2, aes(x = Tp, y = Ts)) + geom_point()
PP = ggMarginal(P, type = 'histogram')

gridExtra::grid.arrange(PP,PP)

LIST = sort(unique(X2$MAE))

L = lapply(LIST, function(x){
  D = X2[X2$MAE == x, ]
  
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
  
  AVA = percent(sum(D$Ts < D$Tp)/nrow(D),0)
  NOAVA = percent(sum(D$Ts >= D$Tp)/nrow(D),0)

  P = ggplot(D, aes(x = Tp, y = Ts)) + geom_point() + ggtitle(x) + 
    geom_vline(xintercept = 50) +
    geom_hline(yintercept = 50) + geom_density2d() +
    xlim(0,200) + 
    ylim(0,200)
  Shades = data.frame(x = c(0,0,200,0,200,200), y = c(0,200,200,0,0,200), g = c('na','na','na','a','a','a'))
  
  P = P +
    geom_polygon(data= Shades, aes(x=x, y=y,  fill = g), alpha = 0.06) +
    theme(legend.position = 'none') + scale_fill_manual(values = c( "green", "blue")) +
    annotate('text', x = -Inf, y = Inf ,label = NOAVA, vjust = 2, hjust = -1)+
    annotate('text', x = Inf, y = -Inf ,label = AVA,  vjust = -2, hjust = 1.5)
  
  P
    
  PP = ggMarginal(P, type = 'histogram')
  PP
})

do.call(grid.arrange, c(L, ncol = 7))


