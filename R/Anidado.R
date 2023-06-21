#' Diseño Anidado de factores fijos
#'
#' @param y (vector) datos de la variable respuesta
#' @param ff (vector) datos del factor principal, que anida a otros
#' @param fa (vector) datos del factor anidado dentro del factor principal
#' @param data dataframe con los valores solicitados por los factores
#'
#' @return devuelve una tabla de analisis de varianza para el diseño anidado con factores fijos
#' @export
anidado <- function(y, ff, fa, data){

  #Definir la variable respuesta

  y <- df$respuesta
  ffA <- factor(df$ff)
  faB<- factor(df$fa)
  a <- nlevels(ffA)
  b <- nlevels(faB)
  n <- length(y)

  ybarra <- mean(y)

  #Para las sumas de cuadrados
  sct <- sum(((y)^2)-(((y)^2)/(a*b*n)))
  scff <- (1/(b*n))*sum((y^2)-((ybarra^2)/(a*b*n)))


  #Para SCT
  sct <- ((sum(y-ybarra^2)) - ybarra)^2
  glt <- a*b*n-1
  cmt <- sct / glt

  #Para ff
  sff <- sum(ff)
  nff <- length(ff)
  scff <- sum((b*length(nff))*((y-ybarra)^2))
  glff <- a-1
  cmff<- scff / glff

  #Para fa
  sfa <- sum(fa)
  nfa <- length(fa)
  scfa <- sum((1/length(n)*((y^2)-(1/b*length(n)*(y^2)))))
  glfa <- a*(b-1)
  cmfa<- (scfa/glfa)

  #Para el error
  sce <- mean((y-(1/n)*(ybarra)^2))
  gle <- (a*b)*(n-1)
  cme<- (sce / gle)

  # Valores F
  Fff <- cmff / cmfa
  Ffa <- cmfa / cme

# P-valor
pv_ff <- pf(Fff, glff, gle, lower.tail = FALSE)
pv_fa <- pf(Ffa, glfa, gle, lower.tail = FALSE)

# dataframe
tabla <- data.frame(F = c("FFijo", "FAnidado", "Error", "Total"),
                    SC = c(sff, sfa, sce, sct),
                    GL = c(glff, glfa, gle, glt),
                    CM = c(cmff, cmfa, cme, NA),
                    F = c(Fff, Ffa, NA, NA),
                    `Pr(>F)` = c(pv_ff, pv_fa, NA, NA),
                    check.names = FALSE)

rownames(tabla) <- NULL
anova <- format(tabla)
anova[is.na(tabla)] <- ""

return(anova)
}

