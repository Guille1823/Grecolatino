#' Cuadrado Grecolatino
#'
#' Realiza un cuadrado grecolatino
#'
#'
#' @param respuesta (string) nombre de la variable respuesta
#' @param tratamiento nombre de la variable tratamiento
#' @param fila nombre de la variable fila
#' @param columna nombre de la variable columna
#' @param l_griega nombre de la variable l_griega
#' @param data (\code{data.frame}) Tabla de datos en formato largo con los datos
#'    del tratamiento, filas, columnas, letras griegas y respuesta.
#'
#' @return Devuelve una tabla en formato \code{data.frame} con los cálculos
#'  correspondientes al análisis de varianza.
#' @export
CGRL<- function(respuesta, tratamiento, fila, columna, l_griega, data){

  #y^2/k^2
  sumatotaly<- sum(y)
  s<-sumatotaly^2/k^2

  #TRATAMIENTO
  gl_tto<- k-1
  sum_tto<-tapply(y, INDEX = TRATAMIENTO, FUN= sum)
  n_tto<- tapply(y, INDEX = TRATAMIENTO, FUN = length)
  sumc_tto<-sum(sum_tto^2/n_tto)-s
  cmedio_tto<-sumc_tto/gl_tto
  f_tto<- cmedio_tto/cmedio_error
  p_value_tto<-pf(f_tto, gl_tto, gl_error, lower.tail = FALSE)

  #FILA
  gl_fila<- k-1
  sum_fila<- tapply(y, INDEX = FILA, FUN = sum)
  n_fila<- tapply(y, INDEX = FILA, FUN = length)
  sumc_fila<- sum(sum_fila^2/ n_fila) - s
  cmedio_fila<-sumc_fila/gl_fila
  f_fila<- cmedio_fila/cmedio_error
  p_value_fila<-pf(f_fila, gl_fila, gl_error, lower.tail = FALSE)

  #COLUMNA
  gl_columna<- k-1
  sum_columna<-tapply(y, INDEX = COLUMNA, FUN = sum)
  n_columna<-tapply(y, INDEX = COLUMNA, FUN = length)
  sumc_columna<-sum(sum_columna^2/n_columna) - s
  cmedio_columna<-sumc_columna/ gl_columna
  f_columna<-cmedio_columna/cmedio_error
  p_value_columa<- pf(f_columna, gl_columna, gl_error, lower.tail = FALSE)

  #L_GRIEGAS
  gl_lgriegas<- k-1
  sum_lgriegas<-tapply(y, INDEX = LGRIEGAS, FUN = sum)
  n_lgriegas<-tapply(y, INDEX = LGRIEGAS, FUN = length)
  sumc_lgriegas<-sum(sum_lgriegas^2/n_lgriegas)-s
  cmedio_lgriegas<-sumc_lgriegas/gl_lgriegas
  f_lgriegas<-cmedio_lgriegas/cmedio_error
  p_value_lgriegas<-pf(f_lgriegas, gl_lgriegas, gl_error, lower.tail = FALSE)

  #ERROR
  gl_error<-(k-1)*(k-3)
  sumc_error<-sumc_total-sumc_tto-sumc_fila- sumc_columna-sumc_lgriegas
  cmedio_error<-sumc_error/gl_error

  #TOTAL
  gl_total<- k^2-1
  sumc_total<- sum(y^2)-s

  #Creamos un dataframe
  resultado<- data.frame(
    Fuente=c("Factor principal", "Factor fila bloqueo", "Factor Columna bloqueo", "Factor L.griega bloqueo", "Error", "Total"),
    G_de_l=c(gl_tto, gl_fila, gl_columna, gl_lgriegas, gl_error, gl_total),
    Suma_de_cuadrados=c(sumc_tto, sumc_fila, sumc_columna, sumc_lgriegas, sumc_error,sumc_total),
    Cuadrado_medio=c(cmedio_tto, cmedio_fila, cmedio_columna, cmedio_lgriegas, cmedio_error, NA),
    F=c(f_tto, f_fila, f_columna, f_lgriegas, NA, NA),
    Significancia=c(p_value_tto, p_value_fila, p_value_columa, p_value_lgriegas, NA, NA),
    check.names = FALSE
  )
  rownames(tabla) <- NULL
  anava <- tabla

  return(resultado)
}
