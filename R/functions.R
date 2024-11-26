#' Цензурирование данных о выживаемости
#'
#' Функция выполняет цензурирование данных о выживаемости на основе заданного времени отсечки.
#'
#' @param date_end Дата окончания наблюдения или события (в формате Date).
#' @param date_start Дата начала наблюдения (в формате Date).
#' @param status Статус события (0 - цензурировано, 1 - событие произошло).
#' @param cens_time Единица времени для цензурирования (число, например, 1 для дней, 30 для месяцев).
#' @param cutoff_time Время отсечки для цензурирования (число).
#' @param data Датафрейм, содержащий исходные данные.
#'
#' @return Датафрейм с добавленными колонками: `os_status_<cutoff_time>`, `os_time_<cutoff_time>` и `fu_time`.
#' @export
#' @importFrom dplyr if_else
#'
#' @examples
#' # Пример использования функции os_cut
#' data <- data.frame(
#'   date_start = as.Date("2020-01-01"),
#'   date_end = as.Date("2021-06-01"),
#'   status = c(1, 0, 1)
#' )
#' result <- os_cut(
#'   date_end = data$date_end,
#'   date_start = data$date_start,
#'   status = data$status,
#'   cens_time = 30,        # Время измеряется в месяцах
#'   cutoff_time = 12,      # 12 месяцев
#'   data = data
#' )
os_cut <- function(date_end,
                   date_start,
                   status,
                   cens_time,
                   cutoff_time,
                   data) {
  fu_time <- as.numeric(as.Date(date_end) - as.Date(date_start)) / cens_time
  os_time <- if_else(fu_time >= cutoff_time, cutoff_time, fu_time)
  os_status <- ifelse(fu_time >= cutoff_time, 0, status)
  
  status_col_name <- paste0("os_status_", cutoff_time)
  time_col_name <- paste0("os_time_", cutoff_time)
  
  result <- setNames(data.frame(os_status, os_time),
                     c(status_col_name, time_col_name))
  result$fu_time <- fu_time
  
  cbind(data, result)
}

#' Цензурирование данных с конкурирующими рисками
#'
#' Функция выполняет цензурирование данных о конкурирующих рисках на основе заданного времени отсечки.
#'
#' @param date_end Дата окончания наблюдения или события (в формате Date).
#' @param date_start Дата начала наблюдения (в формате Date).
#' @param cmprsk_date Дата наступления конкурирующего риска (в формате Date).
#' @param status Статус основного события (0 - цензурировано, 1 - событие произошло).
#' @param cens_time Единица времени для цензурирования (число).
#' @param cutoff_time Время отсечки для цензурирования (число).
#' @param status_name Название основного события (строка, например, "Рецидив").
#' @param cmprsk_name Название конкурирующего риска (строка, например, "Смерть").
#' @param data Датафрейм, содержащий исходные данные.
#'
#' @return Датафрейм с добавленными колонками: `<cmprsk_name>_status_<cutoff_time>` и `<cmprsk_name>_time_<cutoff_time>`.
#' @export
#' @importFrom dplyr if_else
#'
#' @examples
#' # Пример использования функции cmprsk_cut
#' data <- data.frame(
#'   date_start = as.Date("2020-01-01"),
#'   date_end = as.Date("2021-06-01"),
#'   cmprsk_date = as.Date(c(NA, "2021-01-01", NA)),
#'   status = c(1, 0, 1)
#' )
#' result <- cmprsk_cut(
#'   date_end = data$date_end,
#'   date_start = data$date_start,
#'   cmprsk_date = data$cmprsk_date,
#'   status = data$status,
#'   cens_time = 30,         # Время измеряется в месяцах
#'   cutoff_time = 12,       # 12 месяцев
#'   status_name = "Рецидив",
#'   cmprsk_name = "Смерть",
#'   data = data
#' )
cmprsk_cut <- function(date_end,
                       date_start,
                       cmprsk_date,
                       status,
                       cens_time,
                       cutoff_time,
                       status_name,
                       cmprsk_name,
                       data) {
  fu_time <- as.numeric(as.Date(date_end) - as.Date(date_start)) / cens_time
  
  relstatus <- ifelse(is.na(cmprsk_date), status, cmprsk_name)
  relstatus <- ifelse(relstatus == 1, status_name, relstatus)
  reldays <- as.numeric(as.Date(cmprsk_date) - as.Date(date_start)) / cens_time
  reltime <- ifelse(is.na(reldays), fu_time, reldays)
  statusn <- as.factor(ifelse(reltime >= cutoff_time, 0, relstatus))
  time <- ifelse(reltime > cutoff_time, cutoff_time, reltime)
  
  status_col_name <- paste0(cmprsk_name, "_status_", cutoff_time)
  time_col_name <- paste0(cmprsk_name, "_time_", cutoff_time)
  
  result <- setNames(data.frame(statusn, time),
                     c(status_col_name, time_col_name))
  
  cbind(data, result)
}

