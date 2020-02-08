#' Função que pega o status atual da tarefa.
#'
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return O status atual assim como registrado no arquivo.
#'
#' @export
get_status <- function(status_file){
  scan(status_file, what = "character",sep="\n")
}

#' Função que auxiliar usada para alterar o status atual da tarefa.
#'
#' @param msg String contendo o status da tarega.
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return None
#'
#' @export
set_status <- function(msg,status_file){
  write(msg, status_file)
}

#' Função que altera o status atual da tarefa para "interrupt".
#'
#' Usada como auxiliar para interromper a tarefa.
#'
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return None
#'
#' @export
fire_interrupt <- function(status_file){
  set_status("interrupt",status_file)
}

#' Função que altera o status atual da tarefa para "Ready".
#'
#' Usada como auxiliar para avisar que a tarefa terminou e permitir que seja executada novamente.
#'
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return None
#'
#' @export
fire_ready <- function(status_file){
  set_status("Ready",status_file)
}

#' Função que altera o status atual da tarefa para "Running..." e opicionalmente informa a porcentagem da tarefa que foi concluída.
#'
#' Usada como auxiliar para avisar que a tarefa está rodando e opcionalmente a porcentagem completa.
#'
#' @param perc_complete Um número entre 0 e 100 que representa a procentagem da tarefa que já foi rodada.
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return None
#'
#' @export
fire_running <- function(perc_complete,status_file){
  if(missing(perc_complete))
    msg <- "Running..."
  else
    msg <- paste0("Running... ", perc_complete, "% Complete")
  set_status(msg,status_file)
}

#' Função que verifica se o status atual é igual a "interrupt".
#'
#' Usada como auxiliar para interromper a tarefa que está rodando.
#'
#' @param status_file Um arquivo temporário onde está registrado o status da tarefa. Usualmente criado com o comando `status_file = tempfile()`.
#'
#' @return None
#'
#' @export
interrupted <- function(status_file){
  get_status(status_file) == "interrupt"
}
