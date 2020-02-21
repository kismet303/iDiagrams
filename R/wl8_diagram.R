#' Generate Win Loss Diagram
#'
#' Uses the DiagrammeR package to return a win-loss diagram.
#' Currently only supports exactly 8 rows.
#'
#' @param data Data frame with columns `endpoint`, ` 'wins', 'ties', 'losses'`.
#' The dataframe should have exactly 8 rows.
#' @param topline Character string to be included in the top box.
#' @param colour Logical default = TRUE. If TRUE the diagram will be coloured.
#'
#' @return grViz object. Refer to DiagrammeR package for details.
#' @export
#'
#' @examples
#' data <- data.frame(endpoint = c(paste0("endpoint", 1:8)),
#' wins = c(5,5,5,5,5,5,5,5),
#' ties = c(80,70,60,50,40,30,20,10),
#' losses = c(5,5,5,5,5,5,5,5))
#' wl8_diagram(data, topline="N=10*9 = 90")
#' wl8_diagram(data, topline="N=10*9 = 90", colour = FALSE)

wl8_diagram <- function(data, topline, colour=TRUE) {
  topline <- topline
  if(length(setdiff(c("endpoint", "wins", "ties", "losses"), names(data))) !=0){
    stop("`data` must have variables 'endpoint', 'wins', 'ties', 'losses'")
  }

  if(nrow(data) != 8) {
    stop("`data` is expected to have 5 rows")
  }

  if(!is.character(topline) || length(topline) != 1) {
    stop("`topline` should be length 1 character vector")
  }

  if (colour) {
    colours <-  c("Green", "Orange", "Red")
  } else {
    colours <-  c("Grey", "Grey", "Grey")
  }

  data$wins <-  formatC(data$wins, format="f", big.mark=",", digits=0)
  data$ties <-  formatC(data$ties, format="f", big.mark=",", digits=0)
  data$losses <-  formatC(data$losses, format="f", big.mark=",", digits=0)

  # For the substitution to work correcly need to define in .GlobalEnf
  # TODO: Improve this
  assign("wl_diagram.data", data, envir = .GlobalEnv)
  assign("wl_diagram.topline", topline, envir = .GlobalEnv)
  assign("wl_diagram.colours", colours, envir = .GlobalEnv)

  diagram <- DiagrammeR::grViz("
digraph wins_losses {

      # a 'graph' statement
      graph [overlap = true, fontsize = 10]

      # endpoint labels
      node[shape = plaintext, style=unfilled]
      D0; D1; D2; D3; D4; D5; D6; D7; D8;

      # top box
      # node [shape = circle, fontname = Helvetica]
      node [shape = box, fontname = Helvetica]
      Top;


      # main boxes
      node [shape = box, fixedsize = true, style=filled, width = 0.9]
      11; 12; 13; 21; 22; 23; 31; 32; 33; 41; 42; 43; 51; 52; 53; 61; 62; 63; 71; 72; 73; 81; 82; 83;

      node [fontname = Helvetica]
      Top [label= 'Total Patient Pairs\n @@5']
      12 [label= 'Ties \n @@1-1', fillcolor = @@6-2]
      22 [label= 'Ties \n @@1-2', fillcolor = @@6-2]
      32 [label= 'Ties \n @@1-3', fillcolor = @@6-2]
      42 [label= 'Ties \n @@1-4', fillcolor = @@6-2]
      52 [label= 'Ties \n @@1-5', fillcolor = @@6-2]
      62 [label= 'Ties \n @@1-6', fillcolor = @@6-2]
      72 [label= 'Ties \n @@1-7', fillcolor = @@6-2]
      82 [label= 'Ties \n @@1-8', fillcolor = @@6-2]

      11 [label= 'Wins  \n @@2-1', fillcolor = @@6-1]
      21 [label= 'Wins  \n @@2-2', fillcolor = @@6-1]
      31 [label= 'Wins  \n @@2-3', fillcolor = @@6-1]
      41 [label= 'Wins  \n @@2-4', fillcolor = @@6-1]
      51 [label= 'Wins  \n @@2-5', fillcolor = @@6-1]
      61 [label= 'Wins  \n @@2-6', fillcolor = @@6-1]
      71 [label= 'Wins  \n @@2-7', fillcolor = @@6-1]
      81 [label= 'Wins  \n @@2-8', fillcolor = @@6-1]

      13 [label= 'Losses \n @@3-1', fillcolor = @@6-3]
      23 [label= 'Losses \n @@3-2', fillcolor = @@6-3]
      33 [label= 'Losses \n @@3-3', fillcolor = @@6-3]
      43 [label= 'Losses \n @@3-4', fillcolor = @@6-3]
      53 [label= 'Losses \n @@3-5', fillcolor = @@6-3]
      63 [label= 'Losses \n @@3-6', fillcolor = @@6-3]
      73 [label= 'Losses \n @@3-7', fillcolor = @@6-3]
      83 [label= 'Losses \n @@3-8', fillcolor = @@6-3]

      D0 [label= '']
      D1 [label= '@@4-1']
      D2 [label= '@@4-2']
      D3 [label= '@@4-3']
      D4 [label= '@@4-4']
      D5 [label= '@@4-5']
      D6 [label= '@@4-6']
      D7 [label= '@@4-7']
      D8 [label= '@@4-8']

      # Endpoint labels
      D0 -> D1 [ color = 'white']
      D1 -> D2 [ color = 'white']
      D2 -> D3 [ color = 'white']
      D3 -> D4 [ color = 'white']
      D4 -> D5 [ color = 'white']
      D5 -> D6 [ color = 'white']
      D6 -> D7 [ color = 'white']
      D7 -> D8 [ color = 'white']

      Top->11; Top->12;Top->13;
      12 -> 21; 12 -> 22; 12 -> 23;
      22 -> 31; 22 -> 32; 22 -> 33
      32 -> 41; 32 -> 42; 32 -> 43;
      42 -> 51; 42 -> 52; 42 -> 53;
      52 -> 61; 52 -> 62; 52 -> 63;
      62 -> 71; 62 -> 72; 62 -> 73;
      72 -> 81; 72 -> 82; 72 -> 83;

}

[1]: wl_diagram.data$ties
[2]: wl_diagram.data$wins
[3]: wl_diagram.data$losses
[4]: wl_diagram.data$endpoint
[5]: wl_diagram.topline
[6]: wl_diagram.colours
      ")

  rm("wl_diagram.data", envir = .GlobalEnv)
  rm("wl_diagram.topline", envir = .GlobalEnv)
  rm("wl_diagram.colours", envir = .GlobalEnv)

  diagram
}
