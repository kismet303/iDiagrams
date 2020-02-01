#' Generate Win Loss Diagram
#'
#' Uses the DiagrammeR package to return a win-loss diagram.
#' Currently only supports exactly 5 rows.
#'
#' @param data Data frame with columns `endpoint`, ` 'wins', 'ties', 'losses'`.
#' The dataframe should have exactly 5 rows.
#' @param topline Character string to be included in the top box.
#'
#' @return grViz object. Refer to DiagrammeR package for details.
#' @export
#'
#' @examples
#' data <- data.frame(endpoint = c(paste0("endpoint", 1:5)),
#' wins = c(5,5,5,5,5),
#' ties = c(50,40,30,20,10),
#' losses = c(5,5,5,5,5))
#' wl_diagram(data, topline="N=10*6 = 60")
#'

wl_diagram <- function(data, topline) {
  topline <- topline
  if(length(setdiff(c("endpoint", "wins", "ties", "losses"), names(data))) !=0){
    stop("`data` must have variables 'endpoint', 'wins', 'ties', 'losses'")
  }

  if(nrow(data) != 5) {
    stop("`data` is expected to have 5 rows")
  }


  if(!is.character(topline) || length(topline) != 1) {
    stop("`topline` should be length 1 character vector")
  }



  data$wins <-  formatC(data$wins, format="f", big.mark=",", digits=0)
  data$ties <-  formatC(data$ties, format="f", big.mark=",", digits=0)
  data$losses <-  formatC(data$losses, format="f", big.mark=",", digits=0)

  # For the substitution to work correcly need to define in .GlobalEnf
  # TODO: Improve this
  assign("wl_diagram.data", data, envir = .GlobalEnv)
  assign("wl_diagram.topline", topline, envir = .GlobalEnv)

  diagram <- DiagrammeR::grViz("
digraph wins_losses {

      # a 'graph' statement
      graph [overlap = true, fontsize = 10]

      # endpoint labels
      node[shape = plaintext, style=unfilled]
      D0; D1; D2; D3; D4; D5;

      # top box
      # node [shape = circle, fontname = Helvetica]
      node [shape = box, fontname = Helvetica]
      Top;


      # main boxes
      node [shape = box, fixedsize = true, style=filled, width = 0.9]
      11; 12; 13; 21; 22; 23; 31; 32; 33; 41; 42; 43; 51; 52; 53;


      node [fontname = Helvetica]
      Top [label= 'Total Patient Pairs\n @@5']
      12 [label= 'Ties \n @@1-1']
      22 [label= 'Ties \n @@1-2']
      32 [label= 'Ties \n @@1-3']
      42 [label= 'Ties \n @@1-4']
      52 [label= 'Ties \n @@1-5']


      11 [label= 'Wins  \n @@2-1']
      21 [label= 'Wins  \n @@2-2']
      31 [label= 'Wins  \n @@2-3']
      41 [label= 'Wins  \n @@2-4']
      51 [label= 'Wins  \n @@2-5']

      13 [label= 'Losses \n @@3-1']
      23 [label= 'Losses \n @@3-2']
      33 [label= 'Losses \n @@3-3']
      43 [label= 'Losses \n @@3-4']
      53 [label= 'Losses \n @@3-5']


      D0 [label= '']
      D1 [label= '@@4-1']
      D2 [label= '@@4-2']
      D3 [label= '@@4-3']
      D4 [label= '@@4-4']
      D5 [label= '@@4-5']


      node [fillcolor = green]
      11 [fillcolor = green]
      21 [fillcolor = green]
      31 [fillcolor = green]
      41 [fillcolor = green]
      51 [fillcolor = green]


      node [fillcolor = orange]
      12 [fillcolor = orange]
      22 [fillcolor = orange]
      32[fillcolor = orange]
      42[fillcolor = orange]
      52[fillcolor = orange]

      node [fillcolor = red]
      13 [fillcolor = red]
      23  [fillcolor = red]
      33 [fillcolor = red]
      43 [fillcolor = red]
      53 [fillcolor = red]


      # Endpoint labels
      D0 -> D1 [ color = 'white']
      D1 -> D2 [ color = 'white']
      D2 -> D3 [ color = 'white']
      D3 -> D4 [ color = 'white']
      D4 -> D5 [ color = 'white']

      Top->11; Top->12;Top->13;
      12 -> 21; 12 -> 22; 12 -> 23;
      22 -> 31; 22 -> 32; 22 -> 33
      32 -> 41; 32 -> 42; 32 -> 43;
      42 -> 51; 42 -> 52; 42 -> 53;

}

[1]: wl_diagram.data$ties
[2]: wl_diagram.data$wins
[3]: wl_diagram.data$losses
[4]: wl_diagram.data$endpoint
[5]: wl_diagram.topline
      ")

  rm("wl_diagram.data", envir = .GlobalEnv)
  rm("wl_diagram.topline", envir = .GlobalEnv)

  diagram
}

# debugonce(wl_diagram)
# wl_diagram(data=data2, topline="hello")
