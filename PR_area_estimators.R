PR_area_estimators <- function(M){
  # ############################################################################ #
  #' This function calculates four estimators of 
  #' the area under the precision-recall curve. 
  #' This function is supplementary material for the paper 
  #' "An Experimental Investigation on the Noise Resilience of Ranking Measures"
  #' submitted to ICONIP 2016 by Daniel Berrar.
  #' 
  #' The function calculates the following estimators:
  #' 
  #' (1) The average precision (AUCPR_avg); Eq. 6 of the paper.
  #' (2) The minmax trapezoid (AUCPR_minmax); Eq. 7 of the paper.
  #' (3) The upper trapezoid (AUCPR_max); Eq. 8 of the paper.
  #' (4) The lower trapezoid (AUCPR_min); Eq. 9 of the paper.
  #' 
  #' INPUT: a matrix M with n rows and 2 columns.
  #'        The first column contains the real class label, where
  #'        1 indicates positive and 0 indicates negative.
  #'        n refers to the total number of instances.
  #' 
  #' OUTPUT: AUCPR_avg, AUCPR_minmax, AUCPR_max, and AUCPR_min.
  #' 
  #' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
  #' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
  #' OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
  #' NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
  #' HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
  #' WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
  #' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
  #' OTHER DEALINGS IN THE SOFTWARE.
  #'    
  # ############################################################################ #
  
  # ---------------------------------------------------------------------------- #
  # BEGIN input error handling.
  # ---------------------------------------------------------------------------- #
  if (!is.matrix(M)) {
    stop("Invalid class.  ", "PR_area_estimators() needs an n x 2 matrix input.")
  }
  
  if (any(!is.element(M[, 1], c(0, 1)))) {
    stop("Invalid class labels.  ", "PR_area_estimators() needs class labels 1 and 0.")
  }
  
  if (!is.element(0, M[, 1])) {
    stop("Invalid class labels.  ", "Some class labels should be 0.")
  }
  
  if (!is.element(1, M[, 1])) {
    stop("Invalid class labels.  ", "Some class labels should be 1.")
  }
  # ---------------------------------------------------------------------------- #
  # END input error handling.
  # ---------------------------------------------------------------------------- #
  
  
  # ---------------------------------------------------------------------------- #
  # BEGIN data pre-processing.
  # ---------------------------------------------------------------------------- #
  # Order M based on decreasing values of the ranking score.
  M <- M[order(M[, 2], decreasing = TRUE), ]
  # ---------------------------------------------------------------------------- #
  # END data pre-processing.
  # ---------------------------------------------------------------------------- #
    
  
  # ---------------------------------------------------------------------------- #
  # BEGIN calculating average precision (AUCPR_avg). [Slightly faster version]
  # ---------------------------------------------------------------------------- #
  pos <- which(M[, 1] == 1)
  maxpos <- max(pos)
  X <- M[1:maxpos, ]
  prec <- rep(0, nrow(X))
  X <- cbind(X, prec)
  
  # Calculate now the precision at each threshold.
  counter <- 0
  for (i in 1:nrow(X)) {
    if(X[i, 1] == 1) {
      counter <- counter + 1
      X[i, 3] <- counter / i
    } else {
      X[i, 3] <- counter / i
    }
  }
  
  pos <- which(X[, 1] == 1)
  AUCPR_avg <- sum(X[pos, 3]) / nrow(X[pos, ])
  names(AUCPR_avg) <- "AUCPR_avg"
  # ---------------------------------------------------------------------------- #
  # END calculating average precision. [Slightly faster version]
  # ---------------------------------------------------------------------------- #
  
  
  # ---------------------------------------------------------------------------- #
  # BEGIN calculating average precision (AUCPR_avg). [Eq.6]
  # ---------------------------------------------------------------------------- #
  # The following code produces the same result as the faster version above.
  # The code here corresponds to Eq.6. of the paper.
  
  # Calculate precision, recall (or TPR), and FPR.
  #   n <- nrow(M)
  #   n_positive <- sum(M[, 1])
  #   n_negative <- n - n_positive
  #   prec <- c(rep(0, n))
  #   recall <- c(rep(0, n))
  #   FPR <- c(rep(0, n))
  
  #   for (i in 1:n) {
  #     x <- cumsum(M[1:i, 1])
  #     prec[i] <- x[length(x)] / i
  #     recall[i] <- x[length(x)] / n_positive
  #     FPR[i] <- length(which(M[1:i, 1] == 0)) / n_negative    
  #   }
  #   
  #   B <- cbind(prec, recall)
  #   colnames(B) <- c("precision", "recall")
  #   
  #   AUCPR_avg <- 0
  #   
  #   for (i in 1:nrow(B)) {
  #     prec_i <- B[i, 1]
  #     r_i <- B[i, 2]
  #     if (i == 1) {
  #       r_iminus1 <- 0
  #     } else {
  #       r_iminus1 <- B[i - 1, 2]
  #     }
  #     AUCPR_avg <- AUCPR_avg + prec_i * (r_i - r_iminus1)
  #   }
  #   
  #   names(AUCPR_avg) <- "AUCPR_avg"
  # ---------------------------------------------------------------------------- #
  # END calculating average precision. [Eq.6]
  # ---------------------------------------------------------------------------- #
  
  
  # ---------------------------------------------------------------------------- #
  # BEGIN calculating AUCPR_minmax, AUCPR_max, and AUCPR_min.
  # ---------------------------------------------------------------------------- #
  # Calculate precision, recall (or TPR), and FPR.
  n <- nrow(M)
  n_positive <- sum(M[, 1])
  n_negative <- n - n_positive
  prec <- c(rep(0, n))
  recall <- c(rep(0, n))
  FPR <- c(rep(0, n))
  
  for (i in 1:n) {
    x <- cumsum(M[1:i, 1])
    prec[i] <- x[length(x)] / i
    recall[i] <- x[length(x)] / n_positive
    FPR[i] <- length(which(M[1:i, 1] == 0)) / n_negative    
  }
  
  B <- cbind(recall, round(prec, 3))
  colnames(B) <- c("recall", "precision")
  B <- rbind(c(0, 0), B) # Include the start point (0,0).
  AUCPR_minmax <- 0
  AUCPR_max <- 0
  AUCPR_min <- 0
  
  for (i in 1:(nrow(B) - 1)) {
    r_i <- B[i, 1]
    r_iplus1 <- B[i + 1, 1]
    
    pos_i <- which(B[, 1] == r_i)
    pos_iplus1 <- which(B[, 1] == r_iplus1)
    
    p_min_i <- min(B[pos_i, 2])
    p_max_i <- max(B[pos_i, 2])
    
    p_min_iplus1 <- min(B[pos_iplus1, 2])
    p_max_iplus1 <- max(B[pos_iplus1, 2])
    
    piece_minmax <- (p_min_i + p_max_iplus1) / 2 * (r_iplus1 - r_i) # Eq.7
    piece_max <- (p_max_i + p_max_iplus1) / 2 * (r_iplus1 - r_i) # Eq.8
    piece_min <- (p_min_i + p_min_iplus1) / 2 * (r_iplus1 - r_i) # Eq.9
    
    AUCPR_minmax <- AUCPR_minmax + piece_minmax    
    AUCPR_max <- AUCPR_max + piece_max    
    AUCPR_min <- AUCPR_min + piece_min    
  }
  
  names(AUCPR_minmax) <- "AUCPR_minmax"
  names(AUCPR_max) <- "AUCPR_max"
  names(AUCPR_min) <- "AUCPR_min"
  # ---------------------------------------------------------------------------- #
  # END calculating AUCPR_minmax, AUCPR_max, and AUCPR_min.
  # ---------------------------------------------------------------------------- #
  
  
  
  # ############################################################################ #
  result <- c(AUCPR_avg, AUCPR_minmax, AUCPR_max, AUCPR_min)
  result
}  
