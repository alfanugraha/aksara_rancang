satelliteImpact <- function(sat_type = "energy", tbl_sat = data.frame(), tbl_output_matrix = matrix(), emission_lookup = data.frame()){ 
  if(sat_type == "energy" | sat_type == "waste"){
    impact <- list() # impact$cons; impact$emission
    # if(sat_type == "energy") impact$cons <- energy else impact$cons <- waste
    impact$cons <- tbl_sat
    
    prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
    impact$cons[, 4:ncol(impact$cons)] <- prop
    
    coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% tbl_output_matrix
    colnames(impact$cons)[3] <- "Tconsumption"
    
    impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
    
    order_cname <- names(impact$cons)[4:ncol(impact$cons)]
    em_f <- numeric()
    for(m in 1:length(order_cname)){
      em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
    }
    em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
    
    impact$emission <- impact$cons
    impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
    impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
    colnames(impact$emission)[3] <- "Temission"
  } else { # for labour case
    impact <- list()
    impact$cons <- tbl_sat
    coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% tbl_output_matrix
  }
  impact$cons[is.na(impact$cons)] <- 0
  impact$emission[is.na(impact$emission)] <- 0
  return(impact)
}