# Analiza kredytu hipotecznego z ratami stałymi i malejącymi

# Przykładowe dane: 
#  K <- 200000  # Kwota kredytu
#  r <- 0.05    # roczna stopa oprocentowania (5%)
#  n <- 120     # Liczba miesięcy (okres kredytu 10 lat)


# 1.Kwota kredytu hipotecznego ze stałą ratą: K
# Roczna stopa oprocentowania: r
# Okres w miesiącach: n

# Wysokość oprocentowania miesięcznego:
  op_miesięczne <- function(r) {
    q <- 1 + r / 12
    return(q)
  }
  q <- op_miesięczne(r)
  cat("Oprocentowanie miesięczne (q):", q)


# Wysokość raty miesięcznej:
  rata_miesięczna <- function(K, q, n) {
    R <- K * (q^n) * ((q - 1) / (q^n - 1))
    return(R)
  }
  R <- rata_miesięczna(K, q, n)
  cat("Miesięczna rata (R):", R)

# Całkowitą kwotę do spłaty F = R*n
  kwota_do_spłaty <- function(R, n) {
    F = R*n
    return(F)
  }
  F <- kwota_do_spłaty(R, n)
  cat("Całkowita kwota do spłaty (F):", F)




# 2.Kwota kredytu hipotecznego z ratą malejącą: K
# Roczna stopa oprocentowania: r
# Okres w miesiącach: n

# Wysokość części kapitałowej raty:
  wysokość_czesci_kapitalowej_raty <- function(K, n) {
    R0 = K/n
    return(R0)
  }
  R0 <- wysokość_czesci_kapitalowej_raty (K, n)
  cat("Wysokość części kapitałowej raty (R0):", R0)

#Wysokość części odsetkowej raty i-tej Ri1 = ((K-(i - 1)*R0)*r)/12
  wysokość_czesci_odsetkowej_raty_i <- function(K, i, r) {
    Ri1 = ((K-(i - 1)*R0)*r)/12
    return(Ri1)
  }
  Ri1 <- wysokość_czesci_odsetkowej_raty_i (K, 26, r)
  cat("Wysokość części odsetkowej raty i-tej:", Ri1)

# Wysokość raty i-tej Ri = R0 + Ri1
  rata_i <- function(K, r, n, i) {
    R0 = K / n 
    Ri1 = ((K - (i - 1) * R0) * r) / 12
    Ri = R0 + Ri1  
    return(Ri)
  }
  Ri = rata_i(K, r, n, 14)
  cat("Wysokość raty i-tej (Ri):", Ri)

# Całkowitą kwotę do spłaty
  kwota_całkowita_F <- function(K, r, n) {
    sum_Ri1 <- sum(((K - (1:(n-1)) * R0) * r) / 12)
    F <- K + sum_Ri1
    return(F)
  }
  kwota_F <- kwota_całkowita_F(K, r, n)
  cat("Całkowita kwota do spłaty F:", kwota_F)

# Najniższa, średnia i najwyższa wartość raty
  raty <- numeric(n)
  for (i in 1:n) {
    raty[i] <- rata_i(K, r, n, i)
  }

  min_rata = min(raty)
  srednia_rata = mean(raty)
  max_rata = max(raty)
  
  print(min_rata)
  print(srednia_rata)
  print(max_rata)