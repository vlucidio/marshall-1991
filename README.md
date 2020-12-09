# marshall-1991
Estimador local do método de marshall 

library(readr)
dados <- read.table('banco.csv',header=TRUE, sep=';',dec=",")
r_local <- c()
n_local <- c()
m_local <- c()
s_2_local <- c()
n_med_local <- c()
c_local <- c()
taxa_local <- c()

for(j in 1:157){
  r_local[j] <- dados$Obitos.C15[j]
  n_local[j] <- dados$X2010.2018[j]
  
  for(i in mapa_local_nb[[j]]){
    r_local[j] <- r_local[j]+dados$Obitos.C15[i]
    n_local[j] <- n_local[j]+dados$X2010.2018[i]
    m_local[j] <- r_local[j]/n_local[j]
    
    n_med_local[j] = (n_local[j])/(length(mapa_local_nb[[j]])+1)
    
    
    
  }
  s_2_local[j] <- dados$X2010.2018[j]*(dados$TaxaMortalidade.C15[j] - m_local[j])^2
  for(i in mapa_local_nb[[j]]){
    
    s_2_local[j] <- s_2_local[j]+dados$X2010.2018[i]*(dados$TaxaMortalidade.C15[i] - m_local[j])^2
  }
  
  
  s_2_local[j] <- s_2_local[j]/n_local[j]
  
  c_local[j] <- (s_2_local[j]-(m_local[j]/n_med_local[j]))/(s_2_local[j]-(m_local[j]/n_med_local[j])
                                                            +(m_local[j]/dados$X2010.2018[j]))
  taxa_local[j] <- m_local[j]+(c_local[j]*(dados$TaxaMortalidade.C15[j]-m_local[j]))
}
