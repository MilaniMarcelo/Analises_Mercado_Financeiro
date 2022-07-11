# CALCULAR AS PROBABILIDADES TEÓRICAS DA LEI DE BENFORD

# Pacotes Necessários
library(ggplot2)
library(tidyverse)
library(kableExtra)

# Encontrando as probabilidades
# de cada Dígito
Benford_log <- t (
  sapply( 1:9, function(i){
    logs <- log10( 1 + (1/i) )
    return( c( DIGITO = i, PROBABILIDADE_OCORRENCIA = round(logs, digits = 4) ) )
  } ) )

# Criando um table para o gráfico
Benford_log <- tibble::as_tibble(Benford_log)
Benford_log %>%
  kbl() %>% 
  kable_styling("hover", full_width = F)

Benford_log <- Benford_log %>% 
  mutate(DIGITO = as.integer(DIGITO))
# <> --------------------------- <>
# <> --------------------------- <>
# Criando o gráfico de
# Dígito vs Probabilidade calculada pelo Log
ggplot(Benford_log, aes(x = DIGITO, y = PROBABILIDADE_OCORRENCIA ) ) +
  geom_line(linetype = "dashed") +
  geom_point() +
  geom_text(aes(label = PROBABILIDADE_OCORRENCIA), 
            vjust = -1) +
  scale_x_discrete(limits = 1:9) +
  scale_y_continuous(limits = c(0,0.35)) +
  theme_bw()
  
