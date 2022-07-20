### ANALISE DA ATA DO COPOM ###

# OBJETIVO: Analisar a ata de junho 2022 do COPOM usando Text Mining.

# PREMISSA: Será usada a versão em inglês da ata por conta dos pacotes usados.

# PASSO 1: Instalar e carregar os pacotes necessários:

if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  "tidytext",
  "pdftools",
  "stopwords",
  "textdata",
  "dplyr",
  "tidyr",
  "lubridate",
  "magrittr",
  "knitr",
  "ggplot2",
  "ggthemes",
  "jsonlite",
  "purrr",
  "stringr",
  "scales",
  "forcats"
)

# PASSO 2: Importar a ata de Junho 2022 do COPOM:

# URL da ata de junho 2022
www <- "https://www.bcb.gov.br/content/copom/copomminutes/Minutes%20247.pdf"
# Ler arquivo PDF convertendo para caracter
# Vetor de caracteres com tamanho igual ao numero de paginas
raw_copom_last <- pdftools::pdf_text(www)

# PASSO 3: Tratar os dados do relatório:

#Tratar para formar um objeto DataFrame com 3 colunas
#Coluna 1: dados do texto de cada pagina da ata
#Coluna 2: mes da reuniao do COPOM
#Coluna 3: informacao da pagina referente aos dados do texto

copom_last_clean <- dplyr::tibble(
  text = unlist(strsplit(raw_copom_last, "\r"))
) %>% 
  dplyr::mutate(
    meeting = "June 2022", 
    page = dplyr::row_number(),
    text = gsub("\n", "", text)
  )

# PASSO 4: Aplicar o processo de "Tokenização" dos dados:

#Token é uma unidade de texto, frequentemente uma palavra. A "Tokenização" é a 
#quebra do texto em tokens.

# Text mining - Criar tokens
copom_last <- copom_last_clean %>% 
  tidytext::unnest_tokens(word, text)
# Contar palavras
copom_last %>%
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::slice_head(n = 6) %>% 
  knitr::kable()

# PASSO 5: Remover as "Stop words" e números classificados como palavras:

copom_last_sw <- copom_last %>%
  # Remover palavras comuns (stop words)
  dplyr::anti_join(stop_words)%>%
  # Remover números
  dplyr::mutate(word = gsub("[^A-Za-z ]", "", word)) %>%
  # Contar palavras
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::filter(word != "")
copom_last_sw %>% 
  dplyr::slice_head(n = 6) %>% 
  knitr::kable()

# PASSO 6: Análise de sentimento. Identificar as palavras negativas e positivas
#          usadas com mais frequência:

# Obter análise de sentimento das palavras com base em uma biblioteca
copom_last_sw %>%
dplyr::inner_join(tidytext::get_sentiments("bing")) %>% 
dplyr::slice_head(n = 10) %>% 
knitr::kable()

# PASSO 7: Análise de sentimento. Gráfico de sentimento da ata completa:

# Análise de sentimento final da ata
copom_sentiment <- copom_last %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::count(meeting, page, sentiment) %>%
  tidyr::pivot_wider(
    id_cols = c(meeting, page),
    names_from = sentiment, 
    values_from = n,
    values_fill = 0
  ) %>%
  dplyr::mutate(sentiment = positive - negative)
# Gerar gráfico
copom_sentiment %>% 
  ggplot2::ggplot(ggplot2::aes(page, sentiment, fill = sentiment > 0)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = c("#b22200", "#282f6b")) +
  ggplot2::labs(
    x = "Página da ata",
    y = "Sentimento",
    title = "Análise de sentimento da Ata do COPOM - Junho/2022",
    subtitle = "Bing lexicon",
    caption = paste0("Elaboração: Marcelo Otavio Milani\nDados: ", www)
  )

