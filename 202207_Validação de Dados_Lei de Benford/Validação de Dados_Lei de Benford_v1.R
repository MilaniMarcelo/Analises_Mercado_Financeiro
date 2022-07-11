# VALIDAÇÃO DE DADOS - LEI DE BENFORD

# OBJETIVO: Utilizar a Lei de Benford como instrumento para validação de uma
#           base de dados real, que contém o Patrimônio Declarado de Clientes

# PASSO 1: Carregar a base de dados (a base está no formato .csv)

#Carregar pacote de importação de dados
library(readr)
#Importar base de dados
BD_Patrimonio_Clientes <- read_csv("C:/Users/momil/OneDrive/Gestão - Marcelo Milani/Projetos Hobbies/3. Mercado Financeiro/202207_Validação de Dados_Lei de Benford/Validação de Dados_Lei de Benford/Base_Dados_Patrimonio_Clientes.csv")

# PASSO 2: Carregar pacote que analisa dados usando a Lei de Benford

#Carregar pacote
library(benford.analysis)

# PASSO 3: Analisar os dígitos de primeira e segunda ordem pela Lei de Benford

#Comando para realizar a análise
bfd.cp <- benford(BD_Patrimonio_Clientes$Patrimonio_Cliente)
#Plotar resultados graficamente
plot(bfd.cp)

# PASSO 4: Validar os resultados obtidos 
#         (Testes estatísticos: Qui-Quadrado e Desvio Absoluto Médio)

#Exibir os resultados da análise realizada
bfd.cp

# PASSO 5: Gerar lista com os dados que apresentam distorção (Dados suspeitos)

Dados_Suspeitos <- getSuspects(bfd.cp, BD_Patrimonio_Clientes)

