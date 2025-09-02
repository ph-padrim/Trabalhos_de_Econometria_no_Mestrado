# ==============================================================================
#                EXERCÍCIO 1 DE MICROECONOMETRIA APLICADA 
# ==============================================================================

# ------------------------------------------------------------------------------ 
#                         Descrição do Trabalho
#
#   Foi utilizada a base de dados da RAIS com informações de firmas de 2021, 
# disponibilizada no drive da disciplina). A ideia do trabalho é fazer análises 
# da produtividade para grupos de comparação, sociodemográficos, temporais ou 
# espaciais. Por fim elaborar um relatório com aplicações livres de: (i) 
# análises não-paramétricas; (ii) regressões quantílicas; (iii) decomposição de 
# Oaxaca-Blinder.
# ------------------------------------------------------------------------------

# Carregando pacotes

library(readr)
library(dplyr)
library(ggplot2)
library(quantreg)
library(oaxaca)

# Carregando a base de dados da RAIS de firmas de 2021

base <- read.csv("/home/ph-padrim/Downloads/rais_firma_2021.csv",
                 fileEncoding = "cp1252",
                 sep = ",",
                 dec = ".")

# Descrição da base

names(base)
summary(base)

# ------------------------------------------------------------------------------
#                             Kdensity (base completa)
# ------------------------------------------------------------------------------

# Kdensity  do setor público vs setor privado

base <- base %>% 
  mutate(Publico = ifelse(startsWith(as.character(natureza_juridica),"1"),
                          "Público", "Privado")) %>%
  mutate(Publico = as.factor(Publico))

base %>%
  ggplot(aes(x = produtividade, fill = Publico)) +
  geom_density(alpha = 0.5, na.rm = T) +
  labs(title = "Densidade Kernel Público x Privado", 
           x = "Produtividade", y = "Densidade") +
  theme_minimal() +
  xlim(0,200)

# ------------------------------------------------------------------------------
#                Kdensity (base apenas das empresas privadas)
# ------------------------------------------------------------------------------

# Base com apenas o setor privado criada a partir da base original

base_privado <- base %>% filter(Publico == "Privado")

# Kdensity entre os diferentes níveis tecnológicos das empresas
                                                   
unique(base_privado$nivel_tec)
base_privado$nivel_tec <- as.factor(base_privado$nivel_tec)
base_privado %>%
  ggplot(aes(x = produtividade, fill = nivel_tec)) +
  geom_density(alpha = 0.5, na.rm = T) +
  labs(title = "Densidade Kernel por nível tecnológico", x = "Produtividade", 
           y = "Densidade") +
  theme_minimal() +
  xlim(0,100)

# Kdensity do sexo majoritário. Se a empresas possui um percentual maior de 50% 
# de funcionários do sexo feminino, logo ele é classificada como sexo_majoritario 
# "Feminino". Caso contrário, sexo_majoritario recebe a classificação "Masculino"

base_privado <- base_privado %>%
  mutate(sexo_majoritario = if_else(sexo_med > 0.5, "Feminino", "Masculino")) %>%
  mutate(sexo_majoritario = as.factor(sexo_majoritario))

base_privado %>%
  ggplot(aes(x = produtividade, fill = sexo_majoritario)) +
  geom_density(alpha = 0.5, na.rm = T) +
  labs(title = "Densidade Kernel por sexo majoritário", x = "Produtividade", 
           y = "Densidade") +
  theme_minimal() +
  xlim(0,50)

# Kdensity das empresas MEI vs não-MEI 

unique(base_privado$tamanho)
base_privado <- base_privado %>%
  mutate(MEI = if_else(tamanho == 1, "Sim", "Não")) %>%
  mutate(MEI = as.factor(MEI))

base_privado %>%
  ggplot(aes(x = produtividade, fill = MEI)) +
  geom_density(alpha = 0.5, na.rm = T) +
  labs(title = "Densidade Kernel MEI x outros tamanhos", x = "Produtividade", 
           y = "Densidade") +
  theme_minimal() +
  xlim(0,50)

# Criando uma nova variável para discriminar o tamonho das empresas

base_privado <- base_privado %>%
  mutate(tamanho_cat = case_when(
    tamanho == "1" ~ "MEI",
    tamanho == "2" ~ "micro",
    tamanho == "3" ~ "pequena", 
    tamanho == "4" ~ "média",
    tamanho == "5" ~ "grande",
    TRUE ~ "outro"
  ))
base_privado$tamanho_cat <- as.factor(base_privado$tamanho_cat)

# Kdensity por tamanho da empresa segundo a classificação da variável tamanho_cat

base_privado %>%
  ggplot(aes(x = produtividade, fill = tamanho_cat)) +
  geom_density(alpha = 0.5, na.rm = TRUE) +
  labs(title = "Densidade Kernel Por Tamanho de Empresa", 
       x = "Produtividade", 
       y = "Densidade",
       fill = "Tamanho") +
  theme_minimal() +
  xlim(0, 50)

# ------------------------------------------------------------------------------
#                             Regressão kernel
# ------------------------------------------------------------------------------

# Quantidade de vínculos explicativa da produtividade

vinc_sal_priv_limpos <- base_privado[, c("qtd_vinc_atv", "produtividade", 
                                       "nivel_tec")] %>% 
na.omit() %>%
arrange(qtd_vinc_atv)

reg_kernel_vinc_sal <- ksmooth(vinc_sal_priv_limpos$qtd_vinc_atv, 
                               vinc_sal_priv_limpos$produtividade,
                               "normal")

plot(reg_kernel_vinc_sal,
     xlab = "Quantidade de Vínculos Ativos",
     ylab = "Produtividade",
     main = "Regressão Kernel Produtividade vs Vínculos
     (privado)")

# Quantidade de vínculos explicativa da produtividade (sem outliers)

mean(vinc_sal_priv_limpos$qtd_vinc_atv) + 3*sd(vinc_sal_priv_limpos$qtd_vinc_atv)
mean(vinc_sal_priv_limpos$produtividade) + 3*sd(vinc_sal_priv_limpos$produtividade)

vinc_sal_priv_limpos2 <- vinc_sal_priv_limpos[(vinc_sal_priv_limpos$qtd_vinc_atv <= 246.3813) 
                                              &
                                              vinc_sal_priv_limpos$produtividade <= 60.50579,]

reg_kernel_vinc_sal2 <- ksmooth(vinc_sal_priv_limpos2$qtd_vinc_atv, 
                                vinc_sal_priv_limpos2$produtividade,
                                "normal")

plot(reg_kernel_vinc_sal2,
     xlab = "Quantidade de Vínculos Ativos sem ouliers",
     ylab = "Produtividade (sem outliers)",
     main = "Regressão Kernel",
     type = "l",
     lwd = 1, xlim = c(0, 246.3813), ylim = c(0, 60.50579))
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 0,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 0,]$produtividade,
              "normal"), col = 2)
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 1,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 1,]$produtividade,
              "normal"), col = 3)
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 2,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 2,]$produtividade,
              "normal"), col = 4)
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 3,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 3,]$produtividade,
              "normal"), col = 5)
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 4,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 4,]$produtividade,
              "normal"), col = 6)
lines(ksmooth(vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 5,]$qtd_vinc_atv, 
              vinc_sal_priv_limpos2[vinc_sal_priv_limpos2$nivel_tec == 5,]$produtividade,
              "normal"), col = 7)
legend("topleft", legend = c("todos", "0", "1", "2", "3", "4", "5"), 
       col = c("black", 2, 3, 4, 5, 6, 7), lwd = 2, title = "Nível tecnológico")

# ------------------------------------------------------------------------------
#                                MQO
# ------------------------------------------------------------------------------

mqo_geral1 <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                   raca_cor_med + idade_med + tempo_emprego_med + 
                   ate_fundamental_med + superior_med, data = base_privado)
summary(mqo_geral1)

mqo_geral2 <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                   raca_cor_med + idade_med + tempo_emprego_med + 
                   hb_med + hn_med + mb_med + mn_med +
                   ate_fundamental_med + superior_med, data = base_privado)
summary(mqo_geral2)

mqo_geral_dummies <- lm(produtividade ~ tamanho + nivel_tec + idade + sexo_med + 
                          raca_cor_med + idade_med + tempo_emprego_med + 
                          ate_fundamental_med + superior_med, data = base_privado)
summary(mqo_geral_dummies)

mqo_geral3 <- lm(produtividade ~ qtd_vinc_atv + tamanho + nivel_tec + idade + 
                   sexo_med + raca_cor_med + idade_med + tempo_emprego_med + 
                   ate_fundamental_med + superior_med, data = base_privado)
summary(mqo_geral3)

mqo_MEI <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                raca_cor_med + idade_med + tempo_emprego_med + 
                ate_fundamental_med + superior_med, 
                data = subset(base_privado, tamanho_cat == "MEI"))
summary(mqo_MEI)

mqo_micro <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                  raca_cor_med + idade_med + tempo_emprego_med + 
                  ate_fundamental_med + superior_med, 
                  data = subset(base_privado, tamanho_cat == "micro"))
summary(mqo_micro)

mqo_pequena <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                    raca_cor_med + idade_med + tempo_emprego_med + 
                    ate_fundamental_med + superior_med, 
                    data = subset(base_privado, tamanho_cat == "pequena"))
summary(mqo_pequena)

mqo_media <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                  raca_cor_med + idade_med + tempo_emprego_med + 
                  ate_fundamental_med + superior_med, 
                  data = subset(base_privado, tamanho_cat == "média"))
summary(mqo_media)

mqo_grande <- lm(produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
                   raca_cor_med + idade_med + tempo_emprego_med + 
                   ate_fundamental_med + superior_med, 
                   data = subset(base_privado, tamanho_cat == "grande"))
summary(mqo_grande)





# ------------------------------------------------------------------------------
#                           Regressão quantílica
# ------------------------------------------------------------------------------

# Selecionar 0,1% aleatoriamente

amostra_privado <- base_privado %>% 
  sample_frac(0.0001)

quant_geral_10 <- rq(formula = produtividade ~ qtd_vinc_atv + tamanho + nivel_tec + idade + 
                                    sexo_med + raca_cor_med + idade_med +  
                                    tempo_emprego_med + ate_fundamental_med + 
                                    superior_med, tau = 0.1, method = "br", 
                                    data = amostra_privado)
summary(quant_geral_10)
plot(summary(quant_geral_10))
plot(quant_geral_10)

quant_geral_50 <- rq(produtividade ~ qtd_vinc_atv + tamanho + nivel_tec + idade + 
                                    sexo_med + raca_cor_med + idade_med + 
                                    tempo_emprego_med + ate_fundamental_med + 
                                    superior_med, tau = 0.5, method = "br", 
                                    data = base_privado)
summary(quant_geral_50)
plot(summary(quant_geral_50))

quant_geral_90 <- rq(produtividade ~ qtd_vinc_atv + tamanho + nivel_tec + idade + 
                                    sexo_med + raca_cor_med + idade_med + 
                                    tempo_emprego_med + ate_fundamental_med + 
                                    superior_med, tau = 0.9, method = "br", 
                                    data = base_privado)
summary(quant_geral_90)
plot(summary(quant_geral_90))

# ------------------------------------------------------------------------------
#                           Decomposição
# ------------------------------------------------------------------------------



# ==============================================================================
#                              FIM
# ==============================================================================




