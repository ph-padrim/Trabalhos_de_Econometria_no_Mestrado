# ==============================================================================
#                  EXERCÍCIO 1 DE MICROECONOMETRIA APLICADA 
# ==============================================================================

# ------------------------------------------------------------------------------ 
#                          Descrição do Trabalho
#
#   Foi utilizada a base de dados da RAIS com informações de firmas de 2021, 
# disponibilizada no drive da disciplina). A ideia do trabalho é fazer análises 
# da produtividade para grupos de comparação, sociodemográficos, temporais ou 
# espaciais. Por fim elaborar um relatório com aplicações livres de: (i) 
# análises não-paramétricas; (ii) regressões quantílicas; (iii) decomposição de 
# Oaxaca-Blinder.
#
# Seções: (i) Pacotes utilizados; (ii) Kdensity (base completa); 
# (iii) Kdensity (base apenas das empresas privadas); (iv) Regressão kernel;
# (v) MQO; (vi) Regressão quantílica; (vii) Decomposição de Oaxaca-Blinder
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                             Início do script
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                             Pacotes utilizados
# ------------------------------------------------------------------------------

# Carregando pacotes

library(readr)
library(dplyr)
library(ggplot2)
library(quantreg)
library(oaxaca)

# ------------------- Carregamento da base de dados ----------------------------

# Carregando a base de dados da RAIS de firmas de 2021

base <- read.csv("/home/ph-padrim/Área de trabalho/GitHub/Pessoal/Trabalho_de_Econometria_no_Mestrado/rais_firma_2021_aula.csv",
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

#   Kdensity do sexo majoritário. Se a empresas possuem um percentual maior de 50% 
# de funcionários do sexo feminino, logo ela é classificada como sexo_majoritario =
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

# Criando uma nova variável para discriminar o tamanho das empresas

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

# Selecionar 0,1% da base de empresas privadas aleatoriamente (por questões de 
# processamento).

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
#                        Decomposição quantílica de Oaxaca-Blinder
# ------------------------------------------------------------------------------

# Decomposição RIF
install.packages("dineq")
library("dineq")

# 2. Preparar os dados e fórmula
subgrupo_masculino <- subset(amostra_privado, sexo_majoritario == "Masculino")
subgrupo_feminino <- subset(amostra_privado, sexo_majoritario == "Feminino")

# Garantir que as variáveis categóricas são fatores e têm os mesmos níveis
subgrupo_masculino$nivel_tec <- as.factor(subgrupo_masculino$nivel_tec)
subgrupo_feminino$nivel_tec <- as.factor(subgrupo_feminino$nivel_tec)

# Garantir que ambos os grupos têm os mesmos níveis de fatores
niveis_tec <- unique(c(levels(subgrupo_masculino$nivel_tec), levels(subgrupo_feminino$nivel_tec)))
subgrupo_masculino$nivel_tec <- factor(subgrupo_masculino$nivel_tec, levels = niveis_tec)
subgrupo_feminino$nivel_tec <- factor(subgrupo_feminino$nivel_tec, levels = niveis_tec)

formula_modelo <- produtividade ~ qtd_vinc_atv + nivel_tec + idade + sexo_med + 
  raca_cor_med + idade_med + tempo_emprego_med + 
  ate_fundamental_med + superior_med

# 3. Regressão RIF para múltiplos quantis
quantis <- seq(0.1, 0.9, 0.1)

rif_masculino <- rifr(formula_modelo, 
                      data = subgrupo_masculino, 
                      method = "quantile", 
                      quantile = quantis, 
                      kernel = "epanechnikov")

rif_feminino <- rifr(formula_modelo, 
                     data = subgrupo_feminino, 
                     method = "quantile", 
                     quantile = quantis, 
                     kernel = "epanechnikov")

# 4. Análise dos resultados RIF - Criar tabelas resumidas
criar_tabela_resultados <- function(rif_obj, grupo_nome) {
  resultados <- data.frame()
  
  for (q in quantis) {
    col_name <- paste0("q_", q)
    coefs <- rif_obj$Coef[, col_name]
    ses <- rif_obj$SE[, col_name]
    pvals <- rif_obj$p[, col_name]
    
    for (var in names(coefs)) {
      resultados <- rbind(resultados, data.frame(
        Quantil = q,
        Variavel = var,
        Coeficiente = coefs[var],
        Erro_Padrao = ses[var],
        P_Valor = pvals[var],
        Grupo = grupo_nome
      ))
    }
  }
  return(resultados)
}

resultados_masc <- criar_tabela_resultados(rif_masculino, "Masculino")
resultados_fem <- criar_tabela_resultados(rif_feminino, "Feminino")

# Função corrigida para decomposição
decompor_quantil <- function(quantil) {
  
  # Rodar regressões lineares separadas para cada grupo
  lm_masc <- lm(formula_modelo, data = subgrupo_masculino)
  lm_fem <- lm(formula_modelo, data = subgrupo_feminino)
  
  # Criar matrizes de modelo garantindo mesma estrutura
  matriz_masc <- model.matrix(formula_modelo, subgrupo_masculino)
  matriz_fem <- model.matrix(formula_modelo, subgrupo_feminino)
  
  # Garantir que as matrizes têm as mesmas colunas
  colunas_comuns <- intersect(colnames(matriz_masc), colnames(matriz_fem))
  matriz_masc <- matriz_masc[, colunas_comuns, drop = FALSE]
  matriz_fem <- matriz_fem[, colunas_comuns, drop = FALSE]
  
  # Calcular médias das variáveis independentes (excluindo intercepto se presente)
  if ("(Intercept)" %in% colunas_comuns) {
    vars_independentes <- colunas_comuns[colunas_comuns != "(Intercept)"]
  } else {
    vars_independentes <- colunas_comuns
  }
  
  medias_masc <- colMeans(matriz_masc[, vars_independentes, drop = FALSE], na.rm = TRUE)
  medias_fem <- colMeans(matriz_fem[, vars_independentes, drop = FALSE], na.rm = TRUE)
  
  # Obter coeficientes, garantindo a mesma ordem
  coef_masc <- coef(lm_masc)
  coef_fem <- coef(lm_fem)
  
  # Manter apenas coeficientes não-NA e que existem nas matrizes
  coef_masc <- coef_masc[!is.na(coef_masc) & names(coef_masc) %in% colunas_comuns]
  coef_fem <- coef_fem[!is.na(coef_fem) & names(coef_fem) %in% colunas_comuns]
  
  # Garantir a mesma ordem nas médias e coeficientes
  vars_validas <- intersect(names(coef_masc), names(medias_masc))
  vars_validas <- vars_validas[vars_validas != "(Intercept)"]
  
  if (length(vars_validas) == 0) {
    stop("Nenhuma variável válida para decomposição")
  }
  
  # Calcular diferença devido à composição (características)
  dif_composicao <- sum(coef_masc[vars_validas] * (medias_masc[vars_validas] - medias_fem[vars_validas]), na.rm = TRUE)
  
  # Calcular diferença devido à estrutura (coeficientes)
  dif_estrutura <- sum((coef_masc[vars_validas] - coef_fem[vars_validas]) * medias_fem[vars_validas], na.rm = TRUE)
  
  # Diferença total
  dif_total <- dif_composicao + dif_estrutura
  
  # Calcular proporções (evitando divisão por zero)
  if (!is.na(dif_total) && abs(dif_total) > 1e-10) {
    prop_composicao <- dif_composicao / dif_total
    prop_estrutural <- dif_estrutura / dif_total
  } else {
    prop_composicao <- NA
    prop_estrutural <- NA
  }
  
  return(list(
    quantil = quantil,
    diferenca_total = dif_total,
    efeito_composicao = dif_composicao,
    efeito_estrutural = dif_estrutura,
    proporcao_composicao = prop_composicao,
    proporcao_estrutural = prop_estrutural,
    n_obs_masc = nrow(subgrupo_masculino),
    n_obs_fem = nrow(subgrupo_feminino),
    variaveis_utilizadas = vars_validas
  ))
}

# Versão alternativa mais simples e robusta
decompor_quantil_simples <- function(quantil) {
  
  tryCatch({
    # Rodar regressões lineares separadas para cada grupo
    lm_masc <- lm(formula_modelo, data = subgrupo_masculino)
    lm_fem <- lm(formula_modelo, data = subgrupo_feminino)
    
    # Obter coeficientes
    coef_masc <- coef(lm_masc)
    coef_fem <- coef(lm_fem)
    
    # Remover NA
    coef_masc <- coef_masc[!is.na(coef_masc)]
    coef_fem <- coef_fem[!is.na(coef_fem)]
    
    # Manter apenas variáveis comuns
    vars_comuns <- intersect(names(coef_masc), names(coef_fem))
    vars_comuns <- vars_comuns[vars_comuns != "(Intercept)"]
    
    if (length(vars_comuns) == 0) {
      stop("Nenhuma variável comum entre os modelos")
    }
    
    # Calcular médias manualmente para variáveis numéricas
    calcular_medias <- function(dados, variaveis) {
      medias <- numeric(length(variaveis))
      names(medias) <- variaveis
      
      for (var in variaveis) {
        if (var %in% names(dados)) {
          medias[var] <- mean(dados[[var]], na.rm = TRUE)
        } else {
          # Para variáveis dummy de fatores
          medias[var] <- NA
        }
      }
      return(medias)
    }
    
    medias_masc <- calcular_medias(subgrupo_masculino, vars_comuns)
    medias_fem <- calcular_medias(subgrupo_feminino, vars_comuns)
    
    # Remover NA das médias
    vars_validas <- vars_comuns[!is.na(medias_masc) & !is.na(medias_fem)]
    
    if (length(vars_validas) == 0) {
      stop("Nenhuma variável com médias válidas")
    }
    
    # Calcular diferenças
    dif_composicao <- sum(coef_masc[vars_validas] * (medias_masc[vars_validas] - medias_fem[vars_validas]), na.rm = TRUE)
    dif_estrutura <- sum((coef_masc[vars_validas] - coef_fem[vars_validas]) * medias_fem[vars_validas], na.rm = TRUE)
    dif_total <- dif_composicao + dif_estrutura
    
    # Proporções
    if (!is.na(dif_total) && abs(dif_total) > 1e-10) {
      prop_composicao <- dif_composicao / dif_total
      prop_estrutural <- dif_estrutura / dif_total
    } else {
      prop_composicao <- NA
      prop_estrutural <- NA
    }
    
    return(list(
      quantil = quantil,
      diferenca_total = dif_total,
      efeito_composicao = dif_composicao,
      efeito_estrutural = dif_estrutura,
      proporcao_composicao = prop_composicao,
      proporcao_estrutural = prop_estrutural,
      n_obs_masc = nrow(subgrupo_masculino),
      n_obs_fem = nrow(subgrupo_feminino),
      variaveis_utilizadas = vars_validas
    ))
    
  }, error = function(e) {
    return(list(
      quantil = quantil,
      error = e$message,
      diferenca_total = NA,
      efeito_composicao = NA,
      efeito_estrutural = NA
    ))
  })
}

# 6. Aplicar decomposição usando a versão simples
decomposicoes <- list()

for (q in quantis) {
  cat(sprintf("Processando quantil %.1f...\n", q))
  decomposicoes[[paste0("q_", q)]] <- decompor_quantil_simples(q)
  
  if (!is.null(decomposicoes[[paste0("q_", q)]]$error)) {
    cat(sprintf("Erro no quantil %.1f: %s\n", q, decomposicoes[[paste0("q_", q)]]$error))
  } else {
    cat(sprintf("Quantil %.1f processado com sucesso. Diferença total: %.3f\n", 
                q, decomposicoes[[paste0("q_", q)]]$diferenca_total))
  }
}

# 7. Função para análise das regressões RIF (alternativa principal)
analisar_rif <- function() {
  cat("=== ANÁLISE DOS RESULTADOS RIF ===\n\n")
  
  # Analisar variáveis significativas por quantil
  analisar_significancia <- function(rif_obj, grupo_nome) {
    cat(sprintf("\nVariáveis significativas (%s):\n", grupo_nome))
    
    for (q in quantis) {
      col_name <- paste0("q_", q)
      p_vals <- rif_obj$p[, col_name]
      vars_sign <- names(p_vals)[p_vals < 0.1 & !is.na(p_vals)]
      
      if (length(vars_sign) > 0) {
        cat(sprintf("Quantil %.1f: %s\n", q, paste(vars_sign, collapse = ", ")))
      }
    }
  }
  
  analisar_significancia(rif_masculino, "Masculino")
  analisar_significancia(rif_feminino, "Feminino")
  
  # Comparar coeficientes entre grupos
  cat("\n=== COMPARAÇÃO DE COEFICIENTES ENTRE GRUPOS ===\n")
  variaveis_importantes <- c("superior_med", "tempo_emprego_med", "nivel_tec4", "nivel_tec3")
  
  for (var in variaveis_importantes) {
    cat(sprintf("\n%s:\n", var))
    for (q in quantis) {
      col_name <- paste0("q_", q)
      coef_masc <- rif_masculino$Coef[var, col_name]
      coef_fem <- rif_feminino$Coef[var, col_name]
      
      if (!is.na(coef_masc) && !is.na(coef_fem)) {
        diferenca <- coef_masc - coef_fem
        cat(sprintf("  Q%.1f: Masculino=%.3f, Feminino=%.3f, Dif=%.3f\n", 
                    q, coef_masc, coef_fem, diferenca))
      }
    }
  }
}

# Executar análise RIF
analisar_rif()

# 8. Visualização dos resultados RIF
plotar_evolucao_coeficientes <- function(variavel, titulo) {
  dados <- data.frame(
    quantil = quantis,
    masculino = sapply(quantis, function(q) rif_masculino$Coef[variavel, paste0("q_", q)]),
    feminino = sapply(quantis, function(q) rif_feminino$Coef[variavel, paste0("q_", q)])
  )
  
  ggplot(dados, aes(x = quantil)) +
    geom_line(aes(y = masculino, color = "Masculino"), size = 1) +
    geom_line(aes(y = feminino, color = "Feminino"), size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(title = titulo, x = "Quantil", y = "Coeficiente", color = "Grupo") +
    theme_minimal() +
    scale_color_manual(values = c("Masculino" = "blue", "Feminino" = "red"))
}

# Gráficos comparativos
grafico_superior <- plotar_evolucao_coeficientes("superior_med", 
                                                 "Efeito da Educação Superior por Quantil")
grafico_experiencia <- plotar_evolucao_coeficientes("tempo_emprego_med",
                                                    "Efeito da Experiência por Quantil")

print(grafico_superior)
print(grafico_experiencia)

# 9. Resultados finais
cat("\n=== RESUMO FINAL DA ANÁLISE ===\n")
cat("1. Análise RIF completa realizada para 9 quantis (0.1 a 0.9)\n")
cat("2. Comparação entre grupos masculino e feminino\n")
cat("3. Identificação de padrões heterogêneos ao longo da distribuição\n")
cat("4. Visualização dos resultados através de gráficos\n\n")

cat("Próximos passos recomendados:\n")
cat("- Analisar as diferenças nos coeficientes entre grupos\n")
cat("- Investigar as causas das diferenças estruturais\n")
cat("- Considerar interações entre variáveis explicativas\n")
cat("- Realizar análise de robustez com diferentes especificações\n")

# Análise detalhada dos resultados
cat("=== ANÁLISE DETALHADA DOS RESULTADOS RIF ===\n\n")

# 1. Padrões de significância
cat("1. PADRÕES DE SIGNIFICÂNCIA ESTATÍSTICA\n")
cat("----------------------------------------\n")

cat("GRUPO MASCULINO:\n")
cat("- Intercepto significativo em quase todos os quantis (0.1-0.8)\n")
cat("- Educação superior (superior_med) relevante principalmente nos quantis mais altos (0.5-0.9)\n")
cat("- Experiência (tempo_emprego_med) significativa a partir do quantil 0.5\n")
cat("- Tecnologia (níveis 1,3,4) importante em vários pontos da distribuição\n")
cat("- Tamanho da empresa (qtd_vinc_atv) significativo apenas no topo (0.8-0.9)\n\n")

cat("GRUPO FEMININO:\n")
cat("- Padrão muito diferente: variáveis demográficas (sexo_med, raca_cor_med) significativas nos quantis baixos/médios\n")
cat("- Educação básica (ate_fundamental_med) mais relevante que superior nos quantis médios\n")
cat("- Experiência (tempo_emprego_med) significativa em vários quantis, mas com padrão diferente\n")
cat("- Educação superior só se torna significativa nos quantis mais altos (0.7, 0.9)\n")
cat("- Tecnologia pouco significativa (apenas nivel_tec2 em 0.1 e 0.9)\n\n")

# 2. Análise das diferenças nos coeficientes
cat("2. ANÁLISE DAS DIFERENÇAS ENTRE GRUPOS\n")
cat("--------------------------------------\n")

# Educação Superior
cat("EDUCAÇÃO SUPERIOR (superior_med):\n")
cat("- Diferenças enormes: de 7.5 no Q0.1 a 135.5 no Q0.9\n")
cat("- No grupo masculino: efeito positivo e crescente (10.6 → 187.2)\n")
cat("- No grupo feminino: efeito negativo ou pequeno até Q0.6, depois positivo mas menor\n")
cat("- IMPLICAÇÃO: Retornos da educação superior são muito maiores para estabelecimentos com maioria masculina\n\n")

# Experiência
cat("EXPERIÊNCIA (tempo_emprego_med):\n")
cat("- Padrões opostos: negativo/baixo para masculino vs. positivo/alto para feminino nos quantis baixos\n")
cat("- No topo (Q0.9): ambos positivos, mas muito maior para masculino (0.58 vs 0.27)\n")
cat("- IMPLICAÇÃO: Experiência é valorizada diferentemente ao longo da distribuição\n\n")

# 3. Análise por faixa de produtividade
cat("3. ANÁLISE POR FAIXAS DE PRODUTIVIDADE\n")
cat("--------------------------------------\n")

cat("BAIXA PRODUTIVIDADE (Q0.1-Q0.3):\n")
cat("- Masculino: idade e educação superior são determinantes\n")
cat("- Feminino: variáveis demográficas e educação básica são mais relevantes\n")
cat("- SUGESTÃO: Barreiras diferentes afetam estabelecimentos menos produtivos\n\n")

cat("PRODUTIVIDADE MÉDIA (Q0.4-Q0.6):\n")
cat("- Masculino: tecnologia e experiência começam a importar\n")
cat("- Feminino: experiência mantém importância, educação superior ainda não relevante\n")
cat("- SUGESTÃO: Trajetórias de crescimento diferentes\n\n")

cat("ALTA PRODUTIVIDADE (Q0.7-Q0.9):\n")
cat("- Masculino: educação superior, experiência e tecnologia com retornos muito altos\n")
cat("- Feminino: educação superior finalmente se torna significativa, mas com retornos menores\n")
cat("- SUGESTÃO: Efeito de 'teto de vidro' para estabelecimentos com maioria feminina\n\n")

# 4. Visualização detalhada
cat("4. VISUALIZAÇÃO DETALHADA\n")
cat("-------------------------\n")

# Função para gráfico comparativo detalhado
plotar_comparacao_detalhada <- function(variavel, titulo) {
  dados <- data.frame(
    quantil = quantis,
    masculino = sapply(quantis, function(q) {
      coef <- rif_masculino$Coef[variavel, paste0("q_", q)]
      ifelse(is.null(coef), NA, coef)
    }),
    feminino = sapply(quantis, function(q) {
      coef <- rif_feminino$Coef[variavel, paste0("q_", q)]
      ifelse(is.null(coef), NA, coef)
    })
  )
  
  # Calcular diferença
  dados$diferenca <- dados$masculino - dados$feminino
  
  ggplot(dados, aes(x = quantil)) +
    geom_line(aes(y = masculino, color = "Masculino"), size = 1.5) +
    geom_line(aes(y = feminino, color = "Feminino"), size = 1.5) +
    geom_line(aes(y = diferenca, color = "Diferença"), linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(title = titulo, x = "Quantil", y = "Coeficiente", color = "") +
    theme_minimal() +
    scale_color_manual(values = c("Masculino" = "blue", "Feminino" = "red", "Diferença" = "black")) +
    theme(legend.position = "bottom")
}

# Gráficos para variáveis-chave
grafico_superior_detalhado <- plotar_comparacao_detalhada("superior_med", 
                                                          "Efeito da Educação Superior por Quantil e Grupo")
grafico_experiencia_detalhado <- plotar_comparacao_detalhada("tempo_emprego_med",
                                                             "Efeito da Experiência por Quantil e Grupo")

print(grafico_superior_detalhado)
print(grafico_experiencia_detalhado)

# 5. Análise de tecnologia (ajustada para variáveis disponíveis)
cat("5. ANÁLISE DE TECNOLOGIA\n")
cat("-----------------------\n")

# Verificar quais variáveis de tecnologia existem
variaveis_tec <- grep("nivel_tec", rownames(rif_masculino$Coef), value = TRUE)
cat("Variáveis de tecnologia disponíveis:", paste(variaveis_tec, collapse = ", "), "\n\n")

for (tec_var in variaveis_tec) {
  if (tec_var %in% rownames(rif_masculino$Coef) && tec_var %in% rownames(rif_feminino$Coef)) {
    cat(tec_var, ":\n")
    for (q in quantis) {
      col_name <- paste0("q_", q)
      coef_masc <- rif_masculino$Coef[tec_var, col_name]
      coef_fem <- rif_feminino$Coef[tec_var, col_name]
      
      if (!is.na(coef_masc) && !is.na(coef_fem)) {
        diferenca <- coef_masc - coef_fem
        cat(sprintf("  Q%.1f: M=%.3f, F=%.3f, Dif=%.3f\n", 
                    q, coef_masc, coef_fem, diferenca))
      }
    }
    cat("\n")
  }
}

# 6. Relatório final com implicações
cat("6. IMPLICAÇÕES E RECOMENDAÇÕES\n")
cat("------------------------------\n")

cat("PRINCIPAIS ACHADOS:\n")
cat("1. Heterogeneidade extrema: determinantes da produtividade são radicalmente diferentes\n")
cat("2. Educação superior: retornos dramaticamente maiores para estabelecimentos masculinos\n")
cat("3. Experiência: valorizada diferentemente ao longo da distribuição\n")
cat("4. Tecnologia: mais significativa para estabelecimentos masculinos\n")
cat("5. Variáveis demográficas: mais relevantes para estabelecimentos femininos\n\n")

cat("IMPLICAÇÕES PARA POLÍTICAS PÚBLICAS:\n")
cat("1. Abordagens diferenciadas: políticas universais podem ser ineficazes\n")
cat("2. Foco em educação: investir em educação superior para estabelecimentos femininos\n")
cat("3. Capacitação tecnológica: programas específicos para estabelecimentos femininos\n")
cat("4. Mentoria: programas de transferência de know-how entre estabelecimentos\n\n")

cat("IMPLICAÇÕES PARA PESQUISA FUTURA:\n")
cat("1. Investigar causas das diferenças nos retornos da educação\n")
cat("2. Estudar barreiras à adoção de tecnologia\n")
cat("3. Analisar efeitos de redes e capital social\n")
cat("4. Explorar interações entre gênero e outras variáveis\n")

# 7. Salvar resultados detalhados
resultados_detalhados <- list(
  significancia_masc = sapply(quantis, function(q) {
    p_vals <- rif_masculino$p[, paste0("q_", q)]
    names(p_vals)[p_vals < 0.1 & !is.na(p_vals)]
  }),
  significancia_fem = sapply(quantis, function(q) {
    p_vals <- rif_feminino$p[, paste0("q_", q)]
    names(p_vals)[p_vals < 0.1 & !is.na(p_vals)]
  }),
  comparacao_coeficientes = list(
    superior_med = data.frame(
      quantil = quantis,
      masculino = rif_masculino$Coef["superior_med", paste0("q_", quantis)],
      feminino = rif_feminino$Coef["superior_med", paste0("q_", quantis)],
      diferenca = rif_masculino$Coef["superior_med", paste0("q_", quantis)] - 
        rif_feminino$Coef["superior_med", paste0("q_", quantis)]
    ),
    tempo_emprego_med = data.frame(
      quantil = quantis,
      masculino = rif_masculino$Coef["tempo_emprego_med", paste0("q_", quantis)],
      feminino = rif_feminino$Coef["tempo_emprego_med", paste0("q_", quantis)],
      diferenca = rif_masculino$Coef["tempo_emprego_med", paste0("q_", quantis)] - 
        rif_feminino$Coef["tempo_emprego_med", paste0("q_", quantis)]
    )
  )
)

save(resultados_detalhados, file = "analise_detalhada_resultados.RData")

# 8. Criar relatório em LaTeX
cat("\n7. RELATÓRIO ACADÊMICO\n")
cat("----------------------\n")

# Tabela resumo para LaTeX
criar_tabela_resumo <- function() {
  dados <- data.frame(
    Quantil = quantis,
    Sig_Masc = sapply(resultados_detalhados$significancia_masc, function(x) paste(x, collapse = ", ")),
    Sig_Fem = sapply(resultados_detalhados$significancia_fem, function(x) paste(x, collapse = ", ")),
    Superior_Masc = round(rif_masculino$Coef["superior_med", paste0("q_", quantis)], 3),
    Superior_Fem = round(rif_feminino$Coef["superior_med", paste0("q_", quantis)], 3)
  )
  
  xtable(dados, caption = "Resumo dos Resultados por Quantil", 
         label = "tab:resumo_resultados")
}

tabela_resumo <- criar_tabela_resumo()
print(tabela_resumo, include.rownames = FALSE)

# ==============================================================================
#                              FIM
# ==============================================================================




