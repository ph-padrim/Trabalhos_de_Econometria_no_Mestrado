Projetos de Econometria
Este repositório contém uma coleção de projetos de econometria desenvolvidos, focando
na aplicação de diversas metodologias para análise de dados econômicos. Cada projeto
explora diferentes técnicas e abordagens para investigar questões relevantes no campo da
economia.
Sumário dos Trabalhos
1. Trabalho 1 de Microeconometria: Fatores da Produtividade das
Firmas no Brasil
Metodologia:
Este trabalho empregou uma estratégia econométrica em múltiplos estágios para analisar
os fatores da produtividade das firmas no Brasil, utilizando dados da RAIS 2021. As
principais metodologias incluem:
• Análise Descritiva: Utilização da Densidade de Kernel para visualizar e comparar
distribuições de produtividade entre diferentes grupos de firmas (público vs. privado,
por tamanho, MEI vs. outros, nível tecnológico e sexo majoritário).
• Modelos de Regressão: Aplicação de Mínimos Quadrados Ordinários (MQO) para
estimar o efeito médio das variáveis explicativas sobre a produtividade. Regressão
Quantílica foi utilizada para analisar como esses efeitos variam em diferentes pontos da
distribuição de produtividade (firmas menos e mais produtivas). Além disso, a
Regressão Kernel foi empregada para explorar a relação não-paramétrica entre
produtividade e número de vínculos empregatícios.
• Decomposição de Blinder-Oaxaca: Esta técnica foi aplicada para investigar o hiato de
produtividade entre firmas com maioria de funcionários homens e mulheres, dividindo
a diferença em componentes explicados (atribuíveis a diferenças nas características
observáveis) e não explicados (atribuíveis a diferenças nos retornos a essas
características).
2. Trabalho 2 de Econometria: Aplicação Prática de Endogeneidade e
Dados em Painel
Metodologia:
Este projeto focou na aplicação prática de técnicas para lidar com endogeneidade e na
modelagem de dados em painel, utilizando dados da RAIS entre 2011 e 2021. As
metodologias abordadas foram:
• Endogeneidade:
• Estimadores de Variáveis Instrumentais (IV): Utilizados para corrigir a
endogeneidade, buscando uma variável instrumental (Z) que seja exógena ao termo
de erro e relevante para a variável explicativa endógena.
• Mínimos Quadrados em 2 Estágios (MQ2E): Uma extensão do IV para múltiplas
variáveis endógenas ou instrumentos, realizada em duas etapas: regressão de cada
variável endógena em todas as variáveis exógenas (incluindo instrumentos) e, em
seguida, regressão da variável dependente nos valores preditos das variáveis
endógenas.
• Modelos para Dados em Painel:
• Pooled Cross-Sections: Trata dados de diferentes períodos como um único
conjunto de corte transversal, aplicando Mínimos Quadrados Ordinários (MQO).
• Diferenças-em-Diferenças (DiD): Utilizado para avaliar o impacto causal de
intervenções, comparando grupos de tratamento e controle antes e depois da
intervenção.
• Estimação de Efeitos Fixos (EF) e Aleatórios (EA): O EF controla a
heterogeneidade não observada correlacionada com as variáveis explicativas
(usando Within Transformation, First Differencing ou Dummy Variables). O EA
assume que a heterogeneidade não observada não é correlacionada e a trata como
um componente aleatório do termo de erro (estimado via FGLS). O Teste de
Hausman é empregado para comparar os estimadores de EF e EA.
3. Trabalho 1 de Econometria: Análise da Relação entre Variáveis e
Remuneração Média Real dos Trabalhadores
Metodologia:
Este trabalho utilizou o modelo de Mínimos Quadrados Ordinários (MQO) para analisar a
relação entre diversas variáveis explicativas e a remuneração média real dos trabalhadores
das firmas no Brasil, com base em dados da RAIS de 2021. A metodologia incluiu:
• Mínimos Quadrados Ordinários (MQO): O MQO foi aplicado para minimizar a soma
dos quadrados dos resíduos e obter os melhores estimadores lineares não viesados,
sob a condição de que as hipóteses clássicas sejam atendidas (linearidade nos
parâmetros, amostragem aleatória, média condicional zero do termo de erro, não
multicolinearidade perfeita, homocedasticidade, não autocorrelação dos erros, número
de observações maior que o número de parâmetros, variabilidade das variáveis
explicativas, ausência de viés de especificação e termo de erro com distribuição
normal). O trabalho detalha a derivação matricial dos estimadores de MQO e a
utilização de técnicas de inferência estatística como o coeficiente de determinação (R²),
teste t e teste F para verificar a significância e confiabilidade dos coeficientes estimados
e do modelo como um todo. Diversas especificações de equações foram testadas para
analisar as relações das variáveis explicativas com a variável dependente.
