# pfv-sh

**Modelo de Previsão de Geração Solar Fotovoltaica Semi-horária**

[![R ≥ 4.0](https://img.shields.io/badge/R-%E2%89%A5%204.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## 1. Visão Geral do Projeto

O `pfv-sh` é um pacote R para **previsão de geração solar fotovoltaica** com resolução semi-horária (30 minutos), desenvolvido para suportar a operação do Sistema Interligado Nacional (SIN) brasileiro.

### Funcionalidades Principais

- **Treinamento de modelos** de previsão de geração solar (ARIMAX, Físico-Estimado)
- **Previsão operacional** com horizontes de D+0 a D+9
- **Associação automática** de usinas às quadrículas dos modelos NWP
- **Identificação de períodos de geração** solar por usina
- **Interpolação temporal** de dados NWP para resolução semi-horária

### Escopo e Limitações

| Aspecto | Descrição |
|---------|-----------|
| **Cobertura temporal** | Previsões semi-horárias (48 pontos/dia) |
| **Horizonte** | D+0 a D+9 (até 10 dias à frente) |
| **Modelos NWP suportados** | GFS, ECMWF (configurável) |
| **Modelos de previsão** | ARIMAX, Físico-Estimado (regressão linear) |
| **Limitações conhecidas** | Requer dados históricos de geração; sensível a lacunas nos dados NWP |

---

## 2. Arquitetura do Sistema

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              ENTRADAS                                       │
├─────────────────┬─────────────────────┬─────────────────────────────────────┤
│  Geração        │  Irradiância        │  Cadastro de                        │
│  Observada      │  Prevista (NWP)     │  Usinas                             │
│  (MW)           │  (W/m²)             │  (lat, lon, cap)                    │
└────────┬────────┴──────────┬──────────┴──────────────────┬──────────────────┘
         │                   │                             │
         ▼                   ▼                             ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         PRÉ-PROCESSAMENTO                                   │
│  • Associação NWP ↔ Usina (distância euclidiana)                            │
│  • Interpolação para resolução semi-horária                                 │
│  • Preenchimento de lacunas de rodadas NWP                                  │
│  • Identificação de períodos com geração solar                              │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            MODELAGEM                                        │
│  ┌─────────────────────────┐    ┌─────────────────────────┐                 │
│  │       ARIMAX            │    │   Físico-Estimado       │                 │
│  │  • auto.arima()         │    │  • Regressão Linear     │                 │
│  │  • Variáveis exógenas   │    │    Simples (RLS)        │                 │
│  │    (irradiância)        │    │  • Regressão Linear     │                 │
│  │  • Seleção por AICc     │    │    Múltipla (RLM)       │                 │
│  └─────────────────────────┘    └─────────────────────────┘                 │
│                                                                             │
│  Treinamento: por usina × modelo NWP × horizonte × meia-hora                │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              SAÍDAS                                         │
│  • Artefatos de modelo (.rds) - modo train                                  │
│  • Previsões de geração (data.table) - modo predict                         │
│    Colunas: id_modelo_prev, id_usina, id_modelo_nwp, data_hora_rodada,      │
│             data_hora_previsao, valor                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Início Rápido

### 3.1 Pré-requisitos

- R ≥ 4.0
- Dependências gerenciadas via `renv`

### 3.2 Instalação Local

```bash
# Clone o repositório
git clone https://github.com/isabelafiuza/pfv-sh.git
cd pfv-sh

# Restaure as dependências
R -e "renv::restore()"

# Instale o pacote
R -e "remotes::install_local('.', dependencies = FALSE)"
```

### 3.3 Execução via Docker

```bash
# Build da imagem
docker build -t pfv-sh .

# Treinamento
docker run -v $(pwd)/data:/app/data -v $(pwd)/out:/app/out pfv-sh

# Previsão (ajuste o config.jsonc para mode: "predict")
docker run -v $(pwd)/data:/app/data -v $(pwd)/out:/app/out pfv-sh
```

### 3.4 Execução dos Testes

```bash
# Execute os testes unitários
R -e "devtools::test()"

# Ou via linha de comando
Rscript -e "testthat::test_local()"
```

---

## 4. Exemplos de Uso

### 4.1 Via Linha de Comando (CLI)

```bash
# Usando diretório de dados padrão
Rscript main.r --datadir ./data

# Usando diretório customizado
Rscript main.r --datadir /caminho/para/dados
```

### 4.2 Arquivo de Configuração

O arquivo `config.jsonc` no diretório de dados controla a execução:

```jsonc
{
    // Modo: "train" para treinamento, "predict" para previsão
    "mode": "train",
    
    // Caminhos de entrada/saída
    "input": "./data",
    "output": "./out",
    "artifact": ".",
    
    // Usinas (vazio = todas)
    "ids_usinas": ["BAUFI1"],
    
    // Data de referência (YYYY-MM-DD)
    "data_referencia": "2025-07-02",
    
    // Horizontes de previsão
    "horizonte_dias": ["D+0", "D+1"],
    
    // Modelos NWP
    "modelos_NWP": ["GFS"],
    
    // Configuração dos modelos de previsão
    "modelos_previsao": {
        "arimax": {
            "tipo": "arimax",
            "n_dias_treino": 360,
            "amos_min": 5
        },
        "fisico_estimado": {
            "tipo": "fisico_estimado",
            "n_dias_treino": 180,
            "amos_min": 5
        }
    },
    
    // Parâmetros para identificação de período de geração
    "parametros_periodo_geracao": {
        "fator_tolerancia_limite_superior_geracao": 1.1,
        "fator_tolerancia_limite_inferior_geracao": 0.01,
        "percentual_dias_geracao": 0.9
    }
}
```

### 4.3 Uso Programático em R

```r
library(pfv.sh)
library(pfvIO)

# Conectar ao mock de dados
conn <- conectamock_pfv("./data")

# Carregar e validar configuração
config <- get_config(conn)
config <- parse_config(config, conn)

# Executar treinamento
if (config$mode == "train") {
    train_main(config)
}

# Executar previsão
if (config$mode == "predict") {
    resultado <- predict_main(config)
    print(resultado)
}
```

---

## 5. Estrutura dos Dados

### 5.1 Dados de Entrada

| Arquivo | Descrição | Colunas Principais |
|---------|-----------|-------------------|
| `usinas.csv` | Cadastro das usinas | `id_usina`, `latitude`, `longitude`, `capacidade_instalada_MW` |
| `geracao_observada.csv` | Geração histórica | `id_usina`, `data_hora_observacao`, `valor` |
| `irradiancia_prevista.csv` | Previsões NWP | `id_modelo_nwp`, `latitude`, `longitude`, `data_hora_rodada`, `data_hora_previsao`, `valor` |
| `config.jsonc` | Configuração | Ver seção 4.2 |

### 5.2 Dados de Saída

**Modo Train:**
- `{id_usina}_modelos_ajustados.rds`: Artefatos dos modelos treinados

**Modo Predict:**
- `data.table` com colunas:
  - `id_modelo_prev`: Tipo do modelo (arimax, fisico_estimado)
  - `id_usina`: Identificador da usina
  - `id_modelo_nwp`: Modelo NWP utilizado
  - `data_hora_rodada`: Data/hora da rodada
  - `data_hora_previsao`: Data/hora da previsão
  - `valor`: Geração prevista (MW)

### 5.3 Convenções Temporais

- **Fuso horário**: UTC
- **Resolução**: Semi-horária (30 minutos)
- **Formato de data**: ISO 8601 (`YYYY-MM-DDTHH:MM:SSZ`)
- **Horizontes**: `D+0` (mesmo dia) a `D+9` (9 dias à frente)

---

## 6. Modelos de Previsão

### 6.1 ARIMAX

Modelo auto-regressivo integrado de média móvel com variáveis exógenas:

- **Variável dependente**: Geração normalizada
- **Variáveis exógenas**: Irradiância prevista (normalizada)
- **Seleção automática**: `auto.arima()` do pacote `forecast`
- **Critério de seleção**: AICc + erro médio absoluto

### 6.2 Físico-Estimado

Modelo baseado em regressão linear:

- **RLS (Regressão Linear Simples)**: Geração ~ Irradiância
- **RLM (Regressão Linear Múltipla)**: Geração ~ Irradiância + outras variáveis
- **Critério de seleção**: Menor erro médio absoluto

### 6.3 Baseline

Para comparação, utilize a persistência (última geração observada no mesmo horário).

---

## 7. Limitações Conhecidas

1. **Lacunas de dados NWP**: Preenchidas com `NA`; podem impactar previsões
2. **Horários de baixa geração**: Excluídos do treinamento (fora do período solar)
3. **Dados insuficientes**: Modelo dummy retornado se `n_dados < amos_min`
4. **Validação cruzada**: Não implementada (walk-forward validation planejado)

---

## 8. Licença

Este projeto está licenciado sob a licença MIT. Veja [LICENSE](LICENSE) para detalhes.

---

## 9. Contribuindo

Veja [CONTRIBUTING.md](CONTRIBUTING.md) para diretrizes de contribuição.

---

## 10. Referências

- [pfvIO](https://github.com/lkhenayfis/pfvIO) - Pacote de I/O para dados de previsão fotovoltaica
