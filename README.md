# pfv-sh

[![R-CMD-check](https://github.com/isabelafiuza/pfv-sh/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/isabelafiuza/pfv-sh/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/isabelafiuza/pfv-sh/graph/badge.svg?token=TOKEN_AQUI)](https://codecov.io/gh/isabelafiuza/pfv-sh)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Modelo de Previsão de Geração Solar Fotovoltaica Semi-horária**

Desenvolvido pelo [Operador Nacional do Sistema Elétrico (ONS)](https://www.ons.org.br/) para uso em previsão operacional de geração renovável.

---

## Visão Geral

O `pfv-sh` é um pacote R para **previsão de geração solar fotovoltaica** com resolução semi-horária (30 minutos), desenvolvido para apoiar a programação da operação do Sistema Interligado Nacional (SIN) brasileiro.

Este pacote implementa um pipeline de treinamento e previsão que:

- **Treina modelos** de previsão de geração solar (ARIMAX, Físico-Estimado)
- **Executa previsão operacional** com horizontes de D+0 a D+9
- **Associa automaticamente** usinas às quadrículas dos modelos NWP
- **Identifica períodos de geração** solar por usina
- **Interpola temporalmente** dados NWP para resolução semi-horária
- **Rastreia proveniência** com run IDs, checksums de configuração e checkpoints para retomada
- **Coleta métricas** de desempenho por usina e gera relatórios de saúde do pipeline

### Casos de Uso

- Previsão operacional de geração solar para programação da operação do SIN
- Treinamento de modelos de previsão semi-horária para usinas fotovoltaicas
- Avaliação comparativa de modelos ARIMAX vs. Físico-Estimado
- Análise de desempenho de usinas solares em diferentes horizontes de previsão

### Escopo e Limitações

| Aspecto                    | Descrição                                                            |
| -------------------------- | -------------------------------------------------------------------- |
| **Cobertura temporal**     | Previsões semi-horárias (48 pontos/dia)                              |
| **Horizonte**              | D+0 a D+9 (10 dias de previsão)                                     |
| **Modelos NWP suportados** | GFS, ECMWF (configurável)                                            |
| **Modelos de previsão**    | ARIMAX, Físico-Estimado (regressão linear)                           |
| **Limitações conhecidas**  | Requer dados históricos de geração; sensível a lacunas nos dados NWP |

---

## Arquitetura

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              ENTRADAS                                       │
├─────────────────┬─────────────────────┬─────────────────────────────────────┤
│  Geração        │  Irradiância        │  Cadastro de                        │
│  Observada      │  Prevista (NWP)     │  Usinas                             │
│  (MW)           │  (W/m²)             │  (lat, lon, cap. instalada)         │
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
                 ┌───────────────┴────────────┐
                 │                            │
                 ▼                            ▼
┌────────────────────────────────┐ ┌────────────────────────────────┐
│     MODO: TRAIN (train_main)   │ │   MODO: PREDICT (predict_main) │
│                                │ │                                │
│  ARIMA/ARIMAX →                │ │  Pré-processamento NWP →       │
│  Físico-Estimado →             │ │  Aplicação dos modelos →       │
│  Artefato c/ metadados         │ │  Previsões de geração (MW)     │
└────────────┬───────────────────┘ └────────────┬───────────────────┘
             │                                  │
             └──────────────┬───────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         OBSERVABILIDADE                                     │
│  Proveniência (JSON) │ Métricas (JSON) │ Relatório de Saúde (JSON)          │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Componentes Principais

| Módulo            | Descrição                                                       |
| ----------------- | --------------------------------------------------------------- |
| `cli.r`           | Entry point do pacote (`cli_main`), parsing de flags e env vars |
| `config-file.r`   | Parsing e validação do arquivo de configuração                  |
| `train.r`         | Pipeline de treinamento com suporte a paralelismo e retomada    |
| `predict.r`       | Pipeline de previsão com suporte a paralelismo e retomada       |
| `parallel.r`      | Gestão do backend paralelo (`future`/`future.apply`)            |
| `artifact.r`      | Construção e validação de artefatos de modelo enriquecidos      |
| `provenance.r`    | Rastreabilidade de execução, checkpoints e retomada             |
| `metrics.r`       | Coleta de métricas por usina e agregados do pipeline            |
| `health-report.r` | Classificação de saúde por usina e do pipeline                  |
| `logging (zzz.r)` | Logging estruturado com contexto (run_id, mode)                 |
| `utils.r`         | Funções auxiliares (interpolação, associação NWP-usina)         |
| `combinacao.r`    | Combinação de previsões por modelo e horizonte                  |

---

## Quick Start

### Pré-requisitos

- R >= 4.0
- [renv](https://rstudio.github.io/renv/) para gerenciamento de dependências

### Instalação

```bash
# Instale o pacote usando remotes para desenvolvimento (branch main)
Rscript -e "remotes::install_github(\"isabelafiuza/pfv-sh\")"

# Instale o pacote usando remotes de uma tag específica (para uso)
Rscript -e "remotes::install_github(\"isabelafiuza/pfv-sh@release\")"
```

### Execução Rápida

```bash
# 1. Prepare seus dados no diretório ./data (veja seção "Dados de Entrada")

# 2. Configure o arquivo config.jsonc

# 3. Execute o treinamento
Rscript main.r --datadir ./data

# 4. Altere mode para "predict" no config.jsonc e execute
Rscript main.r --datadir ./data
```

### Execução com Paralelismo e Retomada

```bash
# Treinamento paralelo com 4 workers
Rscript main.r --datadir ./data --parallel --workers 4

# Retomada após falha (reprocessa apenas usinas pendentes)
Rscript main.r --datadir ./data --parallel --resume
```

### Usando Docker

```bash
# Build da imagem
docker build -t pfv-sh .

# Execução com volumes montados
docker run -v $(pwd)/data:/app/data -v $(pwd)/out:/app/out pfv-sh --datadir /app/data

# Execução paralela com variáveis de ambiente
docker run \
  -e PFVSH_PARALLEL=true \
  -e PFVSH_WORKERS=4 \
  -e PFVSH_RESUME=true \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/out:/app/out \
  pfv-sh --datadir /app/data
```

---

## Uso Detalhado

### Linha de Comando

```bash
Rscript main.r --datadir <DIRETÓRIO> [--parallel] [--resume] [--workers N]
```

| Argumento    | Descrição                                            | Default  |
| ------------ | ---------------------------------------------------- | -------- |
| `--datadir`  | Diretório contendo dados de entrada e `config.jsonc` | `./data` |
| `--parallel` | Habilita processamento paralelo de usinas            | `FALSE`  |
| `--resume`   | Retoma execução a partir do último checkpoint        | `FALSE`  |
| `--workers`  | Número de workers paralelos (requer `--parallel`)    | auto     |

### Variáveis de Ambiente

| Variável          | Descrição                    | Valores                          |
| ----------------- | ---------------------------- | -------------------------------- |
| `LOG_LEVEL`       | Nível de log                 | `debug`, `info`, `warn`, `error` |
| `PFVSH_PARALLEL`  | Habilita paralelismo via env | `true`/`false`                   |
| `PFVSH_RESUME`    | Habilita retomada via env    | `true`/`false`                   |
| `PFVSH_WORKERS`   | Número de workers via env    | inteiro positivo                 |

A resolução de prioridade para flags é: **argumento CLI** > **variável de ambiente** > **valor padrão**.

```bash
LOG_LEVEL=debug Rscript main.r --datadir ./data
```

### Arquivo de Configuração (`config.jsonc`)

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

### Uso Programático em R

```r
library(pfvsh)
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

## Dados de Entrada

O diretório de dados deve conter os seguintes arquivos:

| Arquivo                        | Formato        | Descrição                                     |
| ------------------------------ | -------------- | --------------------------------------------- |
| `config.jsonc`                 | JSONC          | Configuração do modelo                        |
| `usinas.parquet`               | Parquet ou CSV | Cadastro de usinas (id, lat, lon, capacidade) |
| `geracao_observada.parquet`    | Parquet ou CSV | Série temporal de geração por fonte           |
| `irradiancia_prevista.parquet` | Parquet ou CSV | Previsões NWP de irradiância                  |

### Schemas de Dados

#### `usinas.parquet`

```
id_usina,latitude,longitude,capacidade_instalada_MW,data_inicio_operacao_comercial
USINA_A,-23.5505,-46.6333,100.0,2020-01-01 12:00:00
```

#### `geracao_observada.parquet`

```
id_fonte_observacao,id_usina,data_hora_observacao,valor,status
PI,USINA_A,2024-01-01 00:00:00,45.2,0
```

#### `irradiancia_prevista.parquet`

```
id_modelo_nwp,latitude,longitude,data_hora_rodada,data_hora_previsao,valor
GFS,-23.5,-46.5,2024-01-01 00:00:00,2024-01-01 12:00:00,850.5
```

---

## Saídas

### Dados de Resultado

**Modo Train:**

| Arquivo                        | Descrição                          |
| ------------------------------ | ---------------------------------- |
| `{id_usina}_modelos_ajustados.rds` | Artefato de modelo com metadados |

**Modo Predict:**

`data.table` com as seguintes colunas:

| Coluna               | Descrição                        |
| -------------------- | -------------------------------- |
| `id_modelo_prev`     | Tipo do modelo (arimax, fisico_estimado) |
| `id_usina`           | Identificador da usina           |
| `id_modelo_nwp`      | Modelo NWP utilizado             |
| `data_hora_rodada`   | Data/hora da rodada              |
| `data_hora_previsao` | Data/hora da previsão            |
| `valor`              | Geração prevista (MW)            |

### Artefatos de Observabilidade

Cada execução do pipeline produz adicionalmente os seguintes artefatos. Em modo train são gravados no diretório `artifact`; em modo predict, no diretório `output`.

| Arquivo                            | Descrição                                                                 |
| ---------------------------------- | ------------------------------------------------------------------------- |
| `{id_usina}_modelos_ajustados.rds` | Artefato de modelo com metadados — modo train (no dir artifact)           |
| `provenance-{run_id}.json`         | Registro de proveniência com status por usina e timestamps                |
| `metrics-{run_id}.json`            | Métricas por usina (duração, volume de dados, qualidade do modelo)        |
| `health-{run_id}.json`             | Relatório de saúde (healthy/degraded/failed) com avisos e erros           |
| `checkpoint-{run_id}.json`         | Checkpoint para retomada (removido após conclusão bem-sucedida)           |

---

## Convenções Temporais

- **Fuso horário**: UTC
- **Resolução**: Semi-horária (30 minutos)
- **Formato de data**: ISO 8601 (`YYYY-MM-DDTHH:MM:SSZ`)
- **Horizontes**: `D+0` (mesmo dia) a `D+9` (9 dias à frente)

---

## Modelos de Previsão

### ARIMA/ARIMAX

Modelo auto-regressivo integrado de média móvel:

- **Variável dependente**: Geração normalizada
- **Variáveis exógenas**: Irradiância prevista normalizada (apenas ARIMAX)
- **Seleção automática**: `auto.arima()` do pacote `forecast`
- **Critério de seleção**: AICc + erro médio absoluto

### Físico-Estimado

Modelo baseado em regressão linear:

- **RLS (Regressão Linear Simples)**: Geração ~ Irradiância
- **RLM (Regressão Linear Múltipla)**: Geração ~ Irradiância + outras variáveis
- **Critério de seleção**: Menor erro médio absoluto

---

## Metodologia

### Retomada de Execução

Quando `--resume` está habilitado:

1. O pipeline verifica checkpoints existentes no diretório de saída
2. Valida o hash da configuração (rejeita checkpoints de configurações diferentes)
3. Reprocessa apenas usinas pendentes, carregando resultados intermediários do disco
4. Remove checkpoints e resultados intermediários após conclusão bem-sucedida

---

## Testes

```bash
# Executar testes unitários
Rscript -e "devtools::test()"

# Verificação completa do pacote
Rscript -e "devtools::check()"

# Linting (inclui complexidade ciclomática)
Rscript -e "lintr::lint_package()"

# Cobertura de testes
Rscript -e "covr::package_coverage()"
```

---

## Contribuindo

Contribuições são bem-vindas! Por favor, leia o [CONTRIBUTING.md](CONTRIBUTING.md) para detalhes sobre:

- Configuração do ambiente de desenvolvimento
- Padrões de código e estilo
- Processo de submissão de Pull Requests

---

## Licença

Este projeto está licenciado sob a Licença MIT - veja o arquivo [LICENSE](LICENSE) para detalhes.

---

## Documentação Adicional

- [ARCHITECTURE.md](ARCHITECTURE.md) - Detalhes da arquitetura da aplicação
- [CONTRIBUTING.md](CONTRIBUTING.md) - Diretrizes para contribuição
- [CHANGELOG.md](CHANGELOG.md) - Histórico de versões

---

## Contato

- **Organização**: [ONS - Operador Nacional do Sistema Elétrico](https://www.ons.org.br/)
- **Issues**: [GitHub Issues](https://github.com/isabelafiuza/pfv-sh/issues)

## Citação

```bibtex
@software{pfvsh2025,
  author = {{ONS - Operador Nacional do Sistema Elétrico}},
  title = {pfv-sh: Modelo de Previsão de Geração Solar Fotovoltaica Semi-horária},
  year = {2025},
  url = {https://github.com/isabelafiuza/pfv-sh},
  version = {0.2.0}
}
```
