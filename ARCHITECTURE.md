# Arquitetura do Sistema

Este documento descreve a arquitetura técnica do `pfvsh`, incluindo o fluxo de dados, componentes principais e decisões de design.

## 1. Visão Geral

O `pfvsh` é um pacote R para previsão de geração solar fotovoltaica semi-horária. Opera em dois modos:

1. **Train**: Ajusta modelos de previsão (plugáveis via S3 Strategy Pattern) usando dados históricos de geração e irradiância NWP
2. **Predict**: Gera previsões de geração aplicando modelos pré-treinados a novas entradas NWP

Ambos os modos suportam execução paralela via `future`/`future.apply`, retomada a partir de checkpoints, e emitem artefatos de observabilidade (proveniência, métricas, relatórios de saúde).

---

## 2. Diagrama de Fluxo

```
                          ┌─────────────────────┐
                          │   config.jsonc      │
                          │   (configuração)    │
                          └──────────┬──────────┘
                                     │
                                     ▼
                          ┌─────────────────────┐
                          │   main.r            │
                          │  → cli_main()       │
                          └──────────┬──────────┘
                                     │
                     ┌───────────────┴────────────┐
                     │                            │
                     ▼                            ▼
          ┌─────────────────────┐      ┌─────────────────────┐
          │   MODO: train       │      │   MODO: predict     │
          │   train_main()      │      │   predict_main()    │
          └──────────┬──────────┘      └──────────┬──────────┘
                     │                            │
  ┌──────────────────┼────────────────────────────┼──────────────────┐
  │                  │                            │                  │
  │       ┌──────────┴──────────┐     ┌──────────┴──────────┐        │
  │       │  treina_usina()     │     │  predict_plant_     │        │
  │       │  (por usina, ∥)     │     │  wrapper()          │        │
  │       └──────────┬──────────┘     │  (por usina, ∥)     │       │
  │                  │                └──────────┬──────────┘        │
  │                  │                           │                   │
  │     ┌────────────┴───────────┐               │                   │
  │     │                       │               ▼                   │
  │     ▼                       ▼     ┌──────────────────┐           │
  │  ┌──────────────┐  ┌──────────────┐  │ predict_usina()  │           │
  │  │ train_modelo │  │ parse_train.*│  │ parse_predict.*  │           │
  │  │ ()           │  │ (S3 dispatch)│  │ (S3 dispatch)   │           │
  │  └──────┬───────┘  └──────────────┘  └────────┬─────────┘           │
  │         │                                     │                   │
  │         ▼                                     │                   │
  │  ┌──────────────────────┐                     │                   │
  │  │  {usina}_modelos_    │◄────────────────────┤                   │
  │  │  ajustados.rds       │                     │                   │
  │  │  (artefato c/        │                     │                   │
  │  │   metadados)         │                     │                   │
  │  └──────────────────────┘                     │                   │
  │                                               ▼                   │
  │                                    ┌──────────────────┐            │
  │                                    │ previsao_geracao │            │
  │                                    │ _fotovoltaica    │            │
  │                                    │ .parquet         │            │
  │                                    └──────────────────┘            │
  └──────────────────────────────────────────────────────────────────┘
                               │
           ┌───────────────────┼───────────────┐
           │                   │               │
           ▼                   ▼               ▼
  ┌──────────────────┐ ┌──────────────┐ ┌──────────────┐
  │  provenance-     │ │ metrics-     │ │ health-      │
  │  {run_id}.json   │ │ {run_id}     │ │ {run_id}     │
  │                  │ │ .json        │ │ .json        │
  └──────────────────┘ └──────────────┘ └──────────────┘
  (train → artifact/  │ predict → out/)
```

---

## 3. Componentes Principais

### 1. Entry Point (`cli.r` / `main.r`)

`main.r` é o script de entrada que parseia argumentos CLI e delega para `cli_main()`.

`cli_main()` é a função principal do pacote, responsável por:

- Resolver parâmetros com precedência: argumento CLI > variável de ambiente > padrão
- Carregar e parsear configuração via `pfvIO`
- Despachar para modo `train` ou `predict`
- Configurar workers paralelos quando solicitado

| Parâmetro  | Flag CLI      | Variável de Ambiente | Padrão  |
| ---------- | ------------- | -------------------- | ------- |
| `parallel` | `--parallel`  | `PFVSH_PARALLEL`     | `FALSE` |
| `resume`   | `--resume`    | `PFVSH_RESUME`       | `FALSE` |
| `workers`  | `--workers N` | `PFVSH_WORKERS`      | auto    |

### 2. Configuração (`config-file.r`)

Gerencia parsing e validação do arquivo de configuração.

| Função                            | Descrição                                          |
| --------------------------------- | -------------------------------------------------- |
| `parse_config()`                  | Interpreta e valida configuração completa          |
| `resolve_config_paths()`          | Resolve caminhos relativos a partir do base dir    |
| `valida_nomes_config()`           | Verifica presença de chaves obrigatórias           |
| `valida_tipos_config()`           | Valida tipos de cada chave                         |
| `parsearg_data_referencia.*()`    | Interpreta janela temporal (S3 generic)            |
| `parsearg_ids_usinas()`           | Interpreta lista de usinas                         |
| `parsearg_horizonte_dias()`       | Interpreta horizonte de previsão                   |
| `parsearg_modelos_nwp()`          | Interpreta lista de modelos NWP                    |
| `parsearg_modelos_previsao()`     | Interpreta lista de modelos de previsão            |

### 3. Treinamento (`train.r`)

Calibra modelos para cada usina via `parse_train.*` S3 dispatch.

| Função                       | Descrição                                        |
| ---------------------------- | ------------------------------------------------ |
| `train_main()`               | Orquestra treinamento para todas as usinas       |
| `treina_usina()`             | Processa uma usina individual                    |
| `load_train_resume_state()`  | Carrega estado de retomada do checkpoint         |
| `tally_train_results()`      | Consolida resultados e emite observabilidade     |
| `parse_train.arimax()`       | Ajusta modelo ARIMAX (com/sem variáveis exógenas)|
| `parse_train.fisico_estimado()` | Ajusta regressão linear (RLS/RLM)             |

#### Artefato de Saída

```r
list(
    id_usina = "USINA_A",
    models = list(
        list(
            combinacao_ajuste = list(...),
            modelo = structure(..., class = "arimax"),
            erros = data.frame(...)
        ),
        ...
    ),
    metadata = list(
        type = "arimax",
        n_models = 96L,
        timestamp = "2025-01-01T12:00:00Z",
        package_version = "0.2.0",
        config_hash = "sha256:abc123..."
    )
)
```

### 4. Previsão (`predict.r`)

Gera previsões usando modelos pré-treinados via `parse_predict.*` S3 dispatch.

| Função                         | Descrição                                        |
| ------------------------------ | ------------------------------------------------ |
| `predict_main()`               | Orquestra previsão para todas as usinas          |
| `predict_plant_wrapper()`      | Wrapper por usina com tratamento de erros        |
| `predict_usina()`              | Processa uma usina individual                    |
| `load_predict_resume_state()`  | Carrega estado de retomada do checkpoint         |
| `parse_predict.arimax()`       | Aplica modelo ARIMAX (com recalibração)          |
| `parse_predict.fisico_estimado()` | Aplica regressão linear                       |

### 5. Combinação (`combinacao.r`)

Pós-processamento das previsões individuais.

| Função               | Descrição                                          |
| -------------------- | -------------------------------------------------- |
| `suaviza_previsao()` | Aplica suavização LOESS às previsões               |
| `combina_media()`    | Combina previsões de múltiplos modelos pela média  |

### 6. Paralelismo (`parallel.r`)

Gerencia o backend de execução paralela usando o pacote `future`.

| Função                  | Descrição                                          |
| ----------------------- | -------------------------------------------------- |
| `setup_parallel_plan()` | Configura plano paralelo (multisession/multicore)  |
| `reset_parallel_plan()` | Restaura plano anterior                            |
| `get_parallel_config()` | Consulta configuração ativa (workers, strategy)    |
| `run_plants()`          | Executa função por usina (sequencial ou paralelo)  |
| `split_args_by_plant()` | Particiona argumentos extras por usina             |

O número de workers é resolvido com a seguinte precedência:

1. Argumento explícito `workers`
2. Variável de ambiente `PFVSH_WORKERS`
3. Auto-detect: `future::availableCores() - 1` (mínimo 1)

### 7. Artefatos de Modelo (`artifact.r`)

Constrói e valida artefatos de modelo enriquecidos com metadados de proveniência.

| Função                        | Descrição                                              |
| ----------------------------- | ------------------------------------------------------ |
| `build_model_artifact()`      | Monta artefato completo (id + modelo + metadados)      |
| `validate_artifact()`         | Valida estrutura (aceita formato antigo sem metadados) |
| `normalize_config_for_hash()` | Normaliza config para hash determinístico              |

### 8. Proveniência e Checkpoints (`provenance.r`)

Rastreabilidade completa de cada execução do pipeline.

| Função                  | Descrição                                         |
| ----------------------- | ------------------------------------------------- |
| `generate_run_id()`     | Gera ID único `{mode}-{YYYYMMDD}-{HHMMSS}-{hex}` |
| `create_provenance()`   | Cria registro de proveniência inicial             |
| `update_plant_status()` | Atualiza status de uma usina                      |
| `finalize_provenance()` | Marca fim da execução com duração                 |
| `write_provenance()`    | Serializa proveniência como JSON                  |
| `write_checkpoint()`    | Persiste estado para retomada                     |
| `read_checkpoint()`     | Lê e valida checkpoint (verifica config hash)     |
| `get_pending_plants()`  | Retorna usinas pendentes de um checkpoint         |
| `write_plant_result()`  | Salva resultado intermediário de uma usina        |
| `read_plant_result()`   | Carrega resultado intermediário                   |
| `cleanup_checkpoint()`  | Remove checkpoints após conclusão                 |

#### Fluxo de Retomada

```
1. read_checkpoint() → valida config_hash
2. get_pending_plants() → identifica usinas não completadas
3. read_plant_result() → carrega resultados já processados
4. Processa apenas usinas pendentes
5. cleanup_checkpoint() → remove arquivos temporários
```

### 9. Métricas (`metrics.r`)

Coleta de métricas por usina e agregados do pipeline.

| Função                   | Descrição                                              |
| ------------------------ | ------------------------------------------------------ |
| `create_metrics()`       | Cria registro de métricas vazio                        |
| `record_plant_timing()`  | Registra duração de processamento por usina            |
| `record_model_quality()` | Registra qualidade do modelo (n_models, metadados)     |
| `finalize_metrics()`     | Computa agregados (mean/max/min duração)               |
| `write_metrics()`        | Serializa métricas como JSON                           |

### 10. Relatório de Saúde (`health-report.r`)

Classificação de saúde por usina e do pipeline.

| Função                      | Descrição                                          |
| --------------------------- | -------------------------------------------------- |
| `build_health_report()`     | Agrega proveniência e métricas em relatório        |
| `classify_plant_health()`   | Classifica usina: `healthy`/`warning`/`failed`     |
| `classify_overall_health()` | Classifica pipeline: `healthy`/`degraded`/`failed` |
| `write_health_report()`     | Serializa relatório como JSON                      |

Critérios de classificação:

- **failed**: proveniência com status != `"completed"`
- **healthy**: caso contrário

### 11. Logging (`logging.r` / `zzz.r`)

Logging estruturado com contexto de execução. O logger é inicializado em `.onLoad()` (em `zzz.r`) e disponibilizado via `get_pkg_logger()`.

| Função                | Descrição                                          |
| --------------------- | -------------------------------------------------- |
| `get_pkg_logger()`    | Retorna o logger do pacote                         |
| `set_log_context()`   | Injeta `run_id`, `mode`, `stage` em todos os logs  |
| `clear_log_context()` | Remove contexto estruturado                        |

Suporta saída em formato JSON via `PFVSH_LOG_FORMAT=json`.

### 12. Utilitários (`utils.r`)

Funções auxiliares reutilizáveis de preprocessamento e combinação.

| Função                          | Descrição                                          |
| ------------------------------- | -------------------------------------------------- |
| `associa_nwp_usina()`           | Mapeia grid NWP → usinas (Haversine)               |
| `interpola_previsao_nwp()`      | Interpola NWP de 1h para 30min                     |
| `identifica_periodo_ger()`      | Detecta horários com geração solar                 |
| `adicionar_passo_previsao()`    | Calcula D+0, D+1, ..., D+9                         |
| `gera_combinacoes_modelo()`     | Lista todas as combinações de modelos a ajustar    |
| `filtra_dado_por_combinacao()`  | Filtra dados para uma combinação específica        |
| `define_hor_prev()`             | Define horizonte de previsão                       |
| `monta_dt_prev()`               | Organiza previsão em formato de data.table         |
| `get_dataset()`                 | Carrega todos os dados via `pfvIO`                 |

---

## 4. Decisões de Design

### Por que data.table?

- Performance superior para datasets grandes (milhões de linhas de séries semi-horárias)
- Sintaxe concisa para operações por grupo (por usina, por modelo, por horizonte)
- Modificação in-place eficiente em memória com `:=`

### Por que S3 Strategy Pattern para modelos?

- Permite trocar o tipo de modelo sem alterar o pipeline principal
- Adicionar novos modelos requer apenas implementar `parse_train.novo_modelo()` e `parse_predict.novo_modelo()`
- Despacho via `UseMethod()` é nativo ao R e facilita testes com mocks
- Metadados do modelo são extraídos de forma uniforme por qualquer estratégia

### Por que processar usinas individualmente?

- Permite paralelização via `run_plants()` + `future_lapply()`
- Isola falhas — uma usina com erro não afeta outras
- Facilita debugging e logging estruturado por usina (`set_log_context()`)
- Suporta retomada granular com checkpoint por usina

### Por que checkpoints e retomada?

- Pipelines com muitas usinas × modelos × horizontes podem levar horas
- Falhas pontuais não devem exigir reprocessamento completo
- Hash da configuração (`config_hash`) garante que checkpoints incompatíveis são rejeitados
- Checkpoints são limpos após conclusão bem-sucedida

### Por que artefatos de observabilidade (proveniência, métricas, saúde)?

- Rastreabilidade completa de cada execução para auditoria e reprodutibilidade
- Métricas por usina permitem identificar gargalos e anomalias de qualidade
- Relatórios de saúde fornecem visão rápida do estado do pipeline
- Formato JSON facilita integração com ferramentas de monitoramento externas

---

## 5. Fluxo de Dados

### Entrada

```
data/
├── config.jsonc                      # Configuração do pipeline
├── usinas.csv                        # Cadastro de usinas (id, lat, lon, etc.)
├── geracao_observada.csv             # Séries históricas de geração
└── irradiancia_prevista.parquet      # Previsões NWP de irradiância
```

### Processamento Interno

```
pfvIO::conectamock_pfv()
    │
    ├── get_config()
    ├── get_usinas()
    ├── get_geracao_observada()
    └── get_irradiancia_prevista()
```

### Saída

```
artifact/                             # Modo train: artefatos + observabilidade
├── {id_usina}_modelos_ajustados.rds  # Um por usina (modelo + metadados)
├── provenance-{run_id}.json
├── metrics-{run_id}.json
└── health-{run_id}.json

out/                                  # Modo predict: previsões + observabilidade
├── previsao_geracao_fotovoltaica.parquet
├── provenance-{run_id}.json
├── metrics-{run_id}.json
└── health-{run_id}.json
```

---

## 6. Fluxo de Dados Detalhado

### 6.1 Modo Treinamento

```
                    ┌─────────────────────┐
                    │  config.jsonc       │
                    │  • mode: "train"    │
                    │  • ids_usinas       │
                    │  • data_referencia  │
                    │  • horizonte_dias   │
                    │  • modelos_NWP      │
                    └─────────┬───────────┘
                              │
                              ▼
┌──────────────┐    ┌─────────────────────┐    ┌──────────────────┐
│  usinas.csv  │──▶│                     │◀───│ geracao_obs.csv  │
└──────────────┘    │    train_main()     │    └──────────────────┘
                    │                     │
┌──────────────┐    │  Para cada usina:   │
│ irrad_prev.  │──▶│  1. Filtra dados    │
│  .parquet    │    │  2. Associa NWP     │
└──────────────┘    │  3. Interpola NWP   │
                    │  4. Identifica      │
                    │     período ger     │
                    │  5. Gera combina-   │
                    │     ções modelo     │
                    │  6. Treina modelos  │
                    │     parse_train.*() │
                    └─────────┬───────────┘
                              │
                 ┌────────────┴────────────┐
                 ▼                         ▼
     ┌─────────────────────┐   ┌──────────────────────┐
     │  {usina}_modelos_   │   │  observabilidade     │
     │    ajustados.rds    │   │  → artifact/         │
     │                     │   │  provenance-*.json   │
     │  Lista contendo:    │   │  metrics-*.json      │
     │  • models           │   │  health-*.json       │
     │  • metadata         │   └──────────────────────┘
     └─────────────────────┘
```

### 6.2 Modo Previsão

```
┌─────────────────────┐    ┌─────────────────────┐
│  config.jsonc       │    │  {usina}_modelos_   │
│  • mode: "predict"  │    │    ajustados.rds    │
└─────────┬───────────┘    └─────────┬───────────┘
          │                          │
          ▼                          ▼
        ┌─────────────────────────────┐
        │      predict_main()         │
        │                             │
        │  Para cada usina:           │
        │  1. Define dias de previsão │
        │  2. Carrega artefatos       │
        │  3. Prepara dados NWP       │
        │  4. Recalibra modelo Arimax │
        │  5. parse_predict.*()       │
        │  6. Suaviza previsão        │
        └──────────┬──────────────────┘
                   │
      ┌────────────┴────────────┐
      ▼                         ▼
┌─────────────────────┐   ┌──────────────────────┐
│  previsao_geracao_  │   │  observabilidade     │
│  fotovoltaica       │   │  → out/              │
│  .parquet           │   │  provenance-*.json   │
│                     │   │  metrics-*.json      │
│  Colunas:           │   │  health-*.json       │
│  • id_modelo_prev   │   └──────────────────────┘
│  • id_usina         │
│  • id_modelo_nwp    │
│  • data_hora_rodada │
│  • data_hora_prev   │
│  • valor (MW)       │
└─────────────────────┘
```

---

## 7. Camadas de Abstração

### 7.1 Camada de Interface (`cli.r`, `main.r`)

- **Responsabilidade**: Receber inputs do usuário (CLI/env vars/config)
- **Componentes**: `cli_main()`, `get_parser()`, `read_env_flag()`, `read_env_integer()`
- **Saída**: Parâmetros resolvidos com precedência definida; dispatch para modo correto

### 7.2 Camada de Configuração (`config-file.r`)

- **Responsabilidade**: Validar e interpretar configurações
- **Padrão**: S3 dispatch (`parsearg_data_referencia.*`)
- **Validações**: Nomes de chaves, tipos de valores, resolução de caminhos

### 7.3 Camada de Preprocessamento (`utils.r`)

- **Responsabilidade**: Preparar dados para modelagem
- **Funções-chave**:
  - `associa_nwp_usina()`: Mapeia grid NWP → usinas via Haversine
  - `interpola_previsao_nwp()`: Interpolação para 30 min
  - `identifica_periodo_ger()`: Detecta horários com geração solar
  - `adicionar_passo_previsao()`: Indica passo de previsão D+0 a D+9
  - `gera_combinacoes_modelo()`: Lista todas as combinações de modelos
  - `filtra_dado_por_combinacao()`: Filtra dados para treinar/prever

### 7.4 Camada de Modelagem (`train.r`, `predict.r`)

- **Responsabilidade**: Treinamento e previsão por combinação
- **Padrão**: S3 method dispatch por tipo de modelo (`parse_train.*`, `parse_predict.*`)
- **Modelos implementados**:
  - `arimax`: Auto ARIMA sem/com variáveis exógenas NWP
  - `fisico_estimado`: Regressão linear (RLS/RLM)

### 7.5 Camada de Observabilidade (`provenance.r`, `metrics.r`, `health-report.r`)

- **Responsabilidade**: Rastreabilidade, métricas e classificação de saúde
- **Padrão**: Acumulação por usina durante execução, serialização JSON ao final

### 7.6 Camada de I/O (`pfvIO` — externo)

- **Responsabilidade**: Abstração de acesso a dados
- **Padrão**: Conexão mock para desenvolvimento/testes via `conectamock_pfv()`

---

## 8. Padrões de Design

### 8.1 S3 Method Dispatch

O sistema usa S3 para extensibilidade de modelos:

```r
# Generic
parse_train <- function(modelo_parametros, ...) UseMethod("parse_train")

# Method para ARIMAX
parse_train.arimax <- function(modelo_parametros, ...) { ... }

# Method para Físico-Estimado
parse_train.fisico_estimado <- function(modelo_parametros, ...) { ... }
```

**Vantagem**: Adicionar novos modelos requer apenas implementar novos métodos S3, sem alterar o pipeline principal.

### 8.2 Functional Programming

Uso extensivo de `lapply`/`vapply` para operações em listas:

```r
# Treina modelo para todas as combinações
mod_aju <- lapply(list_comb, train_modelo, ...)

# Processa todas as variáveis meteorológicas
dt_prev <- lapply(dt_prev, associa_nwp_usina, dt_usinas = dt_usinas)
```

### 8.3 Copy-on-Modify com data.table

Uso explícito de `copy()` quando necessário preservar dados originais:

```r
dt <- copy(ger_usi)
dt[, hora_min := format(data_hora_observacao, "%H:%M")]
```

### 8.4 Configuration-Driven

Comportamento controlado por arquivo de configuração JSONC:

- Sem magic numbers no código
- Hiperparâmetros externalizados
- Hash de configuração garante reprodutibilidade e compatibilidade de checkpoints

---

## 9. Dependências Principais

| Pacote         | Propósito                          |
| -------------- | ---------------------------------- |
| `data.table`   | Manipulação eficiente de dados     |
| `forecast`     | Modelos ARIMA/ARIMAX               |
| `zoo`          | Interpolação de séries temporais   |
| `lubridate`    | Manipulação de datas               |
| `argparse`     | Parser de CLI                      |
| `lgr`          | Logging estruturado                |
| `future`       | Backend de paralelismo             |
| `future.apply` | `future_lapply` para paralelização |
| `digest`       | Hash SHA-256 para config           |
| `jsonlite`     | Serialização JSON (observabilidade)|
| `pfvIO`        | I/O de dados PFV (interno)         |

---

## 10. Dependências Externas

```
pfvIO (>= 0.2.2)
├── Leitura padronizada de dados
├── Validação de schemas
└── Conexão mock para testes

data.table (>= 1.17.0)
├── Manipulação de dados
└── Operações por grupo

lubridate (>= 1.9.4)
└── Manipulação de datas/horas

forecast (>= 8.24.0)
├── Modelos ARIMA/ARIMAX
└── auto.arima, Arima

zoo (>= 1.8-14)
└── Interpolação de séries temporais (na.approx)

future (>= 1.34.0)
├── Backend de paralelismo
└── Planos: multisession, multicore, sequential

future.apply (>= 1.11.0)
└── future_lapply para paralelização por usina

digest (>= 0.6.0)
└── SHA-256 hash para config e proveniência

jsonlite (>= 1.8.0)
└── Serialização JSON (proveniência, métricas, saúde)

lgr (>= 0.4.4)
├── Logging estruturado
└── Contexto por execução (FilterInject)

argparse (>= 2.2.5)
└── Parsing de argumentos CLI

arrow (>= 14.0.0) [Suggests]
└── Leitura/escrita Parquet
```

---

## 11. Considerações de Performance

### 11.1 Granularidade de Modelos

O sistema treina um modelo separado para cada combinação de:

- Usina
- Modelo NWP
- Horizonte de previsão (D+0, D+1, ..., D+9)
- Meia-hora do dia (00:00, 00:30, ..., 23:30)

**Implicação**: Para 1 usina × 1 NWP × 2 horizontes × 48 meias-horas = 96 modelos por usina.

### 11.2 Otimizações Implementadas

- `data.table` para operações em grandes datasets com `:=` in-place
- `vapply`/`lapply` em vez de loops for
- Interpolação linear via `zoo::na.approx` (vetorizada)
- **Paralelização via `future`/`future.apply`**: `run_plants()` executa por usina em paralelo quando `parallel = TRUE`, com número de workers configurável e auto-detect

### 11.3 Retomada de Execuções

Checkpoints por usina permitem retomar pipelines interrompidos sem reprocessar usinas já concluídas, reduzindo o tempo de recuperação de falhas de O(n) para O(pendentes).

---

## 12. Extensibilidade

### 12.1 Adicionar Novo Modelo de Previsão

1. Implemente `parse_train.novo_modelo()` em `train.r`
2. Implemente `parse_predict.novo_modelo()` em `predict.r`
3. Adicione configuração em `config.jsonc`

```r
# Em train.r
#' @export
parse_train.lstm <- function(modelo_parametros, ...) {
    # Implementação do treinamento LSTM
}

# Em predict.r
#' @export
parse_predict.lstm <- function(modelo, ...) {
    # Implementação da previsão LSTM
}
```

### 12.2 Adicionar Nova Variável Exógena

1. Atualize `get_dataset()` em `utils.r` para carregar a nova variável via `pfvIO`
2. Atualize `associa_nwp_usina()` ou crie nova função de associação
3. Atualize os métodos `parse_train.*` e `parse_predict.*` relevantes

---

## 13. Diagrama de Sequência — Treinamento

```
┌──────┐    ┌─────────┐    ┌───────────┐    ┌───────────┐    ┌─────────┐
│main.r│    │config-  │    │train.r    │    │utils.r    │    │forecast │
└──┬───┘    │file.r   │    └─────┬─────┘    └─────┬─────┘    └────┬────┘
   │        └────┬────┘          │                │               │
   │ cli_main    │               │                │               │
   │────────────>│               │                │               │
   │             │ parse_config  │                │               │
   │             │──────────────>│                │               │
   │             │               │                │               │
   │             │               │ train_main     │               │
   │             │               │───────────────>│               │
   │             │               │                │ get_dataset   │
   │             │               │                │──────────────>│
   │             │               │                │               │
   │             │               │                │ associa_nwp   │
   │             │               │                │<──────────────│
   │             │               │                │               │
   │             │               │ treina_usina   │               │
   │             │               │<───────────────│               │
   │             │               │                │               │
   │             │               │ parse_train.*  │               │
   │             │               │───────────────>│               │
   │             │               │                │ auto.arima    │
   │             │               │                │──────────────>│
   │             │               │                │               │
   │             │               │                │<──────────────│
   │             │               │<───────────────│  modelo       │
   │             │               │                │               │
   │             │               │ saveRDS        │               │
   │             │               │ write_prov..   │               │
   │             │               │ write_metrics  │               │
   │             │               │ write_health   │               │
   │             │               │────────────────│               │
```

---

## 14. CI/CD

| Workflow        | Descrição                                    |
| --------------- | -------------------------------------------- |
| `R-CMD-check`   | Verificação completa do pacote R             |
| `test-coverage` | Testes com relatório de cobertura (Codecov)  |
| `lint`          | lintr com complexidade ciclomática           |
| `docker`        | Build e publicação da imagem Docker          |

---

## 15. Referências

- [Advanced R (2nd ed.)](https://adv-r.hadley.nz/) — Hadley Wickham
- [R Packages (2nd ed.)](https://r-pkgs.org/) — Hadley Wickham, Jenny Bryan
- [data.table documentation](https://rdatatable.gitlab.io/data.table/)
- [forecast package](https://pkg.robjhyndman.com/forecast/)
- [future package](https://future.futureverse.org/)
