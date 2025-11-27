# Arquitetura do Sistema

Este documento descreve a arquitetura técnica do `pfv-sh`, um sistema de previsão de geração solar fotovoltaica semi-horária.

---

## 1. Visão Geral

O `pfv-sh` é um pacote R estruturado seguindo os padrões de desenvolvimento de pacotes R e princípios de programação funcional. O sistema opera em dois modos principais:

1. **Treinamento (train)**: Ajusta modelos de previsão usando dados históricos
2. **Previsão (predict)**: Gera previsões usando modelos pré-treinados

---

## 2. Componentes do Sistema

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           main.r (Entry Point)                          │
│  • Parser de argumentos CLI                                             │
│  • Carregamento de configuração                                         │
│  • Despacho para train_main() ou predict_main()                         │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
          ┌──────────────────────┴──────────────────────┐
          │                                             │
          ▼                                             ▼
┌─────────────────────────────┐         ┌─────────────────────────────────┐
│     train.r                 │         │     predict.r                   │
│                             │         │                                 │
│  train_main()               │         │  predict_main()                 │
│    └─ treina_usina()        │         │    └─ predict_usina()           │
│       └─ train_modelo()     │         │       └─ parse_predict.*()      │
│          └─ parse_train.*() │         │                                 │
│                             │         │                                 │
│  S3 Methods:                │         │  S3 Methods:                    │
│  • parse_train.arimax       │         │  • parse_predict.arimax         │
│  • parse_train.fisico_est.. │         │  • parse_predict.fisico_est..   │
└─────────────────────────────┘         └─────────────────────────────────┘
          │                                             │
          └──────────────────────┬──────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                           utils.r (Shared)                              │
│                                                                         │
│  Preprocessamento:                    Combinações:                      │
│  • associa_nwp_usina()                • gera_combinacoes_modelo()       │
│  • interpola_previsao_nwp()           • filtra_dado_por_combinacao()    │
│  • preenche_ausencia_previsao()                                         │
│  • adicionar_passo_previsao()         Utilitários:                      │
│                                       • define_hor_prev()               │
│  Período de Geração:                  • monta_dt_prev()                 │
│  • identifica_periodo_ger()           • interpola_serie_temporal()      │
└─────────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      config-file.r + parser.r                           │
│                                                                         │
│  Validação:                           Parsing:                          │
│  • valida_nomes_config()              • parsearg_data_referencia()      │
│  • valida_tipos_config()              • parsearg_ids_usinas()           │
│                                       • parsearg_horizonte_dias()       │
│  CLI:                                 • parsearg_modelos_nwp()          │
│  • get_parser()                       • parsearg_modelos_previsao()     │
└─────────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                          pfvIO (External)                               │
│                                                                         │
│  • conectamock_pfv()     - Conexão com fonte de dados                   │
│  • get_config()          - Carrega arquivo de configuração              │
│  • get_usinas()          - Obtém cadastro de usinas                     │
│  • get_geracao_observada() - Obtém dados de geração                     │
│  • get_irradiancia_prevista() - Obtém previsões NWP                     │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Fluxo de Dados

### 3.1 Modo Treinamento

```
                    ┌─────────────────────┐
                    │  config.jsonc       │
                    │  • mode: "train"    │
                    │  • ids_usinas       │
                    │  • data_referencia  │
                    └─────────┬───────────┘
                              │
                              ▼
┌──────────────┐    ┌─────────────────────┐    ┌──────────────────┐
│  usinas.csv  │──▶│                     │◀───│ geracao_obs.csv  │
└──────────────┘    │    train_main()     │    └──────────────────┘
                    │                     │
┌──────────────┐    │  Para cada usina:   │
│ irrad_prev.  │──▶│  1. Filtra dados    │
│    csv       │    │  2. Associa NWP     │
└──────────────┘    │  3. Interpola       │
                    │  4. Identifica      │
                    │     período ger     │
                    │  5. Gera combina-   │
                    │     ções modelo     │
                    │  6. Treina modelos  │
                    └─────────┬───────────┘
                              │
                              ▼
                    ┌─────────────────────┐
                    │  {usina}_modelos_   │
                    │    ajustados.rds    │
                    │                     │
                    │  Lista contendo:    │
                    │  • combinacao_ajuste│
                    │  • modelo (lm/Arima)│
                    └─────────────────────┘
```

### 3.2 Modo Previsão

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
        │  1. Carrega artefatos       │
        │  2. Prepara dados NWP       │
        │  3. Recalibra modelos       │
        │  4. Gera previsões          │
        └─────────────┬───────────────┘
                      │
                      ▼
        ┌─────────────────────────────┐
        │  data.table de previsões    │
        │                             │
        │  Colunas:                   │
        │  • id_modelo_prev           │
        │  • id_usina                 │
        │  • id_modelo_nwp            │
        │  • data_hora_rodada         │
        │  • data_hora_previsao       │
        │  • valor (MW)               │
        └─────────────────────────────┘
```

---

## 4. Camadas de Abstração

### 4.1 Camada de Interface (parser.r, main.r)

- **Responsabilidade**: Receber inputs do usuário (CLI/config)
- **Componentes**: `get_parser()`, `ArgumentParser`
- **Saída**: Objeto `args` com configurações validadas

### 4.2 Camada de Configuração (config-file.r)

- **Responsabilidade**: Validar e interpretar configurações
- **Padrão**: Dispatch via S3 methods (`parsearg_data_referencia.*`)
- **Validações**: Nomes de chaves, tipos de valores

### 4.3 Camada de Preprocessamento (utils.r)

- **Responsabilidade**: Preparar dados para modelagem
- **Funções-chave**:
  - `associa_nwp_usina()`: Mapeia grid NWP → usinas
  - `interpola_previsao_nwp()`: Interpolação para 30 min
  - `identifica_periodo_ger()`: Detecta horários com geração solar

### 4.4 Camada de Modelagem (train.r, predict.r)

- **Responsabilidade**: Treinamento e previsão
- **Padrão**: S3 method dispatch por tipo de modelo
- **Modelos implementados**:
  - `arimax`: Auto ARIMA com variáveis exógenas
  - `fisico_estimado`: Regressão linear (RLS/RLM)

### 4.5 Camada de I/O (pfvIO - externo)

- **Responsabilidade**: Abstração de acesso a dados
- **Padrão**: Conexão mock para desenvolvimento/testes

---

## 5. Padrões de Design

### 5.1 S3 Method Dispatch

O sistema usa S3 para extensibilidade de modelos:

```r
# Generic
parse_train <- function(modelo_parametros, ...) UseMethod("parse_train")

# Method para ARIMAX
parse_train.arimax <- function(modelo_parametros, ...) { ... }

# Method para Físico-Estimado
parse_train.fisico_estimado <- function(modelo_parametros, ...) { ... }
```

**Vantagem**: Adicionar novos modelos requer apenas implementar novos métodos S3.

### 5.2 Functional Programming

Uso extensivo de `lapply` para operações em listas:

```r
# Treina modelo para todas as combinações
mod_aju <- lapply(list_comb, train_modelo, ...)

# Processa todas as variáveis meteorológicas
dt_prev <- lapply(dt_prev, associa_nwp_usina, dt_usinas = dt_usinas)
```

### 5.3 Copy-on-Modify com data.table

Uso explícito de `copy()` quando necessário preservar dados originais:

```r
dt <- copy(ger_usi)  # Evita modificar ger_usi original
dt[, hora_min := format(data_hora_observacao, "%H:%M")]
```

### 5.4 Configuration-Driven

Comportamento controlado por arquivo de configuração JSON:

- Sem magic numbers no código
- Hiperparâmetros externalizados
- Fácil reprodução de experimentos

---

## 6. Dependências Principais

| Pacote | Propósito |
|--------|-----------|
| `data.table` | Manipulação eficiente de dados |
| `forecast` | Modelos ARIMA/ARIMAX |
| `zoo` | Interpolação de séries temporais |
| `lubridate` | Manipulação de datas |
| `argparse` | Parser de CLI |
| `lgr` | Logging estruturado |
| `pfvIO` | I/O de dados PFV (interno) |

---

## 7. Considerações de Performance

### 7.1 Granularidade de Modelos

O sistema treina um modelo separado para cada combinação de:
- Usina
- Modelo NWP
- Horizonte de previsão (D+0, D+1, ...)
- Meia-hora do dia (00:00, 00:30, ..., 23:30)

**Implicação**: Para 1 usina × 1 NWP × 2 horizontes × 48 meias-horas = 96 modelos

### 7.2 Otimizações Implementadas

- `data.table` para operações em grandes datasets
- `vapply`/`lapply` em vez de loops for
- Interpolação linear via `zoo::na.approx` (vetorizada)

### 7.3 Áreas para Melhoria

- Paralelização com `future`/`furrr` para treino
- Cache de modelos recalibrados
- Batch processing para múltiplas usinas

---

## 8. Extensibilidade

### 8.1 Adicionar Novo Modelo de Previsão

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

### 8.2 Adicionar Nova Variável Exógena

1. Atualize `get_dataset()` para carregar a nova variável
2. Modifique `filtra_dado_por_combinacao()` para incluir a variável
3. Atualize os modelos de treinamento/previsão

---

## 9. Diagrama de Sequência - Treinamento

```
┌──────┐    ┌─────────┐    ┌───────────┐    ┌───────────┐    ┌─────────┐
│main.r│    │config-  │    │train.r    │    │utils.r    │    │forecast │
└──┬───┘    │file.r   │    └─────┬─────┘    └─────┬─────┘    └────┬────┘
   │        └────┬────┘          │                │               │
   │             │               │                │               │
   │ parse_args  │               │                │               │
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
   │             │               │ parse_train    │               │
   │             │               │───────────────>│               │
   │             │               │                │ auto.arima    │
   │             │               │                │──────────────>│
   │             │               │                │               │
   │             │               │                │<──────────────│
   │             │               │<───────────────│  modelo       │
   │             │               │                │               │
   │             │               │ saveRDS        │               │
   │             │               │<───────────────│               │
   │             │               │                │               │
```

---

## 10. Referências

- [Advanced R (2nd ed.)](https://adv-r.hadley.nz/) - Hadley Wickham
- [R Packages (2nd ed.)](https://r-pkgs.org/) - Hadley Wickham, Jenny Bryan
- [data.table documentation](https://rdatatable.gitlab.io/data.table/)
- [forecast package](https://pkg.robjhyndman.com/forecast/)
