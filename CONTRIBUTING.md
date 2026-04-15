# Contribuindo para o pfv-sh

Obrigado pelo interesse em contribuir! Este documento fornece diretrizes para contribuições ao projeto.

## Índice

- [Como Contribuir](#como-contribuir)
- [Configuração do Ambiente](#configuração-do-ambiente)
- [Padrões de Código](#padrões-de-código)
- [Testes](#testes)
- [Documentação](#documentação)
- [Processo de Pull Request](#processo-de-pull-request)

---

## Como Contribuir

### Reportando Bugs

1. Verifique se o bug já não foi reportado nas [Issues](https://github.com/isabelafiuza/pfv-sh/issues)
2. Se não encontrar, crie uma nova issue usando o template de bug report
3. Inclua:
   - Descrição clara do problema
   - Passos para reproduzir
   - Comportamento esperado vs. observado
   - Versão do R e do pacote
   - Logs de erro (se aplicável)

### Sugerindo Melhorias

1. Abra uma issue usando o template de feature request
2. Descreva:
   - O problema que a melhoria resolve
   - A solução proposta
   - Alternativas consideradas

### Contribuindo com Código

1. Fork o repositório
2. Crie uma branch para sua feature (`git checkout -b feature/minha-feature`)
3. Faça commits atômicos com mensagens descritivas
4. Escreva/atualize testes para suas mudanças
5. Garanta que todos os testes passam
6. Abra um Pull Request

---

## Configuração do Ambiente

### Pré-requisitos

- R >= 4.0
- Visual Studio Code (recomendado) ou outra IDE
- Git

### Setup Inicial

```bash
# Clone seu fork
git clone https://github.com/SEU_USUARIO/pfv-sh.git
cd pfv-sh

# Adicione o upstream
git remote add upstream https://github.com/isabelafiuza/pfv-sh.git

# Restaure as dependências com renv
Rscript -e "renv::restore()"

# Instale dependências de desenvolvimento
Rscript -e "install.packages(c('devtools', 'testthat', 'lintr', 'roxygen2', 'covr', 'cyclocomp', 'withr'))"
```

### Estrutura do Projeto

```
pfv-sh/
├── R/                      # Código-fonte do pacote
│   ├── pfv-sh.r            # Documentação do pacote e variáveis globais
│   ├── train.r             # Funções de treinamento
│   ├── predict.r           # Funções de previsão
│   ├── combinacao.r        # Lógica de combinação de modelos
│   ├── artifact.r          # Artefatos de modelo
│   ├── utils.r             # Funções utilitárias
│   ├── config-file.r       # Validação e parsing de configuração
│   ├── parser.r            # Parser de argumentos CLI
│   ├── cli.r               # Entry point CLI
│   ├── escrita.r           # Escrita de resultados
│   ├── logging.r           # Logging estruturado
│   ├── metrics.r           # Métricas de pipeline
│   ├── health-report.r     # Relatórios de saúde
│   ├── parallel.r          # Infraestrutura paralela
│   └── provenance.r        # Proveniência
├── tests/testthat/         # Testes unitários
├── data/                   # Dados de exemplo
├── man/                    # Documentação gerada (roxygen2)
├── main.r                  # Ponto de entrada CLI
├── Dockerfile              # Containerização
└── renv.lock               # Lockfile de dependências
```

### Verificando a Instalação

```r
# No R
devtools::load_all()   # Carrega o pacote em desenvolvimento
devtools::test()       # Executa os testes
devtools::check()      # Verificação completa
```

---

## Padrões de Código

### Estilo

Seguimos o [tidyverse style guide](https://style.tidyverse.org/) com customizações definidas em `.lintr`:

```r
# Configurações principais (.lintr):
# - Linha máxima: 120 caracteres
# - Indentação: 4 espaços (block style, sem hanging indent)
# - Nomes: snake_case ou symbols
# - Complexidade ciclomática máxima: 12 por função
# - object_usage_linter desabilitado
# - return_linter desabilitado
```

### Verificação de Estilo

```r
# Executar linter completo (inclui complexidade ciclomática)
lintr::lint_package()

# Para um arquivo específico
lintr::lint("R/meu_arquivo.r")
```

### Complexidade Ciclomática

Funções com complexidade ciclomática acima de 12 falham no lint. Para reduzir a complexidade:

- Extraia condições compostas em funções predicado (ex: `has_high_na_rate()`)
- Mova blocos de lógica independente para helpers privadas
- Combine múltiplas verificações `if` idênticas em um único bloco

### Boas Práticas R

Seguimos os princípios do [Advanced R](https://adv-r.hadley.nz/):

#### 1. Funções Puras e Pequenas

```r
# Bom: função focada, sem efeitos colaterais
calcular_mae <- function(observado, previsto) {
    stopifnot(
        is.numeric(observado),
        is.numeric(previsto),
        length(observado) == length(previsto)
    )
    mean(abs(observado - previsto), na.rm = TRUE)
}

# Evitar: funções grandes com múltiplas responsabilidades
```

#### 2. Validação de Entrada

Use `validate_artifact()` para validar artefatos de modelo. Para dados de entrada, use validação explícita:

```r
processar_dados <- function(dt, coluna) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' deve ser um data.table")
    }
    if (!coluna %in% names(dt)) {
        stop(sprintf("Coluna '%s' não encontrada", coluna))
    }
    # ...
}
```

#### 3. Strategy Pattern

Novos modelos de previsão devem seguir a convenção de despacho S3 do pacote. O despacho é feito via `parse_train()` e `parse_predict()`, que resolvem o método pela classe do objeto de configuração do modelo.

```r
# 1. Implementar método de treinamento
parse_train.meu_modelo <- function(modelo_parametros, ...) {
    args <- list(...)
    pars <- args$pars
    ger_usi <- args$ger_usi
    # ... ajustar modelo e retornar estrutura compatível ...
}

# 2. Implementar método de previsão
parse_predict.meu_modelo <- function(modelo, ...) {
    args <- list(...)
    pars <- args$pars
    # ... executar previsão e retornar vetor numérico (MW) ...
}
```

Consulte `parse_train.arimax()` e `parse_train.fisico_estimado()` em `R/train.r`, e seus equivalentes em `R/predict.r`, como referência de implementação.

#### 4. Operações Vetorizadas

```r
# Bom: vetorizado
valores_normalizados <- (valores - min(valores)) / (max(valores) - min(valores))

# Evitar: loops explícitos quando desnecessários
for (i in seq_along(valores)) {
    valores_normalizados[i] <- (valores[i] - min(valores)) / (max(valores) - min(valores))
}
```

#### 5. data.table Idiomático

```r
# Bom: sintaxe data.table
dt[, valor_ajustado := valor * fator, by = id_usina]
dt[is.na(valor), valor := 0]

# Evitar: misturar com dplyr ou base R desnecessariamente
```

#### 6. Tratamento de Erros

```r
# Bom: mensagens informativas com coleta de erros
errors <- character(0L)
errors <- check_required_columns(errors, dt, schema)
errors <- check_column_types(errors, dt, schema)
if (length(errors) > 0L) {
    stop(paste0("- ", errors, collapse = "\n"), call. = FALSE)
}
```

---

## Testes

### Estrutura de Testes

```
tests/
├── testthat.r                          # Runner principal
└── testthat/
    ├── data/                           # Dados de teste (CSV/JSONC)
    ├── setup.r                         # Setup global (skip helpers)
    ├── helper-generators.r             # Geradores de dados de teste
    ├── helper-logging.r                # Helpers de logging para testes
    ├── test-artifact.r                 # Artefatos de modelo
    ├── test-cli.r                      # Entry point CLI
    ├── test-combinacao.r               # Combinação de modelos
    ├── test-config-file.r              # Configuração
    ├── test-escrita.r                  # Escrita de resultados
    ├── test-health-report.r            # Relatórios de saúde
    ├── test-integration-predict.r      # Integração do predict
    ├── test-integration-train.r        # Integração do train
    ├── test-logging.r                  # Logging estruturado
    ├── test-metrics.r                  # Métricas de pipeline
    ├── test-parallel.r                 # Infraestrutura paralela
    ├── test-pipeline-resume.r          # Retomada de pipeline
    ├── test-predict.r                  # Pipeline predict
    ├── test-provenance.r               # Proveniência
    ├── test-train.r                    # Pipeline train
    └── test-utils.r                    # Utilitários
```

### Geradores de Dados de Teste

O arquivo `helper-generators.r` fornece funções `gen_*()` para criar dados de teste consistentes:

```r
# Gera configuração padrão
config <- gen_config(mode = "train")

# Gera configuração com janela de treinamento específica
config <- gen_config(ids_usinas = c("USI1", "USI2"))

# Gera artefato de modelo
artifact <- gen_model_artifact(id_usina = "USI1")

# Gera dados de geração observada
dt_ger <- gen_geracao_observada(ids = "USI1", pattern = "normal")

# Gera dados de irradiância prevista
dt_irrad <- gen_irradiancia_prevista(ids = "USI1", modelo_nwp = "GFS")

# Gera tabela de usinas
dt_usinas <- gen_usinas(n = 2L)
```

### Escrevendo Testes

```r
test_that("funcao_exemplo retorna resultado esperado", {
    # Arrange: preparar dados
    entrada <- data.table(valor = c(1, 2, 3, NA, 5))

    # Act: executar função
    resultado <- funcao_exemplo(entrada)

    # Assert: verificar resultado
    expect_equal(nrow(resultado), 5)
    expect_true(all(!is.na(resultado$valor)))
})
```

Testes que dependem de dados de teste (diretório `data/`) ou de ferramentas opcionais devem usar `skip_if_not()`:

```r
test_that("integração com dados reais", {
    skip_if_not(dir.exists(testthat::test_path("data")))
    skip_if_no_zstd()
    # ...
})
```

### Executando Testes

```r
# Todos os testes
devtools::test()

# Teste específico
devtools::test(filter = "config-file")

# Com cobertura
covr::package_coverage()
```

### Requisitos de Cobertura

- Novas funções públicas devem ter testes
- Casos de borda (NA, vazios, tipos errados) devem ser testados
- Testes devem ser independentes e reprodutíveis
- CI exige passagem do R-CMD-check, lint e cobertura de testes

---

## Documentação

### Roxygen2

Todas as funções exportadas devem ter documentação completa:

```r
#' Título Curto da Função
#'
#' Descrição mais detalhada do que a função faz,
#' quando usar, e qualquer contexto relevante.
#'
#' @param x Descrição do parâmetro x. Tipo esperado.
#' @param y Descrição do parâmetro y. Valor default.
#'
#' @return Descrição do retorno, incluindo tipo e estrutura.
#'
#' @details
#' Detalhes adicionais sobre o algoritmo, complexidade,
#' ou considerações importantes.
#' Se for exportada pelo pacote, deve ter exemplos.
#'
#' @examples
#' \dontrun{
#' resultado <- minha_funcao(dados, opcao = TRUE)
#' }
#'
#' @seealso [funcao_relacionada()]
#'
#' @export
minha_funcao <- function(x, y = TRUE) {
    # implementação
}
```

### Gerando Documentação

```r
# Atualizar documentação
devtools::document()

# Verificar se há warnings
devtools::check_man()
```

---

## Processo de Pull Request

### Antes de Abrir o PR

1. **Sincronize com upstream**

   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

2. **Execute verificações locais**

   ```r
   devtools::document()  # Atualiza documentação
   devtools::test()      # Testes passam
   lintr::lint_package() # Sem erros de lint
   devtools::check()     # Check completo passa
   ```

3. **Commits organizados**
   - Mensagens descritivas em português ou inglês
   - Um commit por mudança lógica
   - Formato: `tipo: descrição curta`
     - `feat:` nova funcionalidade
     - `fix:` correção de bug
     - `docs:` documentação
     - `test:` testes
     - `refactor:` refatoração

### Template do PR

```markdown
## Descrição

Breve descrição das mudanças.

## Tipo de Mudança

- [ ] Bug fix
- [ ] Nova feature
- [ ] Breaking change
- [ ] Documentação

## Checklist

- [ ] Testes adicionados/atualizados
- [ ] Documentação atualizada
- [ ] `devtools::check()` passa sem erros
- [ ] `lintr::lint_package()` sem avisos
- [ ] Código segue os padrões do projeto

## Issues Relacionadas

Closes #123
```

### Revisão

- PRs requerem pelo menos 1 aprovação
- CI deve passar (testes, lint, R-CMD-check, cobertura)
- Discussões devem ser resolvidas antes do merge

---

## Versionamento

Seguimos [Semantic Versioning](https://semver.org/):

- **MAJOR**: mudanças incompatíveis na API
- **MINOR**: novas funcionalidades compatíveis
- **PATCH**: correções de bugs compatíveis

Atualize o `DESCRIPTION` e `CHANGELOG.md` ao lançar versões.

---

## Dúvidas?

- Abra uma [Discussion](https://github.com/isabelafiuza/pfv-sh/discussions) para perguntas gerais
- Use [Issues](https://github.com/isabelafiuza/pfv-sh/issues) para bugs e features
- Consulte a documentação existente

Obrigado por contribuir!
