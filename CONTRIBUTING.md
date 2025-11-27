# Guia de Contribuição

Obrigado pelo interesse em contribuir com o `pfv-sh`! Este documento fornece diretrizes para colaboradores.

---

## 1. Configuração do Ambiente de Desenvolvimento

### 1.1 Pré-requisitos

- R ≥ 4.0
- RStudio (recomendado) ou outro IDE com suporte a R
- Git

### 1.2 Setup Inicial

```bash
# Clone o repositório
git clone https://github.com/isabelafiuza/pfv-sh.git
cd pfv-sh

# Restaure as dependências com renv
R -e "renv::restore()"

# Instale dependências de desenvolvimento
R -e "install.packages(c('devtools', 'testthat', 'lintr', 'styler', 'roxygen2'))"
```

### 1.3 Estrutura do Projeto

```
pfv-sh/
├── R/                      # Código-fonte do pacote
│   ├── pfv-sh.r            # Documentação do pacote e variáveis globais
│   ├── train.r             # Funções de treinamento
│   ├── predict.r           # Funções de previsão
│   ├── utils.r             # Funções utilitárias
│   ├── config-file.r       # Validação e parsing de configuração
│   └── parser.r            # Parser de argumentos CLI
├── tests/testthat/         # Testes unitários
├── data/                   # Dados de exemplo
├── man/                    # Documentação gerada (roxygen2)
├── main.r                  # Ponto de entrada CLI
├── Dockerfile              # Containerização
└── renv.lock               # Lockfile de dependências
```

---

## 2. Estilo de Código

### 2.1 Formatação

Seguimos o estilo tidyverse. Use `styler` para formatação automática:

```r
# Formatar um arquivo
styler::style_file("R/utils.r")

# Formatar todo o pacote
styler::style_pkg()
```

### 2.2 Linting

Use `lintr` para verificar problemas de estilo:

```r
# Verificar um arquivo
lintr::lint("R/train.r")

# Verificar todo o pacote
lintr::lint_package()
```

A configuração do linter está em `.lintr`.

### 2.3 Princípios de Código

Seguimos os princípios do [Advanced R](https://adv-r.hadley.nz/):

1. **Funções pequenas e focadas**: Uma função, uma responsabilidade
2. **Programação funcional**: Prefira `lapply`/`vapply` sobre loops `for`
3. **Imutabilidade**: Use `copy()` em `data.table` quando necessário
4. **Tratamento de erros**: Use `tryCatch()` com mensagens informativas
5. **Validação de entrada**: Sempre valide parâmetros de funções públicas

```r
# Exemplo de função bem estruturada
#' @title Calcula Erro Médio Absoluto
#' @param actual Vetor numérico de valores observados
#' @param predicted Vetor numérico de valores previstos
#' @return Erro médio absoluto (escalar numérico)
#' @export
calculate_mae <- function(actual, predicted) {
    stopifnot(
        is.numeric(actual),
        is.numeric(predicted),
        length(actual) == length(predicted)
    )
    
    mean(abs(actual - predicted), na.rm = TRUE)
}
```

---

## 3. Documentação

### 3.1 Roxygen2

Todas as funções exportadas devem ter documentação roxygen2:

```r
#' Título Curto da Função
#'
#' Descrição mais detalhada do que a função faz,
#' incluindo contexto e casos de uso.
#'
#' @param param1 Descrição do primeiro parâmetro
#' @param param2 Descrição do segundo parâmetro
#'
#' @return Descrição do valor retornado
#'
#' @examples
#' \dontrun{
#' resultado <- minha_funcao(arg1, arg2)
#' }
#'
#' @export
minha_funcao <- function(param1, param2) {
    # implementação
}
```

### 3.2 Gerando Documentação

```r
# Gerar arquivos de documentação
devtools::document()

# Verificar documentação
devtools::check_man()
```

---

## 4. Testes

### 4.1 Estrutura de Testes

Usamos `testthat` (edição 3). Os testes ficam em `tests/testthat/`.

```r
# tests/testthat/test-utils.r

test_that("define_hor_prev retorna datas corretas", {
    result <- define_hor_prev("2025-11-07", c("D+0", "D+1"))
    
    expect_s3_class(result, "Date")
    expect_length(result, 2)
    expect_equal(result[1], as.Date("2025-11-07"))
})
```

### 4.2 Executando Testes

```r
# Todos os testes
devtools::test()

# Arquivo específico
testthat::test_file("tests/testthat/test-utils.r")

# Com coverage
covr::package_coverage()
```

### 4.3 Diretrizes para Testes

- **Teste casos de borda**: Vetores vazios, `NA`, valores únicos
- **Teste tipos de entrada**: Garanta que a função rejeita entradas inválidas
- **Teste saídas esperadas**: Verifique estrutura, tipos e valores
- **Respeite a temporalidade**: Em testes de séries temporais, garanta ordenação correta

---

## 5. Fluxo de Trabalho Git

### 5.1 Branches

- `main`: Branch principal, sempre estável
- `develop`: Integração de features (se aplicável)
- `feature/*`: Novas funcionalidades
- `bugfix/*`: Correções de bugs
- `hotfix/*`: Correções urgentes em produção

### 5.2 Commits

Siga o padrão [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: adiciona suporte a modelo LSTM
fix: corrige interpolação de dados NWP faltantes
docs: atualiza documentação de configuração
test: adiciona testes para associa_nwp_usina
refactor: extrai lógica de normalização para função separada
```

### 5.3 Pull Requests

1. Crie uma branch a partir de `main`
2. Implemente suas alterações
3. Execute testes localmente: `devtools::test()`
4. Execute linting: `lintr::lint_package()`
5. Atualize documentação se necessário
6. Abra um PR com descrição clara das mudanças

---

## 6. Checklist de PR

Antes de submeter um PR, verifique:

- [ ] Código segue o estilo tidyverse (`styler::style_pkg()`)
- [ ] Sem warnings do linter (`lintr::lint_package()`)
- [ ] Todos os testes passam (`devtools::test()`)
- [ ] Funções exportadas têm documentação roxygen2
- [ ] `devtools::document()` foi executado
- [ ] `devtools::check()` passa sem ERRORs
- [ ] Alterações estão descritas no PR

---

## 7. Reportando Issues

### 7.1 Bugs

Inclua:
- Versão do R e sistema operacional
- Passos para reproduzir
- Comportamento esperado vs. observado
- Mensagens de erro completas
- Exemplo mínimo reproduzível

### 7.2 Features

Inclua:
- Caso de uso / problema a resolver
- Proposta de solução (se houver)
- Impacto em código existente

---

## 9. Contato

- **Maintainer**: Isabela Fiuza (isabela.fiuza@ons.org.br)
- **Issues**: [GitHub Issues](https://github.com/isabelafiuza/pfv-sh/issues)
