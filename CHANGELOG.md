# Changelog

Todas as mudanças notáveis deste projeto serão documentadas neste arquivo.

O formato é baseado em [Keep a Changelog](https://keepachangelog.com/pt-BR/1.0.0/),
e este projeto adere ao [Semantic Versioning](https://semver.org/lang/pt-BR/).

---

## [0.2.0] - 2026-04-15

### Added

- Resolução de caminhos do config.json relativos ao diretório de dados (datadir)
- Criação automática dos diretórios artifact e output se não existirem
- Sistema de proveniência com run IDs e rastreabilidade de execução
- Checkpoints para retomada de pipeline após falhas
- Métricas de desempenho por usina (duração, qualidade do modelo)
- Relatórios de saúde do pipeline (healthy/degraded/failed)
- Execução paralela via `future`/`future.apply` com controle de workers
- Artefatos de modelo enriquecidos com metadados (tipo, versão, config hash)
- Logging estruturado com contexto (run_id, mode) via `lgr`
- Variáveis de ambiente: PFVSH_PARALLEL, PFVSH_RESUME, PFVSH_WORKERS

### Changed

- Modo train agora escreve artefatos de observabilidade no diretório artifact (compatível com mh-pfv)
- Modo predict mantém escrita no diretório output
- Resolução de prioridade de parâmetros: argumento CLI > variável de ambiente > valor padrão

## [0.1.0] - 2025-11-28

### Added

- Estrutura inicial de pacote com pipelines de `train` e `predict`
- Modelos ARIMA e ARIMAX
- Modelo Fisico-Estimado (regressão linear)
- Pré-processamento de dados de modelos NWP: associação de quadrícula, preenchimento, etc.
- Identificação automática de períodos com geração solar
- Suporte a configuração via arquivo JSONC
- Logging estruturado com `lgr`
- Exportação em formato Parquet
- Dockerfile para containerização
- Testes unitários com `testthat`
- Linting com `lintr`
- Documentação completa do projeto (README, CONTRIBUTING, ARCHITECTURE)
- GitHub Actions para CI/CD (R-CMD-check, lint, testes)
- Templates de issues e pull requests

---

## Tipos de Mudanças

- **Added**: para novas funcionalidades
- **Changed**: para mudanças em funcionalidades existentes
- **Deprecated**: para funcionalidades que serão removidas em breve
- **Removed**: para funcionalidades removidas
- **Fixed**: para correções de bugs
- **Security**: para correções de vulnerabilidades
