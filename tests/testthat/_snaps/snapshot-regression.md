# snapshot_provenance_train_structure

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["field_names", "mode", "status", "n_plants", "plant_ids", "plant_status_keys", "plant_status_values", "parallel"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["config_hash", "mode", "n_plants", "parallel", "plant_ids", "plant_status", "status"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["train"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["completed"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BAUFI1"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BAUFI1"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["completed"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        }
      ]
    }

# snapshot_predict_output_structure

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id_usina", "col_names", "col_types", "n_rows", "n_rows_per_modelo_prev", "n_rows_per_modelo_nwp"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["BAUFI1"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["data_hora_previsao", "data_hora_rodada", "id_modelo_nwp", "id_modelo_prev", "id_usina", "valor"]
        },
        {
          "type": "character",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["data_hora_previsao", "data_hora_rodada", "id_modelo_nwp", "id_modelo_prev", "id_usina", "valor"]
            }
          },
          "value": ["POSIXct", "POSIXct", "character", "character", "character", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [98]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["arimax", "combinado"]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [49]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [49]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["GFS"]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [98]
            }
          ]
        }
      ]
    }

