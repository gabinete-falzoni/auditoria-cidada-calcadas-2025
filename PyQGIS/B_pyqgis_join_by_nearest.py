# Junta camada de pontos GPS revistos aos atributos de lote Geosampa (sql)

import processing
from qgis.core import (
    QgsProject,
    QgsVectorLayer,
    QgsProcessingFeatureSourceDefinition
)

# --------------------------------------------
# Get the selected layer (INPUT LAYER)
# --------------------------------------------
input_layer = iface.activeLayer()
if not input_layer:    
    iface.messageBar().pushWarning("Layer Required", "Please select an input layer.")
    raise Exception("Please select an input layer.")

# Get the selected features (if any)
selected_features_count = input_layer.selectedFeatureCount()

if selected_features_count > 0:
    # If there are selected features
    iface.messageBar().pushInfo(
        "Features Selected", 
        f"{selected_features_count} features are selected and will be used."
    )
    selected_features_only = QgsProcessingFeatureSourceDefinition(
        input_layer.source(),
        selectedFeaturesOnly=True
    )
else:
    # If no features are selected, use all features
    iface.messageBar().pushInfo(
        "No Features Selected", 
        "No features are selected. All features will be used."
    )
    selected_features_only = QgsProcessingFeatureSourceDefinition(
        input_layer.source(),
        selectedFeaturesOnly=False
    )

# Get CRS of input_layer
crs = input_layer.crs()

# --------------------------------------------
# Get the second layer by name
# --------------------------------------------
layer2 = QgsProject.instance().mapLayersByName('lotes_perimetro_auditoria_com_numero')[0]

# --------------------------------------------
# Max distance in meters
# --------------------------------------------
max_distance_meters = 5

# 💡 Convert meters to degrees (approximate at equator):
# 1 degree ≈ 111,319.9 meters → 7 meters ≈ 0.00006286 degrees
# This is *only an approximation*, valid for small distances
max_distance_degrees = max_distance_meters / 111319.9  # ≈ 0.00006286

# Check if CRS is geographic (degrees) or projected (meters)
if crs.isGeographic():
    # CRS like WGS 84 (EPSG:4326), use degrees
    max_distance = max_distance_degrees
    print("Using degrees for MAX_DISTANCE")
else:
    # Projected CRS like SIRGAS 2000 / UTM zone 23S (EPSG:31983), use meters
    max_distance = max_distance_meters
    print("Using meters for MAX_DISTANCE")

# --------------------------------------------
# FIELDS_TO_COPY
# --------------------------------------------
# ATENÇÃO: NÃO FUNCIONA AINDA, MISTURA AS COLUNAS E OS VALORES
# FIELDS_TO_COPY = [
#     'n_contrib', 'n_cond', 'codlog', 'logradouro', 'numero',
#     'lo_setor', 'lo_quadra', 'lo_lote', 'numeros_todos',
#     'cep', 'andares', 'testada_val', 'tipo_uso',
#     #'testada_m', 'esquinas'
# ]

# --------------------------------------------
# Run Join Attributes by Nearest
# --------------------------------------------
result = processing.run("native:joinbynearest", {
    'INPUT': selected_features_only,
    'INPUT_2': layer2,
    'FIELDS_TO_COPY': ['sql'],
    'DISCARD_NONMATCHING': False,
    'PREFIX': '',
    'NEIGHBORS': 1,
    'MAX_DISTANCE': max_distance,
    'OUTPUT': 'memory:'  # You can change to a filepath if desired
})

# --------------------------------------------
# Load the result
# --------------------------------------------
joined_layer = result['OUTPUT']
joined_layer.setName('Joined_By_Nearest')
QgsProject.instance().addMapLayer(joined_layer)

# --------------------------------------------
# Remove specified fields
# --------------------------------------------
fields_to_remove = ['fid', 'new_fid', 'n', 'distance', 'feature_x', 'feature_y', 'nearest_x', 'nearest_y']

# Get field indices for the fields to be removed
field_indices = [joined_layer.fields().indexFromName(field) for field in fields_to_remove]

# Remove the fields
if any(index != -1 for index in field_indices):
    joined_layer.dataProvider().deleteAttributes([index for index in field_indices if index != -1])
    joined_layer.updateFields()
    print(f"Fields {fields_to_remove} have been removed from the resulting layer.")
else:
    print(f"Fields {fields_to_remove} not found in the resulting layer.")


# --------------------------------------------
# Print message when done
# --------------------------------------------
print("Script has finished running.")
