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
if not input_layer or not input_layer.selectedFeatureCount():    
    iface.messageBar().pushWarning("Selection Required", "Please select features in the input layer.")
    raise Exception("Please select features in the input layer.")


selected_features_only = QgsProcessingFeatureSourceDefinition(
    input_layer.source(),
    selectedFeaturesOnly=True
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
max_distance_meters = 3

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
