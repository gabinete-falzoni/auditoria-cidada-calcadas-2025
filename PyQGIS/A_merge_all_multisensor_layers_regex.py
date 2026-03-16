# Junta todas as camadas de GPS revistos do MultiSensor em uma. Para
# isso, considera o regex do nome do arquivo - se houver outras camadas
# com padrao de nome identico, sera unida tambems

import re
from qgis.core import (
    QgsProject,
    QgsVectorLayer,
    QgsFeature,
    QgsFields,
    QgsField,
    QgsWkbTypes,
    #QgsCoordinateReferenceSystem,
)

from PyQt5.QtCore import QVariant

# Regex pattern to match layer names (e.g., 2025-11-11-10-18-34-672)
layer_name_pattern = r"\d{4}-\d{2}-\d{2}-\d{2}-\d{2}-\d{2}-\d{3}"

# Get layers from the QGIS Layers panel that match the regex pattern
matching_layers = [
    layer for layer in QgsProject.instance().mapLayers().values()
    if isinstance(layer, QgsVectorLayer) and re.match(layer_name_pattern, layer.name())
]

if len(matching_layers) < 2:
    raise Exception("There must be at least two vector layers matching the pattern.")

# Sort layers by name
matching_layers.sort(key=lambda lyr: lyr.name())

# Use the geometry type and CRS of the first layer
template_layer = matching_layers[0]
geom_type = template_layer.wkbType()
crs = template_layer.crs()

# Build unified field list and add a custom 'new_fid' field
fields = QgsFields()
fields.append(QgsField("new_fid", QVariant.Int))  # our manual FID tracker

# Add existing fields from the first layer
for field in template_layer.fields():
    fields.append(field)

# Create a memory layer for the merged features
merged_layer = QgsVectorLayer(
    f"{QgsWkbTypes.displayString(geom_type)}?crs={crs.authid()}",
    "Merged_Layer", "memory")
provider = merged_layer.dataProvider()
provider.addAttributes(fields)
merged_layer.updateFields()

# Merge features while assigning new_fid
fid_counter = 0
new_features = []

for layer in matching_layers:
    for feat in layer.getFeatures():
        new_feat = QgsFeature()
        new_feat.setGeometry(feat.geometry())

        # Build attributes: [new_fid] + original attributes (matched by name)
        attrs = [fid_counter]
        for field in template_layer.fields():
            val = feat[field.name()] if field.name() in feat.fields().names() else None
            attrs.append(val)

        new_feat.setAttributes(attrs)
        new_features.append(new_feat)
        fid_counter += 1

# Add features to the memory layer
provider.addFeatures(new_features)
merged_layer.updateExtents()

# Reproject the merged layer to EPSG:31983 (SIRGAS 2000 23S)
#epsg_31983 = QgsCoordinateReferenceSystem("EPSG:31983")
#transform_context = QgsProject.instance().transformContext()
#merged_layer.setCrs(epsg_31983)

# Add merged layer to the QGIS project
QgsProject.instance().addMapLayer(merged_layer)
print("Script has finished running.")