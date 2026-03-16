from qgis.core import (
    QgsProject,
    QgsVectorLayer,
    QgsFeature,
    QgsFields,
    QgsVectorLayer,
    QgsWkbTypes,
    QgsField,
)
from PyQt5.QtCore import QVariant

# Get selected layers from the QGIS Layers panel
selected_layers = [layer for layer in iface.layerTreeView().selectedLayers()
                   if isinstance(layer, QgsVectorLayer)]

if len(selected_layers) < 2:
    raise Exception("Select at least two vector layers to merge.")

# Sort layers by name
selected_layers.sort(key=lambda lyr: lyr.name())

# Use the geometry type and CRS of the first layer
template_layer = selected_layers[0]
geom_type = template_layer.wkbType()
crs = template_layer.crs()

# Build unified field list and add a custom 'new_fid' field
fields = QgsFields()
fields.append(QgsField("new_fid", QVariant.Int))  # our manual FID tracker

# Add existing fields from the first layer
for field in template_layer.fields():
    fields.append(field)

# Create a memory layer
merged_layer = QgsVectorLayer(
    f"{QgsWkbTypes.displayString(geom_type)}?crs={crs.authid()}",
    "Merged_Layer", "memory")
provider = merged_layer.dataProvider()
provider.addAttributes(fields)
merged_layer.updateFields()

# Merge features while assigning new_fid
fid_counter = 0
new_features = []

for layer in selected_layers:
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

# Add merged layer to the QGIS project
QgsProject.instance().addMapLayer(merged_layer)
