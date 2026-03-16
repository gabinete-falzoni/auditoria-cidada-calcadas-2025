# Marca todos os pontos selecionados como "descartar" = TRUE

from PyQt5.QtCore import QVariant
from qgis.core import QgsField, QgsFeature

# Get the active layer
layer = iface.activeLayer()

# Check if the layer is valid and a vector layer
if layer is not None and layer.type() == QgsMapLayer.VectorLayer:
    # Start editing the layer to allow modifications
    layer.startEditing()
    
    # Define the new field name and type
    field_name = "descartar"
    
    # Add the new boolean field if it doesn't already exist
    if field_name not in [field.name() for field in layer.fields()]:
        layer.dataProvider().addAttributes([QgsField(field_name, QVariant.Bool)])
        layer.updateFields()  # Update the layer with the new field
    
    # Get the selected features
    selected_features = layer.selectedFeatures()

    # Iterate over the selected features    
    for feature in selected_features:
        # Set the value of the "descartar" field to True
        feature.setAttribute(field_name, True)
        layer.updateFeature(feature)  # Update the feature in the layer
    
    # Commit changes
    layer.commitChanges()
else:
    print("Layer is either not valid or not a vector layer.")
