from qgis.core import QgsExpression, QgsExpressionContext, QgsExpressionContextUtils

# Example: x = 3, layer name = "YourLayerName", field = "start_time"
# layer = iface.activeLayer()  # This will get the active layer
# If you want to access the layer by name:
layer = QgsProject.instance().mapLayersByName('2025-09-16-10-35-43-798')[0]
x = 3  # Integer to add to each value

if layer:
    field_name = 'start_time'  # Name of the column    
    
    # Start editing the layer to allow modifications
    layer.startEditing()

    # Iterate through the features and update the 'start_time' field
    for feature in layer.getFeatures():
        current_value = feature[field_name]
        
        # Make sure the current value is numeric
        if current_value is not None:
            new_value = current_value + x
            
            # Update the field with the new value
            layer.changeAttributeValue(feature.id(), layer.fields().indexOf(field_name), new_value)

    # Commit the changes after updating all features
    layer.commitChanges()

    print("Values updated successfully.")
else:
    print("Layer not found.")
