# Move o ponto 1 para as coordenadas copiadas na Area de Transferencia. Para
# copiar as coordenadas, clicar com o direito do mouse no mapa e copiar as
# coodendadas de baixo

from qgis.core import *
from PyQt5.QtWidgets import QApplication, QShortcut
from PyQt5.QtGui import QKeySequence
from PyQt5.QtCore import Qt
import re

# Function to get the coordinate from the clipboard
def get_clipboard_coordinate():
    clipboard = QApplication.clipboard()
    clipboard_text = clipboard.text()

    # Assuming the clipboard contains a coordinate in a format like 'x, y' or 'x y'
    match = re.match(r"(-?\d+\.\d+)\s*,\s*(-?\d+\.\d+)", clipboard_text)
    if match:
        # The match will give us the coordinates in the order 'longitude, latitude' (x, y).
        # We need to swap them, as the script currently assumes 'latitude, longitude' (y, x).
        longitude = float(match.group(2))  # x
        latitude = float(match.group(1))   # y
        return QgsPointXY(longitude, latitude)
    else:
        raise ValueError("Clipboard does not contain a valid coordinate.")

# Step 1: Define the function that will be triggered by the shortcut
def move_point_one():
    # Get the active layer
    layer = iface.activeLayer()

    # Check if the layer is a point layer
    if not layer or layer.geometryType() != QgsWkbTypes.PointGeometry:
        print("Active layer is not a point layer.")
    else:
        try:
            # Get the coordinate from clipboard
            new_coord = get_clipboard_coordinate()

            # Get the CRS of the active layer
            layer_crs = layer.crs()

            # Check if the layer is in EPSG:4326, and if not, transform the coordinate
            if layer_crs.authid() != "EPSG:4326":
                # Create a transformation from EPSG:4326 to the layer's CRS
                transform = QgsCoordinateTransform(QgsCoordinateReferenceSystem("EPSG:4326"), layer_crs, QgsProject.instance())
                new_coord = transform.transform(new_coord)

            # Start an editing session
            layer.startEditing()

            # Get the first feature in the layer (first fid)
            first_feature = next(layer.getFeatures())

            # Create a new point geometry with the clipboard coordinates (after transformation)
            new_point = QgsGeometry.fromPointXY(new_coord)

            # Update the geometry using changeGeometry(fid, new_geom)
            layer.changeGeometry(first_feature.id(), new_point)

            # Commit the changes
            layer.commitChanges()
            
            # Select the moved feature
            layer.select(first_feature.id())

            # Refresh the map canvas to reflect the changes
            iface.mapCanvas().refresh()

            # Optionally, zoom to the feature's geometry
            # iface.mapCanvas().zoomToFeatureExtent(first_feature.geometry())

            print(f"Moved the first feature to new coordinates: {new_coord}")

        except Exception as e:
            print(f"An error occurred: {e}")

# Create a QShortcut to bind the keyboard shortcut (Shift+1)
shortcut = QShortcut(QKeySequence("Shift+1"), iface.mainWindow())  # Correct way to specify Shift+1
shortcut.activated.connect(move_point_one)  # Connect the shortcut to your move_point_one function

# Optional: Display message indicating the shortcut is active
iface.messageBar().pushInfo("Shortcut Active", "Press Shift+1 to place the first point in the copied coordinates")
