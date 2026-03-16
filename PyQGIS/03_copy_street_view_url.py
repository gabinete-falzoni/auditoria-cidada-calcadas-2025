# Copia o latlong do ponto selecionado e cria um endereco de Google Street View
# para conferencia. Copiar o endereco e colar em um navegador, pois esta parte
# nao funciona com o QGIS rodando em Docker

from PyQt5.QtWidgets import QApplication, QShortcut
from PyQt5.QtGui import QKeySequence
from qgis.core import QgsProject
from qgis.utils import iface
from PyQt5.QtCore import Qt

# Step 1: Define the function that will be triggered by the shortcut
def copy_street_view_url():
    # Step 2: Get the selected feature from the active layer
    layer = iface.activeLayer()  # Get the currently active layer
    if layer is None:
        iface.messageBar().pushWarning("No active layer", "Please select a layer.")
    else:
        selected_features = layer.selectedFeatures()  # Get the selected features
        
        # Step 3: Check if any feature is selected
        if selected_features:
            feature = selected_features[0]  # Assuming you only select one feature
            geometry = feature.geometry()
            
            if geometry.isEmpty():
                iface.messageBar().pushWarning("Invalid geometry", "The selected feature does not have valid geometry.")
            else:
                # Step 4: Extract the coordinates (latitude, longitude)
                point = geometry.asPoint()
                longitude = point.x()
                latitude = point.y()
                
                # Step 5: Construct the Google Street View URL
                google_street_view_url = f"http://maps.google.com/?cbll={latitude},{longitude}&cbp=12,20.09,,0,5&layer=c"
                
                # Step 6: Copy the URL to the clipboard using QClipboard
                clipboard = QApplication.clipboard()  # Get the clipboard instance
                clipboard.setText(google_street_view_url)  # Copy the URL to clipboard
                
                # Step 7: Display message bar to inform the user
                iface.messageBar().pushInfo("URL Copied", "Google Street View link has been copied to the clipboard. Paste it into a browser.")
                print(f"Google Street View URL: {google_street_view_url}")  # Optional: Print the URL to the console
                
        else:
            iface.messageBar().pushWarning("No features selected", "Please select a point feature.")

# Step 8: Create a QShortcut to bind the keyboard shortcut (Alt+Shift+C)
shortcut = QShortcut(QKeySequence(Qt.ALT + Qt.SHIFT + Qt.Key_C), iface.mainWindow())  # Alt+Shift+C
shortcut.activated.connect(copy_street_view_url)  # Connect the shortcut to the function

# Optional: Display message indicating the shortcut is active
iface.messageBar().pushInfo("Shortcut Active", "Press Alt+Shift+C to copy the Google Street View URL for the selected point.")
