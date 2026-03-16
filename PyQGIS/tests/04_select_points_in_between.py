from qgis.PyQt.QtWidgets import QAction
from qgis.PyQt.QtGui import QKeySequence

def select_between_fids():
    layer = iface.activeLayer()

    if not layer:
        iface.messageBar().pushWarning("Error", "No active layer selected.")
        return

    selected_fids = layer.selectedFeatureIds()

    if len(selected_fids) < 2:
        iface.messageBar().pushWarning("Error", "Select at least two points first.")
        return

    fid_min = min(selected_fids)
    fid_max = max(selected_fids)

    fids_to_select = [fid for fid in layer.allFeatureIds() if fid_min <= fid <= fid_max]

    layer.selectByIds(fids_to_select)

    # iface.messageBar().pushInfo("Done", f"Selected FIDs from {fid_min} to {fid_max}.")

# --- Create action with shortcut ---
action = QAction("Select FID Range", iface.mainWindow())
action.setShortcut(QKeySequence("Shift+A"))
action.triggered.connect(select_between_fids)

# Register the action in QGIS (adds to Plugins menu)
iface.addPluginToMenu("Custom Tools", action)

# Also add to toolbar (optional)
iface.addToolBarIcon(action)

print("Shortcut Shift+A is now active.")
