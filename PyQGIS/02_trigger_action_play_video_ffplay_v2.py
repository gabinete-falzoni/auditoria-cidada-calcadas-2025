# Cria um atalho para executar a acao de "play video", que ja deve estar
# inserida na camada

from qgis.PyQt.QtWidgets import QAction
from qgis.PyQt.QtGui import QKeySequence
from qgis.core import QgsExpressionContext, QgsExpressionContextUtils, QgsFeatureRequest

def run_play_video_action():
    layer = iface.activeLayer()
    if not layer:
        iface.messageBar().pushWarning("Play Video", "No active layer.")
        return

    selected_ids = layer.selectedFeatureIds()

    if len(selected_ids) == 0:
        iface.messageBar().pushInfo("Play Video", "No feature selected.")
        return

    # If multiple features → notify user and use the highest FID
    if len(selected_ids) > 1:
        iface.messageBar().pushInfo(
            "Play Video",
            f"Multiple features selected. Using feature with highest FID ({max(selected_ids)})."
        )

    # Use last / highest FID
    fid = max(selected_ids)
    feature = next(layer.getFeatures(QgsFeatureRequest(fid)))

    # Find the "Play video" action
    play_action = None
    for a in layer.actions().actions():
        if a.name() == "Play video":   # exact name
            play_action = a
            break

    if play_action is None:
        iface.messageBar().pushWarning("Play Video", "Action not found.")
        return

    # Build expression context
    context = QgsExpressionContext()
    context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))
    context.setFeature(feature)

    # Run the action
    play_action.run(layer, feature, context)

# --- Create a shortcut Alt+A to trigger the action ---
shortcut_action = QAction("Run Play Video", iface.mainWindow())
shortcut_action.setShortcut(QKeySequence("Alt+A"))
shortcut_action.triggered.connect(run_play_video_action)
iface.mainWindow().addAction(shortcut_action)

print("Shortcut Alt+A ready — select 1 feature (or more) and press Alt+A.")

