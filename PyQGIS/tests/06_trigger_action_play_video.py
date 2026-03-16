from qgis.PyQt.QtWidgets import QAction
from qgis.PyQt.QtGui import QKeySequence
from qgis.core import QgsExpressionContext, QgsFeatureRequest, QgsExpressionContextUtils

def run_play_video_action():
    layer = iface.activeLayer()
    if not layer:
        iface.messageBar().pushWarning("Play video", "No active layer.")
        return

    selected = layer.selectedFeatureIds()
    if len(selected) != 1:
        iface.messageBar().pushWarning("Play video", "Select exactly ONE feature.")
        return

    # get selected feature
    fid = selected[0]
    feature = next(layer.getFeatures(QgsFeatureRequest(fid)))

    # find the QgsAction object itself
    actions = layer.actions().actions()
    play_action = None
    for a in actions:
        if a.name() == "Play video":   # <-- your action name
            play_action = a
            break

    if not play_action:
        iface.messageBar().pushWarning("Play video", "Layer action not found.")
        return

    # Build expression context
    context = QgsExpressionContext()
    context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))

    # ✔️ The correct call: pass the action OBJECT, not an index
    layer.actions().doAction(play_action, feature, context)

# Create the keyboard shortcut
shortcut_action = QAction("Run Play Video", iface.mainWindow())
shortcut_action.setShortcut(QKeySequence("Alt+A"))
shortcut_action.triggered.connect(run_play_video_action)
iface.mainWindow().addAction(shortcut_action)

print("Shortcut Alt+A ready — select 1 feature and press Alt+A.")
