from qgis.PyQt.QtWidgets import QAction
from qgis.PyQt.QtGui import QKeySequence
from qgis.core import QgsFeatureRequest, QgsExpressionContext, QgsExpressionContextUtils

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

    # find the action command (string)
    actions = layer.actions().actions()
    action_command = None
    for a in actions:
        if a.name() == "Play video":   # <-- your action name
            action_command = a.action()  # <-- this returns the *string* command
            break

    if not action_command:
        iface.messageBar().pushWarning("Play video", "Layer action not found.")
        return

    # Build expression context
    context = QgsExpressionContext()
    context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))

    # ✔️ Correct call for your QGIS version:
    # doAction(action_string, feature, context)
    layer.actions().doAction(action_command, feature, context)

# Create the keyboard shortcut
shortcut_action = QAction("Run Play Video", iface.mainWindow())
shortcut_action.setShortcut(QKeySequence("Alt+A"))
shortcut_action.triggered.connect(run_play_video_action)
iface.mainWindow().addAction(shortcut_action)

print("Shortcut Alt+A ready — select 1 feature and press Alt+A.")
