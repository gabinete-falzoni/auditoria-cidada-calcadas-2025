from qgis.core import QgsExpressionContext, QgsExpressionContextUtils, QgsFeatureRequest

# --- Get the active layer and first selected feature ---
layer = iface.activeLayer()

# For testing, just take the first feature; you can later add selection logic
feature = next(layer.getFeatures())

# --- Find your "Play video" action ---
play_action = None
for a in layer.actions().actions():
    if a.name() == "Play video":   # exact name
        play_action = a
        break

if play_action is None:
    print("Play video action not found")
else:
    print("Found action:", play_action.name())
    
    # --- Build expression context for variable expansion ---
    context = QgsExpressionContext()
    # add global + layer scopes
    context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))
    # add the feature for [%field%] references
    context.setFeature(feature)
    
    # --- Run the action ---
    print("Running Play Video action…")
    play_action.run(layer, feature, context)
