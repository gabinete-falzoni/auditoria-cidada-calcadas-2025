# Add action to play video in layer
from qgis.core import QgsAction

# Get your layer
# layer = QgsProject.instance().mapLayersByName("your_layer_name")[0]
# Active layer
layer = iface.activeLayer()

# Remove existing 'Play video' actions (by name match)
for a in layer.actions().actions():
    if a.name().strip() == 'Play video':
        layer.actions().removeAction(a.id())
        print(f"🗑 Removed action: {a.name()}")

# Add action
if layer and layer.type() == QgsMapLayer.VectorLayer:
    # description = 'ffplay -ss [%start_time%] -vf scale=854:480 -t 3 -autoexit [%video_path%]'
    # description = 'ffplay -ss [%start_time%] -vf scale=854:480 -t 3 -autoexit [% @project_home || '/' || "video_path" %]'
    description = 'ffplay -ss [%start_time%] -vf scale=854:480 -t 3 -autoexit "file:///[% @project_home || \'/\' || "video_path" %]"'
    # description = 'ffplay -ss [%start_time%] -vf scale=854:480 -t 3 -autoexit [% path_join(@project_home, "video_path") %]'
    action_text = 'Play video'

    # Create action
    action = QgsAction(
        QgsAction.Generic,   # Action type
        action_text,         # The actual command to run
        description,         # Description shown in the GUI
        None,                # Short name (optional)
        False,                # Capture output (True = runs in external terminal)
    )

    # Set scopes manually using bitmask values
    SCOPE_FEATURE = 'Canvas'
    SCOPE_CANVAS = 'Feature'
    action.setActionScopes([SCOPE_FEATURE, SCOPE_CANVAS])

    # Add action to layer
    layer.actions().addAction(action)

    print("✔️ 'Play video' action added with Canvas and Feature scopes.")
else:
    print("⚠️ Please select a valid vector layer.")

# Add Map Hint HTML triggers to see image and video
if layer and layer.type() == QgsMapLayer.VectorLayer:
    # Set the Display Name (used in Identify Results)
    layer.setDisplayExpression('"imagepath"')

    # Set the HTML Map Tip (used on hover)
    # html_tip = '<img src="file:///[% imagepath %]" width="700" />'
    html_tip = '<img src="file:///[% @project_home || \'/\' || "imagepath" %]" width="700" />'
    # html_tip = '<img src="file://[% path_join(@project_home, "imagepath") %]" width="700" />'
    layer.setMapTipTemplate(html_tip)

    # Trigger update so QGIS UI reflects the change
    layer.triggerRepaint()
    iface.layerTreeView().refreshLayerSymbology(layer.id())

    print("✔️ Map Tip set successfully.")
else:
    print("⚠️ Please select a valid vector layer.")

# print(layer.mapTipTemplate())