from qgis.core import (
    QgsProject,
    QgsFeature,
    QgsGeometry,
    QgsVectorLayer,
    QgsFields,
    QgsField,
    edit
)
from PyQt5.QtCore import QVariant

# --------------------------------------------
# 1. Get the active point layer
# --------------------------------------------
point_layer = iface.activeLayer()

# Sort features (by fid or other logical order)
sorted_feats = sorted(point_layer.getFeatures(), key=lambda f: f.attribute('fid'))

# --------------------------------------------
# 2. Group by sequences (sql changes), including NULLs
# --------------------------------------------
segments = []
current_segment = []
current_sql = None
current_travessia_ok = None  # Track travessia_ok value for the current segment

def add_segment(sql_val, segment, travessia_ok):
    if len(segment) >= 2:
        segments.append((sql_val, segment, travessia_ok))

for feat in sorted_feats:
    # sql_val = feat['sql']
    sql_val = feat['group_id_rev']
    point = feat.geometry().asPoint()
    travessia_ok = feat['travessia_ok']  # Capture the original travessia_ok value

    if current_sql is None and current_segment == []:
        # Start the first segment
        current_sql = sql_val
        current_travessia_ok = travessia_ok

    elif sql_val != current_sql:
        # SQL changed (even from/to None), end previous segment
        add_segment(current_sql, current_segment, current_travessia_ok)
        current_segment = []
        current_sql = sql_val
        current_travessia_ok = travessia_ok

    current_segment.append(point)

# Add last segment
add_segment(current_sql, current_segment, current_travessia_ok)

# --------------------------------------------
# 3. Create output LineString layer
# --------------------------------------------
crs = point_layer.crs().toWkt()
fields = QgsFields()
fields.append(QgsField('group_id_rev', QVariant.String))  # Use String to support NULLs
fields.append(QgsField('categoria_travessia', QVariant.String))  # Keep the original value as a string

line_layer = QgsVectorLayer('LineString?crs=' + crs, 'Segmented_Line', 'memory')
prov = line_layer.dataProvider()
prov.addAttributes(fields)
line_layer.updateFields()

# --------------------------------------------
# 4. Add segments to the line layer
# --------------------------------------------
for sql_val, point_list, travessia_ok in segments:
    line_geom = QgsGeometry.fromPolylineXY(point_list)
    new_feat = QgsFeature()
    attr_val_sql = str(sql_val) if sql_val is not None else None
    new_feat.setGeometry(line_geom)
    new_feat.setAttributes([attr_val_sql, travessia_ok])  # Preserve original travessia_ok value
    prov.addFeature(new_feat)

line_layer.updateExtents()
QgsProject.instance().addMapLayer(line_layer)

# Refresh visuals
line_layer.triggerRepaint()
iface.mapCanvas().refresh()
