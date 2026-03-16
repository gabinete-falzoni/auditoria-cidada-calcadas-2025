from qgis.core import (
    QgsProject,
    QgsFeature,
    QgsGeometry,
    QgsPointXY,
    QgsLineString,
    QgsVectorLayer,
    QgsFields,
    QgsField,
    QgsWkbTypes,
    QgsFeatureSink,
    QgsVectorFileWriter,
    QgsCoordinateReferenceSystem,
)
from PyQt5.QtCore import QVariant
from collections import defaultdict


# --------------------------------------------
# 1. Get your point layer
# --------------------------------------------
#point_layer = QgsProject.instance().mapLayersByName('your_point_layer_name')[0]
point_layer = iface.activeLayer()

# Make sure points are sorted correctly (e.g., by ID or time)
sorted_feats = sorted(point_layer.getFeatures(), key=lambda f: f.attribute('fid'))

# --------------------------------------------
# 2. Group points by consecutive same SQL values
# --------------------------------------------
segments = []
current_sql = None
current_segment = []

## Considerar todos os pontos
# for feat in sorted_feats:
#     sql_value = feat['sql']
#     point = feat.geometry().asPoint()
    
#     if current_sql is None:
#         current_sql = sql_value

#     if sql_value != current_sql:
#         if len(current_segment) >= 2:
#             segments.append((current_sql, current_segment))
#         current_segment = []
#         current_sql = sql_value

#     current_segment.append(point)

# Considerar somente pontos onde o atributo não for NULL
for feat in sorted_feats:
    sql_value = feat['sql']
    
    # # Skip features where sql is NULL
    # if sql_value is None:
    #     continue
    
    point = feat.geometry().asPoint()

    if current_sql is None:
        current_sql = sql_value

    if sql_value != current_sql:
        if len(current_segment) >= 2:
            segments.append((current_sql, current_segment))
        current_segment = []
        current_sql = sql_value

    current_segment.append(point)


# Add the last segment
if len(current_segment) >= 2:
    segments.append((current_sql, current_segment))

# --------------------------------------------
# 3. Create a new LineString layer for output
# --------------------------------------------
crs = point_layer.crs().toWkt()
fields = QgsFields()
fields.append(QgsField('sql', QVariant.Int))

line_layer = QgsVectorLayer('LineString?crs=' + crs, 'Segmented_Line', 'memory')
prov = line_layer.dataProvider()
prov.addAttributes(fields)
line_layer.updateFields()

# --------------------------------------------
# 4. Add features (LineStrings) to the line layer
# --------------------------------------------
for sql_val, point_list in segments:
    line_geom = QgsGeometry.fromPolylineXY([QgsPointXY(p) for p in point_list])
    new_feat = QgsFeature()
    new_feat.setGeometry(line_geom)
    new_feat.setAttributes([sql_val])
    prov.addFeature(new_feat)

line_layer.updateExtents()
QgsProject.instance().addMapLayer(line_layer)
