from qgis.core import (
    QgsProject,
    QgsFeature,
    QgsGeometry,
    QgsPointXY,
    QgsVectorLayer,
    QgsFields,
    QgsField,
    QgsRendererCategory,
    QgsCategorizedSymbolRenderer,
    QgsSimpleLineSymbolLayer,
    QgsSymbol,
    edit
)

from PyQt5.QtCore import QVariant, Qt
from PyQt5.QtGui import QColor
import random

# --------------------------------------------
# 1. Get the active point layer
# --------------------------------------------
point_layer = iface.activeLayer()

# Sort features (by fid or other logical order)
# sorted_feats = sorted(point_layer.getFeatures(), key=lambda f: f.attribute('new_fid'))
sorted_feats = sorted(point_layer.getFeatures(), key=lambda f: f.attribute('fid'))

# --------------------------------------------
# 2. Group by sequences (sql changes), including NULLs
# --------------------------------------------
segments = []
current_segment = []
current_sql = None

def add_segment(sql_val, segment):
    if len(segment) >= 2:
        segments.append((sql_val, segment))

for feat in sorted_feats:
    sql_val = feat['sql']
    point = feat.geometry().asPoint()

    if current_sql is None and current_segment == []:
        # Start the first segment
        current_sql = sql_val

    elif sql_val != current_sql:
        # SQL changed (even from/to None), end previous segment
        add_segment(current_sql, current_segment)
        current_segment = []
        current_sql = sql_val

    current_segment.append(point)

# Add last segment
add_segment(current_sql, current_segment)

# --------------------------------------------
# 3. Create output LineString layer
# --------------------------------------------
crs = point_layer.crs().toWkt()
fields = QgsFields()
fields.append(QgsField('sql', QVariant.String))  # Use String to support NULLs

line_layer = QgsVectorLayer('LineString?crs=' + crs, 'Segmented_Line', 'memory')
prov = line_layer.dataProvider()
prov.addAttributes(fields)
line_layer.updateFields()

# --------------------------------------------
# 4. Add segments to the line layer
# --------------------------------------------
for sql_val, point_list in segments:
    line_geom = QgsGeometry.fromPolylineXY(point_list)
    new_feat = QgsFeature()
    attr_val = str(sql_val) if sql_val is not None else None
    new_feat.setGeometry(line_geom)
    new_feat.setAttributes([attr_val])
    prov.addFeature(new_feat)

line_layer.updateExtents()
QgsProject.instance().addMapLayer(line_layer)


# --------------------------------------------
# 5. Apply categorized symbology to the layer
# SQL Null = zebrado, SQL simples = Azul; SQL vários = laranja
# --------------------------------------------

categories = []

# Category: NULL SQL
symbol_null = QgsSymbol.defaultSymbol(line_layer.geometryType())
symbol_null.setColor(QColor("green"))
symbol_null.setWidth(3)
symbol_null.symbolLayer(0).setPenStyle(Qt.DotLine)
category_null = QgsRendererCategory(None, symbol_null, "NULL")
categories.append(category_null)

# Category: Multiple values (contains comma)
symbol_multi = QgsSymbol.defaultSymbol(line_layer.geometryType())
symbol_multi.setColor(QColor("orange"))
symbol_multi.setWidth(3)
category_multi = QgsRendererCategory("multi", symbol_multi, "Multiple values (comma)")
categories.append(category_multi)

# Category: Single value
symbol_single = QgsSymbol.defaultSymbol(line_layer.geometryType())
symbol_single.setColor(QColor("blue"))
symbol_single.setWidth(3)
category_single = QgsRendererCategory("single", symbol_single, "Single value")
categories.append(category_single)

# Classification function
def classify_sql(val):
    if val is None or str(val).strip().lower() in ["", "null"]:
        return None

    val_str = str(val)

    if "," in val_str:
        return "multi"
    else:
        return "single"


# Add classification field
renderer_field = "sql_class"
line_layer.dataProvider().addAttributes([QgsField(renderer_field, QVariant.String)])
line_layer.updateFields()

# Assign classification to each feature
with edit(line_layer):
    for feat in line_layer.getFeatures():
        sql_val = feat["sql"]
        feat[renderer_field] = classify_sql(sql_val)
        line_layer.updateFeature(feat)


# Apply categorized renderer
renderer = QgsCategorizedSymbolRenderer(renderer_field, categories)
line_layer.setRenderer(renderer)

# Refresh visuals
line_layer.triggerRepaint()
iface.mapCanvas().refresh()


## --------------------------------------------
## 5. Apply categorized symbology to the layer
## --------------------------------------------

#categories = []

## Create classification field if needed
#renderer_field = "sql_class"
#line_layer.dataProvider().addAttributes([QgsField(renderer_field, QVariant.String)])
#line_layer.updateFields()

## Classify features
#with edit(line_layer):
#    for feat in line_layer.getFeatures():
#        sql_val = feat["sql"]
#        class_val = "NULL" if sql_val is None or str(sql_val).strip().lower() in ["", "null"] else str(sql_val)
#        feat[renderer_field] = class_val
#        line_layer.updateFeature(feat)

## Get unique classification values
#unique_values = set()
#for feat in line_layer.getFeatures():
#    val = feat[renderer_field]
#    unique_values.add(val)

## Build categories with custom symbols
#for val in unique_values:
#    if val == "NULL":
#        # Create a "Topo road"-like dual-layer symbol
#        symbol = QgsSymbol.defaultSymbol(line_layer.geometryType())
#        symbol.deleteSymbolLayer(0)  # remove default layer

#        # Base black line
#        base = QgsSimpleLineSymbolLayer()
#        base.setColor(QColor("black"))
#        base.setWidth(2.5)
#        base.setPenStyle(Qt.SolidLine)
#        base.setPenCapStyle(Qt.FlatCap)


#        # Top colored line
#        top = QgsSimpleLineSymbolLayer()
#        top.setColor(QColor("#ffffff"))  # white
#        top.setWidth(1.2)
#        top.setPenStyle(Qt.SolidLine)

#        symbol.appendSymbolLayer(base)
#        symbol.appendSymbolLayer(top)

#        label = "NULL"

#    else:
#        # Single-layer symbol with random color
#        symbol = QgsSymbol.defaultSymbol(line_layer.geometryType())
#        symbol.deleteSymbolLayer(0)  # remove default layer to avoid shared styling

#        simple_line = QgsSimpleLineSymbolLayer()
#        simple_line.setColor(QColor(random.randint(50, 220), random.randint(50, 220), random.randint(50, 220)))
#        simple_line.setWidth(3.0)
#        simple_line.setPenStyle(Qt.SolidLine)

#        symbol.appendSymbolLayer(simple_line)

#        label = val

#    category = QgsRendererCategory(val, symbol, label)
#    categories.append(category)

## Apply renderer
#renderer = QgsCategorizedSymbolRenderer(renderer_field, categories)
#line_layer.setRenderer(renderer)

## Refresh layer
#line_layer.triggerRepaint()
#iface.mapCanvas().refresh()
