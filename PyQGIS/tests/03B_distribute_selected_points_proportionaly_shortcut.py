# def space_points_by_dist_diff():
from qgis.PyQt.QtCore import QVariant
from qgis.core import (
    QgsProject,
    QgsGeometry,
    QgsPointXY,
    QgsCoordinateReferenceSystem,
    QgsCoordinateTransform,
    QgsFeatureRequest
)

layer = iface.activeLayer()
if not layer or layer.selectedFeatureCount() < 2:
    raise Exception("Select at least 2 points in a point layer")

# --- CRS setup ---
layer_crs = layer.crs()
utm = QgsCoordinateReferenceSystem("EPSG:31983")

if layer_crs != utm:
    transform_to_utm = QgsCoordinateTransform(layer_crs, utm, QgsProject.instance())
    transform_to_layer = QgsCoordinateTransform(utm, layer_crs, QgsProject.instance())
else:
    transform_to_utm = None
    transform_to_layer = None

# --- Get selected points and their dist_diff ---
selected_features = list(layer.selectedFeatures())
selected_features.sort(key=lambda f: f.id())

points = []
for f in selected_features:
    geom = f.geometry()
    pt = geom.asPoint()
    if transform_to_utm:
        pt_utm = transform_to_utm.transform(pt)
    else:
        pt_utm = pt
    dist_diff = f["dist_diff"] or 0
    points.append((f.id(), pt_utm, float(dist_diff)))

# --- define endpoints ---
start_pt = points[0][1]
end_pt = points[-1][1]

# --- compute cumulative distance proportions ---
dist_values = [p[2] for p in points]
total_weight = sum(dist_values)
if total_weight == 0:
    raise Exception("All dist_diff values are zero — cannot compute proportional spacing.")

cumulative = []
running = 0
for d in dist_values:
    running += d
    cumulative.append(running / total_weight)

# --- force first and last to stay fixed ---
cumulative[0] = 0.0
cumulative[-1] = 1.0

# --- First pass: proportional placement ---
layer.startEditing()
new_positions = []  # store UTM positions
for i, (fid, _, _) in enumerate(points):
    t = cumulative[i]
    new_x = start_pt.x() + (end_pt.x() - start_pt.x()) * t
    new_y = start_pt.y() + (end_pt.y() - start_pt.y()) * t
    new_point_utm = QgsPointXY(new_x, new_y)
    new_positions.append(new_point_utm)

# --- Second pass: handle dist_diff == 0 sequences ---
i = 0
while i < len(points):
    if points[i][2] == 0:
        start_idx = i - 1
        while i < len(points) and points[i][2] == 0:
            i += 1
        end_idx = i  # first nonzero after the block
        if start_idx >= 0 and end_idx < len(points):
            A = new_positions[start_idx]
            B = new_positions[end_idx]
            n_zeros = end_idx - start_idx - 1
            if n_zeros > 0:
                dx = (B.x() - A.x()) / (n_zeros + 1)
                dy = (B.y() - A.y()) / (n_zeros + 1)
                for j in range(1, n_zeros + 1):
                    new_positions[start_idx + j] = QgsPointXY(
                        A.x() + dx * j, A.y() + dy * j
                    )
    else:
        i += 1

# --- Apply final geometries ---
for idx, (fid, _, _) in enumerate(points):
    # Skip moving the first and last points (keep them fixed)
    if idx == 0 or idx == len(points) - 1:
        continue

    pt_utm = new_positions[idx]
    if transform_to_layer:
        new_point = transform_to_layer.transform(pt_utm)
    else:
        new_point = pt_utm

    new_geom = QgsGeometry.fromPointXY(new_point)
    layer.changeGeometry(fid, new_geom)

layer.commitChanges()
iface.mapCanvas().refresh()
layer.startEditing()
iface.actionVertexTool().trigger()


# # --- Shortcut registration ---
# from PyQt5.QtWidgets import QShortcut
# from PyQt5.QtGui import QKeySequence
# from PyQt5.QtCore import Qt

# shortcut1 = QShortcut(QKeySequence("Shift+D"), iface.mainWindow())
# shortcut1.setContext(Qt.ApplicationShortcut)
# shortcut1.activated.connect(space_points_by_dist_diff)
