import sys

if sys.platform.startswith('win'):
    layer = iface.activeLayer()

    if not layer.isEditable():
        layer.startEditing()

    for feature in layer.getFeatures():
        path = feature["video_path"]
        if isinstance(path, str):
            windows_path = path.replace('/', '\\\\')  # Double backslash for Python escape
            layer.changeAttributeValue(feature.id(), layer.fields().indexFromName("video_path"), windows_path)

    layer.commitChanges()
    print("🪟 Windows")
    print("✅ Paths converted to Windows format.")
elif sys.platform.startswith('linux'):    
    layer = iface.activeLayer()

    if not layer.isEditable():
        layer.startEditing()

    for feature in layer.getFeatures():
        path = feature["video_path"]
        if isinstance(path, str):
            windows_path = path.replace('\\\\', '/')  # Double backslash for Python escape
            layer.changeAttributeValue(feature.id(), layer.fields().indexFromName("video_path"), windows_path)

    layer.commitChanges()
    print("🐧 Linux")
elif sys.platform.startswith('darwin'):
    print("🍏 macOS")
    
# path_sep = '\\' if platform.system() == 'Windows' else '/'
