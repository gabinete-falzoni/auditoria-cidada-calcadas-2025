# ==============================================================================
# Face anonymization with YuNet (robust for small faces)
# ==============================================================================

library(reticulate)
library(opencv)

# ------------------------------------------------------------------------------
# Python environment
# ------------------------------------------------------------------------------

use_condaenv(
  condaenv = "faceblur",
  conda = "/mnt/fern/Dados/miniconda3/condabin/conda",
  required = TRUE
)

cv2 <- import("cv2", convert = FALSE)
np  <- import("numpy", convert = FALSE)

py_config()

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

pasta_base  <- "/mnt/fern/Dados"
pasta_audi  <- sprintf("%s/projetos/2025_Auditoria_Calcadas", pasta_base)
pasta_proc  <- sprintf("%s/01_dados_processados", pasta_audi)
pasta_publi <- sprintf("%s/06_publicacao", pasta_proc)
pasta_web   <- sprintf("%s/qgis2web/keyframes", pasta_publi)

pasta_models <- sprintf("%s/models", pasta_web)
output_dir   <- sprintf("%s/images_blurred", pasta_web)

dir.create(pasta_models, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Download YuNet ONNX model (2023 version)
# ------------------------------------------------------------------------------

yunet_path <- file.path(pasta_models, "face_detection_yunet.onnx")

if (!file.exists(yunet_path)) {
  download.file(
    "https://github.com/opencv/opencv_zoo/raw/main/models/face_detection_yunet/face_detection_yunet_2023mar.onnx",
    yunet_path,
    mode = "wb"
  )
}

# ------------------------------------------------------------------------------
# Helper: Detect faces with YuNet
# ------------------------------------------------------------------------------

detect_faces <- function(image, model_path = yunet_path) {
  cv2 <- import("cv2")
  np <- import("numpy")

  if (!inherits(image, "numpy.ndarray")) stop("image must be a numpy array")

  shape <- py_to_r(image$shape)
  h <- as.integer(shape[[1]])
  w <- as.integer(shape[[2]])

  input_size <- tuple(w, h)

  detector <- cv2$FaceDetectorYN_create(
    model_path,     # ONNX model
    "",             # config (none)
    input_size,     # input size
    score_threshold = 0.2,  # lower = detect more/small faces
    nms_threshold   = 0.3,
    top_k           = 5000L,
    backend_id      = 0L,
    target_id       = 0L
  )

  result <- detector$detect(image)

  if (length(result) < 2 || is.null(result[[2]])) return(list())

  detections <- py_to_r(result[[2]])

  faces <- list()
  for (i in 1:nrow(detections)) {
    x <- as.integer(detections[i, 0])
    y <- as.integer(detections[i, 1])
    width <- as.integer(detections[i, 2])
    height <- as.integer(detections[i, 3])

    # Clamp coordinates to image bounds
    x1 <- max(0, x); y1 <- max(0, y)
    x2 <- min(w, x + width); y2 <- min(h, y + height)

    if (x2 > x1 && y2 > y1) faces[[length(faces) + 1]] <- c(x1, y1, x2, y2)
  }

  faces
}

# ------------------------------------------------------------------------------
# Pixelation-based face blur
# ------------------------------------------------------------------------------

blur_face <- function(image, box) {

  x1 <- max(0, box[1]); y1 <- max(0, box[2])
  x2 <- min(py_to_r(image$shape)[[2]], box[3])
  y2 <- min(py_to_r(image$shape)[[1]], box[4])

  if (x2 <= x1 || y2 <= y1) return(image)

  roi <- image[y1:y2, x1:x2, ]

  shape <- py_to_r(roi$shape)
  h <- as.integer(shape[[1]])
  w <- as.integer(shape[[2]])

  scale <- max(1L, as.integer(min(w, h) / 8))

  small <- cv2$resize(
    roi,
    tuple(max(1L, as.integer(w / scale)), max(1L, as.integer(h / scale))),
    interpolation = cv2$INTER_LINEAR
  )

  pixelated <- cv2$resize(
    small,
    tuple(w, h),
    interpolation = cv2$INTER_NEAREST
  )

  image[y1:y2, x1:x2, ] <- pixelated

  image
}

# ------------------------------------------------------------------------------
# Process one image (optional resizing for speed)
# ------------------------------------------------------------------------------

process_image <- function(img_path, max_width = 960) {

  img <- cv2$imread(normalizePath(img_path))
  if (is.null(img)) return(NULL)

  shape <- py_to_r(img$shape)
  if (length(shape) < 2 || any(is.na(shape))) return(NULL)

  h <- as.integer(shape[[1]])
  w <- as.integer(shape[[2]])
  scale <- 1.0

  # Resize for speed if too wide
  if (w > max_width) {
    scale <- max_width / w
    new_w <- max(1L, as.integer(w * scale))
    new_h <- max(1L, as.integer(h * scale))
    img_small <- cv2$resize(img, tuple(new_w, new_h))
  } else {
    img_small <- img
  }

  faces <- detect_faces(img_small)
  if (length(faces) == 0) return(img)

  for (box in faces) {
    x1 <- as.integer(box[1] / scale)
    y1 <- as.integer(box[2] / scale)
    x2 <- as.integer(box[3] / scale)
    y2 <- as.integer(box[4] / scale)

    img <- blur_face(img, c(x1, y1, x2, y2))
  }

  img
}

# ------------------------------------------------------------------------------
# Batch processing
# ------------------------------------------------------------------------------

image_files <- list.files(
  pasta_web,
  pattern = "\\.jpg$",
  full.names = TRUE
)

cat("Total images:", length(image_files), "\n")

for (i in seq_along(image_files)) {
  # img_path <- image_files[2]
  img_path <- image_files[i]
  cat(sprintf("[%d/%d] %s\n", i, length(image_files), basename(img_path)))

  result <- process_image(img_path)
  if (is.null(result)) next

  out_path <- file.path(output_dir, basename(img_path))
  cv2$imwrite(out_path, result)
}

cat("✅ Done! Images saved to:", output_dir, "\n")