# ==============================================================================
# Blur faces in images (OpenCV DNN via reticulate)
# ==============================================================================

library(reticulate)

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
# Load model (download once if needed)
# ------------------------------------------------------------------------------

proto_path   <- file.path(pasta_models, "deploy.prototxt")
weights_path <- file.path(pasta_models, "res10_300x300_ssd_iter_140000.caffemodel")

# if (!file.exists(proto_path)) {
#   download.file(
#     "https://raw.githubusercontent.com/opencv/opencv/master/samples/dnn/face_detector/deploy.prototxt",
#     proto_path
#   )
# }
#
# if (!file.exists(weights_path)) {
#   download.file(
#     "https://github.com/opencv/opencv_3rdparty/raw/dnn_samples_face_detector_20170830/res10_300x300_ssd_iter_140000.caffemodel",
#     weights_path,
#     mode = "wb"
#   )
# }

net <- cv2$dnn$readNetFromCaffe(proto_path, weights_path)

# ------------------------------------------------------------------------------
# Detect faces
# ------------------------------------------------------------------------------

# detect_faces <- function(image, conf_threshold = 0.5) {
# Lower threshold (detect more faces)
detect_faces <- function(image, conf_threshold = 0.3) {

  # --- Image shape ---
  shape_img <- py_to_r(image$shape)
  if (length(shape_img) < 2) return(list())

  h <- as.integer(shape_img[1])
  w <- as.integer(shape_img[2])

  # --- Blob ---
  blob <- cv2$dnn$blobFromImage(
    image,
    1.0,
    tuple(300L, 300L),
    mean = tuple(104.0, 177.0, 123.0)
  )

  net$setInput(blob)
  detections <- net$forward()

  # --- Detection shape ---
  shape_det <- py_to_r(detections$shape)
  if (length(shape_det) < 3) return(list())

  n <- as.integer(shape_det[3])
  if (is.na(n) || n <= 0) return(list())

  boxes <- list()

  for (i in 0:(n - 1)) {

    # ✅ ALWAYS py_to_r
    confidence <- py_to_r(detections[0, 0, i, 2])

    if (is.na(confidence) || confidence < conf_threshold) next

    x1 <- py_to_r(detections[0, 0, i, 3]) * w
    y1 <- py_to_r(detections[0, 0, i, 4]) * h
    x2 <- py_to_r(detections[0, 0, i, 5]) * w
    y2 <- py_to_r(detections[0, 0, i, 6]) * h

    x1 <- as.integer(x1)
    y1 <- as.integer(y1)
    x2 <- as.integer(x2)
    y2 <- as.integer(y2)

    # clamp
    x1 <- max(0, x1)
    y1 <- max(0, y1)
    x2 <- min(w - 1, x2)
    y2 <- min(h - 1, y2)

    if (x2 <= x1 || y2 <= y1) next

    boxes[[length(boxes) + 1]] <- c(x1, y1, x2, y2)
  }

  boxes
}

# ------------------------------------------------------------------------------
# Blur face (adaptive kernel)
# ------------------------------------------------------------------------------

# # Muito suave para rostos pequenos
# blur_face <- function(image, box) {
#
#   x1 <- box[1]; y1 <- box[2]; x2 <- box[3]; y2 <- box[4]
#
#   width  <- x2 - x1
#   height <- y2 - y1
#
#   # adaptive kernel (important improvement)
#   # ksize <- as.integer(max(15, min(101, floor(min(width, height) / 3))))
#   # Increase kernel aggressively:
#   ksize <- as.integer(max(31, floor(min(width, height))))
#   if (ksize %% 2 == 0) ksize <- ksize + 1
#
#   # must be odd
#   if (ksize %% 2 == 0) ksize <- ksize + 1
#
#   roi <- image[y1:y2, x1:x2, ]
#
#   blurred <- cv2$GaussianBlur(
#     roi,
#     tuple(ksize, ksize),
#     0
#   )
#
#   image[y1:y2, x1:x2, ] <- blurred
#
#   image
# }

# blur_face <- function(image, box) {
#
#   x1 <- box[1]; y1 <- box[2]; x2 <- box[3]; y2 <- box[4]
#
#   roi <- image[y1:y2, x1:x2, ]
#
#   # Get size
#   h <- as.integer(py_to_r(roi$shape)[1])
#   w <- as.integer(py_to_r(roi$shape)[2])
#
#   # Downscale aggressively
#   small <- cv2$resize(roi, tuple(10L, 10L), interpolation = cv2$INTER_LINEAR)
#
#   # Upscale back
#   pixelated <- cv2$resize(small, tuple(w, h), interpolation = cv2$INTER_NEAREST)
#
#   image[y1:y2, x1:x2, ] <- pixelated
#
#   image
# }

blur_face <- function(image, box) {

  x1 <- box[1]; y1 <- box[2]; x2 <- box[3]; y2 <- box[4]

  roi <- image[y1:y2, x1:x2, ]

  shape <- py_to_r(roi$shape)
  h <- shape[1]
  w <- shape[2]

  # Pixelate
  # small <- cv2$resize(roi, tuple(12L, 12L))
  small <- cv2$resize(roi, tuple(8L, 8L))
  roi <- cv2$resize(small, tuple(w, h), interpolation = cv2$INTER_NEAREST)

  # Strong blur on top
  roi <- cv2$GaussianBlur(roi, tuple(51L, 51L), 0)

  image[y1:y2, x1:x2, ] <- roi

  image
}

# ------------------------------------------------------------------------------
# Process one image
# ------------------------------------------------------------------------------

process_image <- function(img_path) {

  img <- cv2$imread(normalizePath(img_path))

  if (is.null(img)) return(NULL)

  faces <- detect_faces(img)

  if (length(faces) == 0) {
    return(img)  # no faces, return original
  }

  for (box in faces) {
    img <- blur_face(img, box)
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