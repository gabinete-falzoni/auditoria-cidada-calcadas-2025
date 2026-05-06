# Borrar rostos de imagens keyframe para publicar mapa online

library('tidyverse')
library('tidylog')
library('sf')
library('opencv')
library('reticulate')


# library('leaflet')

# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_publi <- sprintf('%s/06_publicacao', pasta_proc)
pasta_resul <- sprintf('%s/resultados', pasta_publi)
pasta_web <- sprintf('%s/qgis2web/keyframes', pasta_publi)
pasta_models <- sprintf('%s/models', pasta_web)
dir.create(pasta_models, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Copiar imagens usadas como keyframes para pasta de publicação
# ------------------------------------------------------------------------------

# result <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resul)
# result <- read_sf(result) %>% st_drop_geometry() %>% filter(kf_flag)
#
# result <- result %>% select(imagepath)
#   mutate(imagepath = str_c(pasta_proc, imagepath, sep = '/'))
#
#
# for (img in result$imagepath) {
#   # img <- result$imagepath[1]
#   # print(img)
#
#   img_in  <- str_c(pasta_proc, img, sep = '/')
#   img_out <- str_c(pasta_web, basename(img), sep = '/')
#   file.copy(from = img_in, to = img_out)
# }
#
# rm(result, img, img_in, img_out)


# ------------------------------------------------------------------------------
# Python installation
# ------------------------------------------------------------------------------

# # use_condaenv(
# #   condaenv = "base",
# #   conda = "/mnt/fern/Dados/miniconda3/condabin/conda",
# #   required = TRUE
# # )
# # conda_create("faceblur")
#
# # Use Python environment
# use_condaenv(
#   condaenv = "faceblur",
#   conda = "/mnt/fern/Dados/miniconda3/condabin/conda",
#   required = TRUE
# )
#
# # # Instead of installing opencv via Conda, install via pip inside the environment.
# # # pip builds OpenCV with its own libraries, avoiding system mismatches.
# # py_install(c("opencv-python", "numpy"),
# #            envname = "faceblur",
# #            pip = TRUE)
# #
# # py_config()
#
# cv2 <- import("cv2")
# cv2$`__version__`
#
# # Download the required DNN files
# # deploy.prototxt (model architecture)
# download.file(
#   "https://raw.githubusercontent.com/opencv/opencv/master/samples/dnn/face_detector/deploy.prototxt",
#   destfile = sprintf("%s/deploy.prototxt", pasta_models)
# )
#
# # res10_300x300_ssd_iter_140000.caffemodel (weights)
# download.file(
#   "https://github.com/opencv/opencv_3rdparty/raw/dnn_samples_face_detector_20170830/res10_300x300_ssd_iter_140000.caffemodel",
#   destfile = sprintf("%s/res10_300x300_ssd_iter_140000.caffemodel", pasta_models),
#   mode = "wb"
# )
#
# list.files(pasta_models)
#
# net <- cv2$dnn$readNetFromCaffe(
#   sprintf("%s/deploy.prototxt", pasta_models),
#   sprintf("%s/res10_300x300_ssd_iter_140000.caffemodel", pasta_models)
# )


# ------------------------------------------------------------------------------
# Face blur
# ------------------------------------------------------------------------------

# Use Python environment
use_condaenv(
  condaenv = "faceblur",
  conda = "/mnt/fern/Dados/miniconda3/condabin/conda",
  required = TRUE
)
# cv2 <- import("cv2")
# np <- import("numpy")
# Avoid conversion to R array by reticulate
cv2 <- import("cv2", convert = FALSE)
np  <- import("numpy", convert = FALSE)
py_config()

# Load SSD face detection model
proto_path <- file.path(pasta_models, "deploy.prototxt")
weights_path <- file.path(pasta_models, "res10_300x300_ssd_iter_140000.caffemodel")

net <- cv2$dnn$readNetFromCaffe(proto_path, weights_path)

# -----------------------------
# Define helper functions
# -----------------------------

# Detect faces and return bounding boxes
detect_faces <- function(image, conf_threshold = 0.5) {
  h <- image$shape[1]
  w <- image$shape[2]

  # Convert image to blob for SSD
  blob <- cv2$dnn$blobFromImage(
    image, 1.0, tuple(300L, 300L),
    mean = tuple(104.0, 177.0, 123.0)
  )

  net$setInput(blob)
  detections <- net$forward()

  boxes <- list()

  for (i in 0:(detections$shape[2] - 1)) {
    confidence <- detections[0, 0, i, 2]
    if (confidence > conf_threshold) {
      x1 <- as.integer(detections[0, 0, i, 3] * w)
      y1 <- as.integer(detections[0, 0, i, 4] * h)
      x2 <- as.integer(detections[0, 0, i, 5] * w)
      y2 <- as.integer(detections[0, 0, i, 6] * h)

      # Clamp coordinates
      x1 <- max(0, x1)
      y1 <- max(0, y1)
      x2 <- min(w - 1, x2)
      y2 <- min(h - 1, y2)

      boxes <- append(boxes, list(c(x1, y1, x2, y2)))
    }
  }

  return(boxes)
}

# Blur a region in the image
blur_face <- function(image, box, ksize = 25) {
  x1 <- box[1]; y1 <- box[2]; x2 <- box[3]; y2 <- box[4]
  roi <- image[y1:y2, x1:x2, , drop = FALSE]
  blurred_roi <- cv2$GaussianBlur(roi, tuple(ksize, ksize), 0)
  image[y1:y2, x1:x2, ] <- blurred_roi
  return(image)
}

# -----------------------------
# 4. Batch processing
# -----------------------------
# input_dir <- "images_raw"   # folder with your 5k+ images
output_dir <- sprintf("%s/images_blurred", pasta_web)
dir.create(output_dir)

image_files <- list.files(pasta_web, pattern = "jpg$", recursive = FALSE, full.names = TRUE)

for (img_path in image_files) {
  # img_path <- image_files[2]
  cat("Processing:", basename(img_path), "\n")

  # Load image
  img <- cv2$imread(normalizePath(img_path))

  # Skip if unreadable
  if (is.null(img)) next

  # Detect faces
  faces <- detect_faces(img)

  # Apply blur to each detected face
  for (box in faces) {
    img <- blur_face(img, box, ksize = 25)
  }

  # Save output
  out_path <- file.path(output_dir, path_file(img_path))
  cv2$imwrite(out_path, img)
}

cat("✅ All images processed and saved to", output_dir, "\n")