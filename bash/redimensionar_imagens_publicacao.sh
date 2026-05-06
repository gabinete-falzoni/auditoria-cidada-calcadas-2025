#!/bin/sh

# Rodar esse script de dentro da pasta gitlab/auditoria-cidada-calcadas-2025/bash/

# Atualizar endereço das fotos abaixo: 
TMP_FILES_FOLDER="/media/livre/Expansion/projetos/2025_Auditoria_Calcadas/01_dados_processados/06_publicacao/qgis2web/fotos_mapillary/";
CURRENT_FOLDER="`pwd`";
cd $TMP_FILES_FOLDER;

# Pasta temporária para fotos reduzidas
PROCESSED_FILES_FOLDER="fotos_reduzidas";
mkdir -p $PROCESSED_FILES_FOLDER

# Reduzir todas as imagens
# https://stackoverflow.com/questions/43253889/imagemagick-convert-how-to-tell-if-images-need-to-be-rotated
echo "Redimensionando imagens..."; 
for i in `ls *.jpg`; do
    NAME=`echo $i | cut -d "." -f 1`

    SIZE=`identify $i | cut -d " " -f 3`;
    HORIZ=`echo $SIZE | cut -d "x" -f 1`;
    VERT=`echo $SIZE | cut -d "x" -f 2`;
    
    if [ $HORIZ = $VERT ]; then
        convert -auto-orient $i -geometry 400x400! -depth 8 -density 100x100 -quality 70 jpg:$PROCESSED_FILES_FOLDER/$NAME.jpg
    # Usar -geometry 520x para redimensionar para 520px de largura e manter proporção
    elif [ $HORIZ -gt $VERT ]; then
        convert -auto-orient $i -geometry 440x -depth 8 -density 100x100 -quality 70 jpg:$PROCESSED_FILES_FOLDER/$NAME.jpg
    # Usar -geometry x500 para redimensionar para 500px de altura e manter proporção
    else
        convert -auto-orient $i -geometry x440 -depth 8 -density 100x100 -quality 70 jpg:$PROCESSED_FILES_FOLDER/$NAME.jpg
    fi

    done

echo "\nFeito!\n"
