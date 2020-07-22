### Process to generate Sentinel-2 pre event image before ingesting on Earth Engine:

- Create mosaic in QGIS using SCP (Congedo)
- Stack raster bands - 3.11 GB
- High compression level - 2.87 GB
- Reproject to 2193
- High compression level - 787MB
- Clipping to study area 
- Ingest on Earth Engine