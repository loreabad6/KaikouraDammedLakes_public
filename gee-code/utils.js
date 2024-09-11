/**
 * Function to mask clouds using the Sentinel-2 QA band
 * @param {ee.Image} image Sentinel-2 image
 * @return {ee.Image} cloud masked Sentinel-2 image
 */
exports.maskS2clouds = function(image) {
  var qa = image.select('QA60');
  
  var orig_time = image.get('system:time_start')
  
  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image
          .updateMask(mask)
          .divide(10000)
          .copyProperties(image)
          .set({'system:time_start': orig_time});
};

/**
 * Function to apply a topographic correction to percentile composite
 * Taken from: https://mygeoblog.com/2018/10/17/terrain-correction-in-gee/
 * Optimized for sentinel 2, and for renamed bands!
 * @param collection {ee.ImageCollection} image collection for Sentinel-2 
 * @param dem {ee.Image} DEM to use for topographic correction
 * @param sageom {ee.FeatureCollection} geometry to clip images
 * @return {ee.Image} topographically corrected collection of Sentinel-2 images
 */

var degree2radian = 0.01745;
exports.topoCorrection = function(collection, dem, sageom) {
 
  collection = collection.map(illuminationCondition);
  collection = collection.map(illuminationCorrection);
 
  return(collection);
 
  ////////////////////////////////////////////////////////////////////////////////
  // Function to calculate illumination condition (IC). 
  // Function by Patrick Burns and Matt Macander
  function illuminationCondition(img){
 
  // Extract image metadata about solar position
  var SZ_rad = ee.Image.constant(ee.Number(img.get('MEAN_SOLAR_ZENITH_ANGLE')))
                .multiply(3.14159265359).divide(180).clip(sageom.geometry().buffer(10000));
  var SA_rad = ee.Image.constant(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE'))
                .multiply(3.14159265359).divide(180)).clip(sageom.geometry().buffer(10000));
  // Creat terrain layers
  var slp = ee.Terrain.slope(dem).clip(sageom.geometry().buffer(10000));
  var slp_rad = ee.Terrain.slope(dem).multiply(3.14159265359).divide(180)
                .clip(sageom.geometry().buffer(10000));
  var asp_rad = ee.Terrain.aspect(dem).multiply(3.14159265359).divide(180)
                .clip(sageom.geometry().buffer(10000));
 
  // Calculate the Illumination Condition (IC)
  // slope part of the illumination condition
  var cosZ = SZ_rad.cos();
  var cosS = slp_rad.cos();
  var slope_illumination = cosS.expression("cosZ * cosS",
                                          {'cosZ': cosZ,
                                          'cosS': cosS.select('slope')});
  // aspect part of the illumination condition
  var sinZ = SZ_rad.sin();
  var sinS = slp_rad.sin();
  var cosAziDiff = (SA_rad.subtract(asp_rad)).cos();
  var aspect_illumination = sinZ.expression("sinZ * sinS * cosAziDiff",
                                          {'sinZ': sinZ,
                                            'sinS': sinS,
                                            'cosAziDiff': cosAziDiff});
  // full illumination condition (IC)
  var ic = slope_illumination.add(aspect_illumination);
 
  // Add IC to original image
  var img_plus_ic = ee.Image(img.addBands(ic.rename('IC')).addBands(cosZ.rename('cosZ'))
                      .addBands(cosS.rename('cosS')).addBands(slp.rename('slope')));
  return img_plus_ic;
  }
 
// Function to apply the Sun-Canopy-Sensor + C (SCSc) correction method to each
// image. Function by Patrick Burns and Matt Macander
 
function illuminationCorrection(img){
    var props = img.toDictionary();
    var st = img.get('system:time_start');
 
    var img_plus_ic = img;
    var mask1 = img_plus_ic.select('nir').gt(-0.1);
    var mask2 = img_plus_ic.select('slope').gte(5)
                            .and(img_plus_ic.select('IC').gte(0))
                            .and(img_plus_ic.select('nir').gt(-0.1));
    var img_plus_ic_mask2 = ee.Image(img_plus_ic.updateMask(mask2));
 
    // Specify Bands to topographically correct
    var bandList = ['blue','green','red','nir','swir1','swir2'];
    var compositeBands = img.bandNames();
    var nonCorrectBands = img.select(compositeBands.removeAll(bandList));
 
   // var geom = ee.Geometry(img.get('system:footprint')).bounds().buffer(10000);
 
    function apply_SCSccorr(band){
      var method = 'SCSc';
      var out = img_plus_ic_mask2.select('IC', band).reduceRegion({
        // Compute coefficients: a(slope), b(offset), c(b/a)
        reducer: ee.Reducer.linearFit(),
        // trim off the outer edges of the image for linear relationship
        geometry: ee.Geometry(sageom.geometry().buffer(-5000)), 
        scale: 300,
        maxPixels: 1000000000
      });  
 
  if (out === null || out === undefined ){
      return img_plus_ic_mask2.select(band);
      }
 
  else{
      var out_a = ee.Number(out.get('scale'));
      var out_b = ee.Number(out.get('offset'));
      var out_c = out_b.divide(out_a);
      // Apply the SCSc correction
      var SCSc_output = img_plus_ic_mask2.expression(
        "((image * (cosB * cosZ + cvalue)) / (ic + cvalue))", {
        'image': img_plus_ic_mask2.select(band),
        'ic': img_plus_ic_mask2.select('IC'),
        'cosB': img_plus_ic_mask2.select('cosS'),
        'cosZ': img_plus_ic_mask2.select('cosZ'),
        'cvalue': out_c
      });
 
      return SCSc_output;
    }
 
    }
 
    var img_SCSccorr = ee.Image(bandList.map(apply_SCSccorr))
                          .addBands(img_plus_ic.select('IC'));
    var bandList_IC = ee.List([bandList, 'IC']).flatten();
    img_SCSccorr = img_SCSccorr.unmask(img_plus_ic.select(bandList_IC)).select(bandList);
 
    return img_SCSccorr.addBands(nonCorrectBands)
      .setMulti(props)
      .set('system:time_start',st);
  }
};

/**
  * The script computes surface water mask using Canny Edge detector and Otsu thresholding
  * See the following paper for details: http://www.mdpi.com/2072-4292/8/5/386
  * 
  * Author: Gennadii Donchyts (gennadiy.donchyts@gmail.com)
  * Contributors: Nicholas Clinton (nclinton@google.com) - re-implemented otsu() using ee.Array
  */ 
  
/**
  * Return the value that maximizes interclass variance in NDWI (in the region).
  */
var otsu = function(histogram) {
    histogram = ee.Dictionary(histogram);

    var counts = ee.Array(histogram.get('histogram'));
    var means = ee.Array(histogram.get('bucketMeans'));
    var size = means.length().get([0]);
    var total = counts.reduce(ee.Reducer.sum(), [0]).get([0]);
    var sum = means.multiply(counts).reduce(ee.Reducer.sum(), [0]).get([0]);
    var mean = sum.divide(total);

    var indices = ee.List.sequence(1, size);

    // Compute between sum of squares, where each mean partitions the data.
    var bss = indices.map(function(i) {
        var aCounts = counts.slice(0, 0, i);
        var aCount = aCounts.reduce(ee.Reducer.sum(), [0]).get([0]);
        var aMeans = means.slice(0, 0, i);
        var aMean = aMeans.multiply(aCounts)
            .reduce(ee.Reducer.sum(), [0]).get([0])
            .divide(aCount);
        var bCount = total.subtract(aCount);
        var bMean = sum.subtract(aCount.multiply(aMean)).divide(bCount);
        return aCount.multiply(aMean.subtract(mean).pow(2)).add(
            bCount.multiply(bMean.subtract(mean).pow(2)));
    });

    // Return the mean value corresponding to the maximum BSS.
    return means.sort(bss).get([-1]);
};

/**
  * Compute a threshold using Otsu method (bimodal)
  */
exports.computeThresholdUsingOtsu = function(image, scale, bounds, cannyThreshold, cannySigma, minValue, debug) {
    // clip image edges
    var mask = image.mask().gt(0).focal_min(ee.Number(scale).multiply(5), 'circle', 'meters');

    // detect sharp changes
    var edge = ee.Algorithms.CannyEdgeDetector(image, cannyThreshold, cannySigma);
    edge = edge.multiply(mask);

    // buffer around NDWI edges
    var edgeBuffer = edge.focal_max(ee.Number(scale).multiply(1), 'square', 'meters');
    var imageEdge = image.mask(edgeBuffer);

    // compute threshold using Otsu thresholding
    var buckets = 100;
    var hist = ee.Dictionary(ee.Dictionary(imageEdge.reduceRegion(ee.Reducer.histogram(buckets), bounds, scale)).values().get(0));

    var threshold = ee.Algorithms.If(hist.contains('bucketMeans'), otsu(hist), 0.3);
    threshold = ee.Number(threshold);//.add(0.05)

    if(debug) {
        Map.addLayer(edge.mask(edge), {palette:['ff0000']}, 'edges', false);

        //print(ui.Chart.image.histogram(image, bounds, scale, buckets));
        print(ui.Chart.image.histogram(imageEdge, bounds, scale, buckets));
    }

    return minValue !== 'undefined' ? threshold.max(minValue) : threshold;
};

/**
  * Function to pansharpen Landsat 8 data to 15 meters
  */
var pansharpenL8 = function(scene, kernel) {
  // Compute the per-pixel means of the unsharpened bands.
  var bgr = scene.select(['B7','B6','B5','B4', 'B3', 'B2']);
  var pan = scene.select('B8');
  var bgr_mean = bgr.reduce('mean').rename('mean');
  // Add a kernel
  kernel = ee.Kernel.square({
  radius: 90, 
  units: 'meters'
  });
  // Compute the aggregate mean of the unsharpened bands and the pan band.
  var mean_values = pan.addBands(bgr_mean).reduceNeighborhood({
    reducer: ee.Reducer.mean(), 
    kernel: kernel,
  });
  var gain = mean_values.select('mean_mean').divide(mean_values.select('B8_mean'));
  return bgr.divide(bgr_mean).multiply(pan).multiply(gain);
};