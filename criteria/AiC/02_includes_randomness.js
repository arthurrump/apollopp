var randomMethods = [
  "java+method:///processing/core/PApplet/random(float,float)",
  "java+method:///processing/core/PApplet/random(float)",
  "java+method:///processing/core/PApplet/randomGaussian()",
  "java+method:///processing/core/PApplet/noise(float)",
  "java+method:///processing/core/PApplet/noise(float,float)",
  "java+method:///processing/core/PApplet/noise(float,float,float)"
]

var criterion = {
  "criterion": "Includes meaningful randomness (normal distribution and Perlin noise).",
  "patterns": randomMethods.map(randomMethod => ({
    "verdict": "positive",
    "pattern": [
      [ "method", { "annotation": { "scheme": "java+method" } }, "method" ],
      [ "method", "invokes", "randomMethod" ],
      [ "randomMethod", { "annotation": { "location": randomMethod } }, "randomMethod" ]
    ]
  }))
}
