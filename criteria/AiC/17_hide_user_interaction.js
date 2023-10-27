var criterion = {
  "criterion": "Do not hide user interaction in classes.",
  "patterns": [
    {
      "verdict": "positive",
      "pattern": [
        [ "PApplet", { "annotation": { "location": "java+class:///processing/core/PApplet" } }, "PApplet" ],
        [ "mainTab", { "annotation": { "scheme": "java+class" } }, "mainTab" ],
        [ "mainTab", "extends", "PApplet" ],
        [ "mainTab", "contains", "method" ],
        [ "method", { "annotation": { "scheme": "java+method" } }, "method" ],
        [ "method", "overrides", "PApplet.method" ]
      ],
      "children": [
        {
          "verdict": "neutral",
          "pattern": [
            [ "PApplet.method", { "annotation": { "location": "java+method:///processing/core/PApplet/draw()" } }, "PApplet.method" ]
          ]
        },
        {
          "verdict": "neutral",
          "pattern": [
            [ "PApplet.method", { "annotation": { "location": "java+method:///processing/core/PApplet/setup()" } }, "PApplet.method" ]
          ]
        },
        {
          "verdict": "neutral",
          "pattern": [
            [ "PApplet.method", { "annotation": { "location": "java+method:///processing/core/PApplet/settings()" } }, "PApplet.method" ]
          ]
        }
      ]
    },
    { 
      "verdict": "neutral",
      "pattern": [
        [ "method", "accessesField", "field" ],
        [ "method", { "annotation": { "scheme": "java+method" } }, "method" ]
      ],
      "children":
        [ "key", "keyCode", "keyPressed", "mouseButton", "mousePressed", "mouseX", "mouseY", "pmouseX", "pmouseY" ].map(field => (
          { 
            "verdict": "negative",
            "pattern": [
              [ "field", { "annotation": { "location": `java+field:///processing/core/PApplet/${field}` } }, "field" ]
            ],
            "children":
              [ "keyPressed()", "keyReleased()", "keyTyped()", "mouseClicked()", "mouseDragged()", "mouseMoved()", "mousePressed()", "mouseReleased()", "mouseWheel()" ].map(method => (
                {
                  "verdict": "neutral",
                  "pattern": [
                    [ "PApplet", { "annotation": { "location": "java+class:///processing/core/PApplet" } }, "PApplet" ],
                    [ "mainTab", { "annotation": { "scheme": "java+class" } }, "mainTab" ],
                    [ "mainTab", "extends", "PApplet" ],
                    [ "mainTab", "contains", "method" ],
                    [ "method", { "annotation": { "scheme": "java+method" } }, "method" ],
                    [ "method", "overrides", "PApplet.method" ],
                    [ "PApplet.method", { "annotation": { "location": `java+method:///processing/core/PApplet/${method}` } }, "PApplet.method" ]
                  ]
                }
              ))
          }
        ))
    }
  ]
}
