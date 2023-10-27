var criterion = {
  "criterion": "Methods are only public if they are intended to be used by other classes.",
  "patterns": [
    {
      "verdict": "negative",
      "pattern": [
        [ "method", { "annotation": { "scheme": "java+method" } }, "method" ],
        [ "method", { "annotation": { "modifier": "public" } }, "method" ],
        [ "class", { "annotation": { "scheme": "java+class" } }, "class" ],
        [ "class", "contains", "method" ]
      ],
      "children": [
        {
          "verdict": "neutral",
          "pattern": [
            [ "other", "invokes", "method" ]
          ]
        },
        {
          "verdict": "neutral",
          "pattern": [
            [ "method", { "annotation": { "modifier": "static" } }, "method" ],
            [ "method", "dependsOn", "void" ],
            [ "void", { "annotation": { "location": "java+primitiveType:///void" } }, "void" ],
            [ "method", "contains", "param" ],
            [ "param", { "annotation": { "scheme": "java+parameter" } }, "param" ],
            [ "param", "dependsOn", "stringArray" ],
            [ "stringArray", { "annotation": { "location": "java+array:///java/lang/String%5B%5D" } }, "stringArray" ]
          ]
        },
        {
          "verdict": "neutral",
          "pattern": [
            [ "method", "dependsOn", "@Override" ],
            [ "@Override", { "annotation": { "location": "java+interface:///java/lang/Override" } }, "@Override" ]
          ]
        }
      ].concat(
      [ "java+class:///Test", 
        "java+class:///BeforeEach",
        "java+interface:///org/junit/jupiter/api/Test",
        "java+interface:///org/junit/jupiter/api/BeforeEach" ].map(testAttr => (
        {
          "verdict": "neutral",
          "pattern": [
            [ "method", "dependsOn", "@Test" ],
            [ "@Test", { "annotation": { "location": testAttr } }, "@Test" ]
          ]
        }
      )))
    }
  ]
}
