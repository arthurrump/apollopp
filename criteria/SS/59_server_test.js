var criterion = {
  "criterion": "The server is tested with unit tests that check whether the protocol is handled correctly.",
  "patterns": [ "java+class:///Test", "java+interface:///org/junit/jupiter/api/Test" ].map(test => (
    {
      "verdict": "positive",
      "pattern": [
        [ "server", { "annotation": { "scheme": "java+class" } }, "server" ],
        [ "ServerSocket", { "annotation": { "location": "java+class:///java/net/ServerSocket" } }, "ServerSocket" ],
        [ "server", "dependsOn", "ServerSocket" ],
        [ "serverMethod", { "annotation": { "scheme": "java+method" } }, "serverMethod" ],
        [ "server", "contains", "serverMethod" ],
        [ "testMethod", { "annotation": { "scheme": "java+method" } }, "testMethod" ],
        [ "testMethod", "dependsOn", "@Test"],
        [ "@Test", { "annotation": { "location": test } }, "@Test" ],
        [ "testMethod", "invokes", "serverMethod" ]
      ]
    })
  )
}