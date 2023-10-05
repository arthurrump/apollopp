export type PatternResult = 
| SuccessResult 
| FailureResult;

export type FailureResult = {
  success: false;
  reason:
    // The criterion is not assessable based on source code
    | "not_code" 
    // The criterion is not assessable based on information in the type graph
    | "not_typegraph" 
    // Other reason, explained in `message`
    | "other";
  message?: string; 
}

export type SuccessResult = {
  success: true;
  patterns: Pattern[];
}

export type Pattern = {
  verdict: "positive" | "negative" | "neutral";
  graph: TypeGraph;
}

export type Scheme = "class" | "interface" | "field" | "method" | "constructor" | "parameter" | "variable" | "array" | "primitiveType" | "compilationUnit";

/// A location in source code, like:
/// { scheme = "class"; path = "java/lang/Object" }
/// { scheme = "method"; path = "myPackage/Program/main(java.lang.String[])" }
/// { scheme = "primitiveType"; path = "int" }
/// { scheme = "constructor"; path = "EndAssignment/Ball/Ball(processing.core.PVector)" }
export type Location = {
  scheme: Scheme;
  path: string;
};

export type TypeGraph = {
  nodes: TypeGraphNode[];
  edges: TypeGraphEdge[];
}

export type TypeGraphNode = {
  id: number;
  labels: TypeGraphNodeLabel[];
}

export type TypeGraphNodeLabel =
  | NameClassLabel
  | ModifierLabel
  | InProjectDeclarationLabel
  | ExternalDeclarationLabel;

/// The name of this type is in a certain class of names, eg. the type
/// `BeanFactory` in in the `Factory` name class, but `Bean` is not in any name class
export type NameClassLabel = {
  labelType: "nameClass";
  nameClass: string;
}

/// The node has a modifier keyword, like `public` or `static`
export type ModifierLabel = {
  labelType: "modifier";
  modifier: "private" | "public" | "protected" | "static" | "final" | "synchronized" | "transient" | "abstract" | "native" | "volatile" | "strictfp" | "default";
}

/// The node is declared in the project and has a type like `class`, `method` or `field`
export type InProjectDeclarationLabel = {
  labelType: "inProjectDeclaration";
  declarationType: Scheme;
}

/// The node is declared in an external library or standard library
export type ExternalDeclarationLabel = {
  labelType: "externalDeclaration";
  qualifiedName: Location;
}

export type TypeGraphEdge = {
  fromId: number;
  toId: number;
  label: "extends" | "implements" | "invokes" | "dependsOn" | "contains";
}
