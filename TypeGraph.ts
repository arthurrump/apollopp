export type Result = 
| SuccessResult 
| FailureResult;

export type FailureResult = {
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
  patterns: TypeGraph[];
}

/// A location in source code, in the following notation:
/// |java+class:///java/lang/Object|
/// |java+method:///myPackage/Program/main(java.lang.String%5B%5D)|
/// |java+primitiveType:///int|
/// |java+constructor:///EndAssignment/Ball/Ball(processing.core.PVector)|
export type Location = string;

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
  modifier: string;
}

/// The node is declared in the project and has a type like `class`, `method` or `field`
export type InProjectDeclarationLabel = {
  labelType: "inProjectDeclaration";
  declarationType: string;
}

/// The node is declared in an external library or standard library
export type ExternalDeclarationLabel = {
  labelType: "externalDeclaration";
  qualifiedName: Location;
}

export type TypeGraphEdge = {
  fromId: number;
  toId: number;
  label: | "extends" | "implements" | "invokes" | "dependsOn" | "contains";
}
