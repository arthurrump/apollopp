module graphs::TypeGraph

import Set;
import String;

import analysis::graphs::LabeledGraph;
import lang::java::m3::Core;
import lang::java::m3::AST;

private bool isNameDeclaredInProject(loc name, M3 model) {
    return name in model.declarations<name>;
}

private set[loc] getContainingTypes(loc element, M3 model) {
    if (isType(element)) {
        return { element };
    }

    parents = model.containment<to, from>[element];
    return { ct | p <- parents, ct <- getContainingTypes(p, model) };
}

private set[loc] getContainingElements(loc element, M3 model) {
    return { element } + union({ getContainingElements(p, model) | p <- (model.containment<to, from>[element]) });
}

private set[loc] getCompilationUnitTypes(loc element, M3 model) {
    if (isCompilationUnit(element)) {
        return { t | t <- model.containment[element], isType(t) };
    } else {
        return {};
    }
}

data TypeGraphEdge
    = \extends()
    | \implements()
    | \invokes()
    | \dependsOn()
    | \contains()
    | \annotates()
    | \modifies()
    ;

data TypeGraphAnnotation
    = nameClass(str)
    ;

data TypeGraphNode
    = \element(loc)
    | \modifier(Modifier)
    | \annotation(TypeGraphAnnotation)
    ;

data TypeGraphNodeLabel
    = \projectElementLabel(str scheme)
    | \externalElementLabel(loc location)
    | \modifierLabel(Modifier)
    | \annotationLabel(TypeGraphAnnotation)
    ;

TypeGraphNodeLabel labelTypeGraphNode(M3 model, TypeGraphNode \node) {
    switch(\node) {
        case \element(loc element): {
            if (isNameDeclaredInProject(element, model)) {
                return \projectElementLabel(element.scheme);
            } else {
                return \externalElementLabel(element);
            }
        }
        case \modifier(Modifier modifier):
            return \modifierLabel(modifier);
        case \annotation(TypeGraphAnnotation annotation):
            return \annotationLabel(annotation);
    }
    throw "Unreachable.";
}

alias TypeGraph = LGraph[TypeGraphNode, TypeGraphEdge];

alias Annotate = set[TypeGraphAnnotation] (TypeGraphNode);

set[TypeGraphAnnotation] noAnnotations(TypeGraphNode _) {
    return {};
}

Annotate combineAnnotate(Annotate a, Annotate b) {
    return set[TypeGraphAnnotation] (TypeGraphNode \node) {
        return a(\node) + b(\node);
    };
}

Annotate annotateNameClass(rel[str, str] nameClasses, M3 model) {
    rel[loc, str] elementNames = model.names<qualifiedName, simpleName>;
    return set[TypeGraphAnnotation] (TypeGraphNode \node) {
        switch(\node) {
            case \element(loc element): {
                if (isNameDeclaredInProject(element, model)) {
                    return { \nameClass(c) | <namePart, c> <- nameClasses, name <- elementNames[element], contains(name, namePart) };
                };
            }
        }
        return {};
    };
}

TypeGraph createTypeGraph(M3 model, Annotate annotate = noAnnotations, bool incudeCompilationUnitAsTypes = false) {
    // Extending classes and implementing interfaces
    TypeGraph g = { <\element(from), \extends(), \element(to)> | <from, to> <- model.extends };
    g += { <\element(from), \implements(), \element(to)> | <from, to> <- model.implements };

    // All containment relations between M3 elements
    g += { <\element(from), \contains(), \element(to)> | <from, to> <- model.containment };

    // Type dependency is the reference to a type by a certain element, but we
    // want to exclude the extends and implements relations, which we already
    // covered explicitly. Type dependencies are added transitively for all
    // types containing the type that has a dependency.
    rel[loc, loc] otherTypeDependency = model.typeDependency - model.extends - model.implements;
    g += { <\element(fromAll), \dependsOn(), \element(to)> | <from, to> <- otherTypeDependency, fromAll <- getContainingElements(from, model) };

    // Imports are part of the compilation unit and not the type. If we want to
    // include these as dependencies for the types inside that compilation unit,
    // we can add them here.
    if (incudeCompilationUnitAsTypes) {
        g += { <\element(fromType), \dependsOn(), \element(to)> | <from, to> <- model.typeDependency, fromType <- getCompilationUnitTypes(from, model) };
    }

    // Method invocations, from methods to methods,
    g += { <\element(from), \invokes(), \element(to)> | <from, to> <- model.methodInvocation };
    // and copy to all containing types, to still cover general invocation
    // structure even if methods are split up differently.
    g += { <\element(fromAll), \invokes(), \element(to)> | <from, to> <- model.methodInvocation, fromAll <- getContainingTypes(from, model) };

    // Modifiers
    g += { <\modifier(m), \modifies(), \element(d)> | <d, m> <- model.modifiers };

    // Annotate the graph using the annotation function
    g += { <\annotation(a), \annotates(), \node> | \node <- (g<from> + g<to>), a <- annotate(\node) };

    return g;
}
