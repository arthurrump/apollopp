module graphs::TypeGraph

import IO;
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

data TypeGraphAnnotation
    = \nameClass(str nameClass)
    | \modifier(Modifier modifier)
    | \inProjectDecl(str scheme)
    | \externalDecl(loc location)
    ;

data TypeGraphEdge
    = \extends()
    | \implements()
    | \invokes()
    | \dependsOn()
    | \contains()
    | \annotated(TypeGraphAnnotation annotation)
    ;

alias TypeGraph = LGraph[loc, TypeGraphEdge];

alias Annotate = set[TypeGraphAnnotation] (loc);

set[TypeGraphAnnotation] annotateNone(loc _) {
    return {};
}

Annotate combine(set[Annotate] annotates) {
    return set[TypeGraphAnnotation] (loc \node) {
        return union({ a(\node) | a <- annotates });
    };
}

Annotate annotateInProjectDecls(M3 model) {
    return set[TypeGraphAnnotation] (loc \node) {
        if (isNameDeclaredInProject(\node, model)) {
            return { \inProjectDecl(\node.scheme) };
        };
        return {};
    };
}

Annotate annotateExternalDecls(M3 model) {
    return set[TypeGraphAnnotation] (loc \node) {
        if (!isNameDeclaredInProject(\node, model)) {
            return { \externalDecl(\node) };
        }
        return {};
    };
}

Annotate annotateModifiers(M3 model) {
    return set[TypeGraphAnnotation] (loc \node) {
        return { \modifier(m) | <\node, m> <- model.modifiers };
    };
}

Annotate annotateNameClass(rel[str, str] nameClasses, M3 model) {
    rel[loc, str] elementNames = model.names<qualifiedName, simpleName>;
    return set[TypeGraphAnnotation] (loc \node) {
        if (isNameDeclaredInProject(\node, model)) {
            return { \nameClass(c) | <namePart, c> <- nameClasses, name <- elementNames[\node], contains(name, namePart) };
        }
        return {};
    };
}

Annotate annotateDefaults(M3 model, rel[str, str] nameClasses) {
    return combine({ 
        annotateInProjectDecls(model), 
        annotateExternalDecls(model), 
        annotateModifiers(model), 
        annotateNameClass(nameClasses, model) 
    });
}

TypeGraph createTypeGraph(M3 model, Annotate annotate, bool incudeCompilationUnitAsTypes = false) {
    // Extending classes and implementing interfaces
    TypeGraph g = { <from, \extends(), to> | <from, to> <- model.extends };
    g += { <from, \implements(), to> | <from, to> <- model.implements };

    // All containment relations between M3 elements
    g += { <from, \contains(), to> | <from, to> <- model.containment };

    // Type dependency is the reference to a type by a certain element, but we
    // want to exclude the extends and implements relations, which we already
    // covered explicitly. Type dependencies are added transitively for all
    // types containing the type that has a dependency.
    rel[loc, loc] otherTypeDependency = model.typeDependency - model.extends - model.implements;
    g += { <fromAll, \dependsOn(), to> | <from, to> <- otherTypeDependency, fromAll <- getContainingElements(from, model) };

    // Imports are part of the compilation unit and not the type. If we want to
    // include these as dependencies for the types inside that compilation unit,
    // we can add them here.
    if (incudeCompilationUnitAsTypes) {
        g += { <fromType, \dependsOn(), to> | <from, to> <- model.typeDependency, fromType <- getCompilationUnitTypes(from, model) };
    }

    // Method invocations, from methods to methods,
    g += { <from, \invokes(), to> | <from, to> <- model.methodInvocation };
    // and copy to all containing types, to still cover general invocation
    // structure even if methods are split up differently.
    g += { <fromAll, \invokes(), to> | <from, to> <- model.methodInvocation, fromAll <- getContainingTypes(from, model) };

    // Annotate the graph using the annotation function
    g += { <\node, \annotated(a), \node> | \node <- (g<from> + g<to>), a <- annotate(\node) };

    return g;
}
