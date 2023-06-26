module graphs::TypeGraph

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
    | \dependsOn()
    | \contains()
    ;

str labelTypeGraphEdge(TypeGraphEdge edge) {
    switch (edge) {
        case \extends(): return "extends";
        case \implements(): return "implements";
        case \dependsOn(): return "dependsOn";
        case \contains(): return "contains";
    }
    return "";
}

data TypeGraphNodeLabel
    = inProject(str scheme)
    | external(loc location)
    ;

TypeGraphNodeLabel labelTypeGraphNode(M3 model, loc \node) {
    if (isNameDeclaredInProject(\node, model)) {
        return inProject(\node.scheme);
        // TODO: Name class, based on course config
        // e.g. common pattern names, *Factory, *Listener etc for SS
        // e.g. *Particle, *ParticleSystem etc for AiC
    } else {
        return external(\node);
    }
}

alias TypeGraph = LGraph[loc, TypeGraphEdge];

TypeGraph createTypeGraph(M3 model, bool incudeCompilationUnitAsTypes = false) {
    // Extending classes and implementing interfaces
    g = { <from, \extends(), to> | <from, to> <- model.extends };
    g += { <from, \implements(), to> | <from, to> <- model.implements };

    // Type dependency when a type is used in another type (for a field, for a
    // variable within a method, anything really; also: dependencies are
    // transitive from inner classes, so the set of depencies of the containing
    // class is a superset of the dependencies of the inner class).
    g += { <fromType, \dependsOn(), to> | <from, to> <- model.typeDependency, fromType <- getContainingTypes(from, model) };
    if (incudeCompilationUnitAsTypes) {
        // Imports are part of the compilation unit and not the type. If we want
        // to include these as dependencies for the types inside that
        // compilation unit, we can add them here.
        g += { <fromType, \dependsOn(), to> | <from, to> <- model.typeDependency, fromType <- getCompilationUnitTypes(from, model) };
    }

    // Containment of types within other types, types within packages or packages within packages
    g += { <from, \contains(), to> | <from, to> <- model.containment, isType(from) || isPackage(from), isType(to) || isPackage(to) };

    return g;
}
