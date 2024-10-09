module graphs::ProgramDependenceGraph

import List;
import Set;
import IO;

import analysis::graphs::LabeledGraph;
import lang::java::m3::Core;
import lang::java::m3::AST;

import graph::control::PDT;
import graph::control::dependence::CDG;
import graph::control::flow::CFG;
import graph::\data::DDG;
import graph::program::PDG;
import graph::DataStructures;

data PdgEdgeLabel
    = \controlDepends()
    | \dataDepends()
    ;

alias ProgramDependenceGraph[&VertexId] = LGraph[&VertexId, void, PdgEdgeLabel];

ProgramDependenceGraph[node] createPdg(ProgramDependences pdgs) {
    return union({ createPdg(method, pdgs[method]) | method <- pdgs });
}

ProgramDependenceGraph[node] createPdg(MethodData method, ProgramDependence pdg) {
    // TODO: Get usable locations from the nodes returned by nodeEnvironment
    // Some have a decl, others do not. Some nodes represent e.g. the return
    // value of a method, so do not have a specific decl location. Most do have
    // a src location, but also not all do... 
    
    // Locations are useful to link back to the typegraph, so for each "node" we
    // want to know what is referred to, e.g. which method is called and which
    // variables are referenced.

    // However: the patterns work on a single graph, so we either bundle the PDG
    // into the TypeGraph, or the references don't really matter. In which case
    // we can just use the node? We loose some consistency, but that is not
    // really an issue, is it?

    cdg = {
        <method.nodeEnvironment[from], \controlDepends(), method.nodeEnvironment[to]>
        | <from, to> <- pdg.controlDependence
    };
    ddg = {
        <method.nodeEnvironment[from], \dataDepends(), method.nodeEnvironment[to]>
        | <from, to> <- pdg.dataDependence
    };
    return cdg + ddg;
}

rel[MethodData, ProgramDependence] createPdg(Declaration method, M3 model) {
    println("Creating PDG for <method.decl>");
    GeneratedData cfgData;
    try {
        cfgData = createCFG(model, method);
    } catch err : {
        println("Failed to create CFG.");
        return {};
    }
    switch (cfgData) {
        case EmptyGD(): {
            println("Got empty CFG.");
            return {};
        }
        case GeneratedData(methodData, controlFlow): {
            println("Got CFG.");
            PostDominator postDominator = createPDT(methodData, controlFlow);
            println("Got PDT.");
            ControlDependence controlDep = createCDG(methodData, controlFlow, postDominator);
            println("Got CDG.");
            DataDependence dataDep = createDDG(methodData, controlFlow);
            println("Got DDG");
            ProgramDependence pdg = createPDG(controlDep, dataDep);
            println("Got PDG.");
            return { <methodData, pdg> };
        }
    }
}

ProgramDependenceGraph[node] createPdg(set[Declaration] asts, M3 model) {
    set[Declaration] methods = { m | ast <- asts, /m:\method(_, _, _, _, _) := ast };
    set[Declaration] constructors = { c | ast <- asts, /c:\constructor(_, _, _, _) := ast };
    rel[MethodData, ProgramDependence] pdgs = union({ createPdg(d, model) | d <- methods + constructors });
    return createPdg(toMap(pdgs));
}
