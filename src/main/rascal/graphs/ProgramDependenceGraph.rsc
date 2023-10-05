module graphs::ProgramDependenceGraph

import graph::DataStructures;
import graphs::LLGraph;
import lang::java::m3::Core;
import lang::java::m3::AST;

data PdgEdgeLabel
    = \controlDepends()
    | \dataDepends()
    ;

alias ProgramDependenceGraph[&VertexId] = LLGraph[&VertexId, void, PdgEdgeLabel];

ProgramDependenceGraph[loc] createPdg(ProgramDependences pdgs) {
    return union({ createPdg(method, pdg) | <method, pdg> <- pdgs });
}

ProgramDependenceGraph[loc] createPdg(MethodData method, ProgramDependence pdg) {
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
