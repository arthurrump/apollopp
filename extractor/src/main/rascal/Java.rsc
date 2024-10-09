module Java

import IO;
import List;
import Set;
import String;
import lang::java::m3::AST;
import lang::java::m3::Core;
import lang::json::IO;
import graphs::TypeGraph;
import graphs::ProgramDependenceGraph;
import util::FileSystem;

M3 createModel(loc project) {
    list[loc] projectClassPath = toList(find(project, "jar"));
    return createM3FromDirectory(project, javaVersion = "1.8", classPath = projectClassPath);
}

set[Declaration] createAsts(loc project) {
    return createAstsFromDirectory(project, true, javaVersion = "1.8");
}

rel[&T, &T] relid(set[&T] s) = { <x, x> | x <- s };

TypeGraph[loc] createTypeGraph(M3 model) {
    return graphs::TypeGraph::createTypeGraph(model, annotateDefaults(model, relid({ "Listener", "View", "Controller" })));
}

ProgramDependenceGraph[loc] createProgramDependenceGraph(set[Declaration] asts, M3 model) {
    return graphs::ProgramDependenceGraph::createPdg(asts, model);
}

void writeProjectGraphs(loc dir) {
    for (proj <- dir.ls) {
        println("Creating graphs for " + proj.uri);
        model = createModel(proj);
        typeGraph = createTypeGraph(model);
        writeJSON(proj + "source" + "graph" + "typegraph.json", typeGraph);
        asts = createAsts(proj);
        pdg = createProgramDependenceGraph(asts, model);
        writeJSON(proj + "source" + "graph" + "pdg.json", pdg);
    }
}

int main(list[str] params) {
    if (size(params) != 1) {
        println("USAGE: Java \<path to projects dir\>");
        return 1;
    }

    writeProjectGraphs(toLocation(params[0]));
    return 0;
}
