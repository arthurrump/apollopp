module Java

import IO;
import Set;
import lang::java::m3::Core;
import lang::json::IO;
import graphs::TypeGraph;
import util::FileSystem;

M3 createModel(loc project) {
    list[loc] projectClassPath = toList(find(project, "jar"));
    return createM3FromDirectory(project, javaVersion = "1.8", classPath = projectClassPath);
}

rel[&T, &T] relid(set[&T] s) = { <x, x> | x <- s };

TypeGraph[loc] createTypeGraph(M3 model) {
    return graphs::TypeGraph::createTypeGraph(model, annotateDefaults(model, relid({ "Listener", "View", "Controller" })));
}

void writeProjectTypeGraphs(loc dir) {
    for (proj <- dir.ls) {
        println("Creating TypeGraph for " + proj.uri);
        model = createModel(proj);
        typeGraph = createTypeGraph(model);
        writeJSON(proj + "source" + "graph" + "typegraph.json", typeGraph);
    }
}
