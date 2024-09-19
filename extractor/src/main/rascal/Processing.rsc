module Processing

import IO;
import List;
import Set;
import String;
import lang::java::m3::Core;
import graphs::TypeGraph;
import graphs::Convert;
import util::FileSystem;
import util::SystemAPI;
import lang::json::IO;

list[loc] processingClassPath = 
    [ toLocation(getSystemEnvironment()["PROCESSING_CORELIB"]) ]
    + toList(find(toLocation(getSystemEnvironment()["PROCESSING_LIBRARIES"]), "jar"));

rel[&T, &T] relid(set[&T] s) = { <x, x> | x <- s };

M3 createModel(loc project) {
    return createM3FromDirectory(project, javaVersion = "1.8", classPath = processingClassPath);
}

TypeGraph[loc] createTypeGraph(M3 model) {
    return graphs::TypeGraph::createTypeGraph(model, annotateDefaults(model, relid({ "Particle", "System", "Flock", "Boid" })));
}

list[loc] getProjects(loc dir) {
    println(dir);
    return [ proj + "source" | proj <- dir.ls ];
}

list[M3] getProjectModels(loc dir) {
    result = for (proj <- getProjects(dir)) {
        append createModel(proj);
    }
    return result;
}

list[TypeGraph[loc]] getProjectTypeGraphs(loc dir) {
    result = for (model <- getProjectModels(dir)) {
        append createTypeGraph(model);
    }
    return result;
}

void writeProjectTypeGraphs(loc dir) {
    for (proj <- getProjects(dir)) {
        model = createModel(proj);
        typeGraph = createTypeGraph(model);
        writeJSON(proj + "graph" + "typegraph.json", typeGraph);
    }
}

int main(list[str] params) {
    if (size(params) != 1) {
        println("USAGE: Processing \<path to projects dir\>");
        return 1;
    }

    writeProjectTypeGraphs(toLocation(params[0]));
    return 0;
}
