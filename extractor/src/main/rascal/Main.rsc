module Main

import IO;
import Map;
import Set;
import lang::java::m3::Core;
import lang::java::m3::AST;
import extractors::Project;
import graph::DataStructures;
import graph::factory::GraphFactory;
import vis::Graphs;

Declaration getDeclarationFor(loc location, set[Declaration] ast) {
    set[Declaration] result = { d | /Declaration d := ast, d.decl == location };
    if ({ oneResult } := result) {
        return oneResult;
    }
    throw "Unexpected number of ASTs returned for <(location.uri)>";
}

ProgramDependences createProgramDependences(loc project) {
    M3 model = createM3(project);
    set[Declaration] ast = createProjectAST(project, true);
    set[ProgramDependences] pdgs = { 
        createProgramDependences(m, getDeclarationFor(m, ast), model, Project()) 
        | m <- methods(model) 
    };
    return toMapUnique(union({ toRel(pdg) | pdg <- pdgs }));
}

void main(loc project) {
    
}
