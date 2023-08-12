module Main

import IO;
import lang::java::m3::Core;
import lang::java::m3::AST;
import extractors::Project;
import graph::DataStructures;
import graph::factory::GraphFactory;
import vis::Graphs;
import Set;

Declaration getDeclarationFor(loc location, set[Declaration] ast) {
    set[Declaration] result = { d | /Declaration d := ast, d.decl == location };
    if ({ oneResult } := result) {
        return oneResult;
    }
    throw "Unexpected number of ASTs returned for <(location.uri)>";
}

void main(loc project) {
    M3 model = createM3(project);
    set[Declaration] ast = createProjectAST(project, true);
    set[ProgramDependences] pdg = { 
        createProgramDependences(m, getDeclarationFor(m, ast), model, Project()) 
        | m <- methods(model) 
    };
    iprintln(pdg);
}
