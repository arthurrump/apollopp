import fs from "fs";
import path from "path";
import dotenv from "dotenv";
import { createLanguageModel, createJsonTranslator, processRequests } from "typechat";
import { PatternResult } from "./Patterns.schema";

dotenv.config({ path: path.join(__dirname, "../.env") });

const model = createLanguageModel(process.env);
const schema = fs.readFileSync(path.join(__dirname, "./Patterns.schema.ts"), "utf8");
const translator = createJsonTranslator<PatternResult>(model, schema, "PatternResult");

translator.validator.stripNulls = false;
translator.createRequestPrompt = (request: string) => 
    `You are a service that translates assessment criteria for programming projects into code patterns encoded as JSON objects of type "${translator.validator.typeName}" according to the following TypeScript definitions:\n` +
    `\`\`\`\n${translator.validator.schema}\`\`\`\n` +
    `The following is a criterion:\n` +
    `"""\n${request}\n"""\n` +
    `The following is the criterion translated into up to 4 patterns as a JSON object with 2 spaces of indentation and no properties with the value undefined:\n`;

processRequests("> ", process.argv[2], async (request) => {
    const response = await translator.translate(request);
    if (!response.success) {
        console.log("Request failed: " + response.message);
        return;
    }

    const patternResult = response.data;
    if (!patternResult.success) {
        console.log(
            "Unable to generate a pattern: "
            + patternResult.reason
            + patternResult.message ? " (" + patternResult.message + ")" : ""
        );
        return;
    }

    console.log(JSON.stringify({ criterion: request, result: patternResult.patterns }, undefined, 2));
    console.log("---");
});
