# Suggest: LLM suggestions for patterns

This tool takes in textual assessment criteria, for example from rubrics or requirements specified in the project description, and asks an LLM for code patterns that could be used to check whether those criteria are met.

## Usage

To install dependencies and compile the TypeScript code:

```sh
npm install
npm run build
```

Create a `.env` file with the appropriate configuration values for your LLM. For example, to use OpenAI's ChatGPT:

```env
OPENAI_API_KEY=...
OPENAI_ENDPOINT=https://api.openai.com/v1/chat/completions
OPENAI_MODEL=gpt-3.5-turbo
```

See the [TypeChat documentation](https://microsoft.github.io/TypeChat/docs/examples/) for more details.

Run the tool with

```sh
node .
```

to start in interactive mode. This allows you to type in criteria and get results one by one. Alternatively, you can create a file with one criterion per line and run the tool in batch mode with

```sh
node . path/to/criteria.txt
```

## Technical details

The request to the LLM is of the following form:

> You are a service that translates assessment criteria for programming projects into code patterns encoded as JSON objects of type "PatternResult" according to the following TypeScript definitions:
>
> ```typescript
> export type PatternResult = 
> | SuccessResult 
> | FailureResult;
> 
> export type FailureResult = {
>   success: false;
>   reason:
>     // The criterion is not assessable based on source code
>     | "not_code" 
>     // The criterion is not assessable based on information in the type graph
>     | "not_typegraph" 
>     // Other reason, explained in `message`
>     | "other";
>   message?: string; 
> }
> 
> export type SuccessResult = {
>   success: true;
>   patterns: Pattern[];
> }
> 
> export type Pattern = {
>   verdict: "positive" | "negative" | "neutral";
>   graph: TypeGraph;
> }
> 
> // ... TypeGraph definition
> ```
>
> The following is a criterion:
>
> """
> All fields except constants are private.
>
> """
>
> The following is the criterion translated into up to 4 patterns as a JSON object with 2 spaces of indentation and no properties with the value undefined:
>
> 

The TypeScript definitions include the full `TypeGraph` type definition, which can be found in the [source file](src/Patterns.schema.ts). The results are parsed and checked against the type by the [TypeChat](https://microsoft.github.io/TypeChat) library, with follow-up queries if the LLM does not respond with a validly typed JSON result.

The pattern includes an explicit failure case, to give the LLM an easy way out if a criterion is not expressible with a `TypeGraph` pattern.