# Haskell Json Patch

A haskell implementation of [json patch](http://jsonpatch.com/) and a json beautifier as a CLI tool.

## Contributors
- Sepehr Noorafshan
- Lester Lin
- Hursh Arora

## How to run

To run our suite of automated tests, run `stack test`.

In order to run our project, first run `stack build`. Doing so will compile the project and generate an executable that can be found in the `.stack-work` directory.

You can then run the executable using `stack run`. Doing so will show you the guide to our CLI. To use the CLI this way, you will need two hyphens after `stack run` followed by the desired options. For example:

`stack run -- -i demo/in.json -o demo/out.json -p demo/patch1.json -b`

`stack run -- -i demo/in.json -o demo/out.json -p demo/patch2.json -b`

`stack run -- -i demo/in.json -o demo/out.json -p demo/patch3.json -b`

The `demo/out.json` file will have the result of applying the patches defined in the specfied patch file to the `demo/in.json` file's data.

We invisioned the tool to be used by appending path to the generated executable to your `$PATH` variable (or copying it to your `/bin` directory). This makes it easy to use the tool in any directory as well as being able to rename the binary and thus naming the command that would execute the binary.

