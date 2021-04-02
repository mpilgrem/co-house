# co-house

Haskell bindings to the UK Companies House's
[APIs](https://developer.company-information.service.gov.uk/) and a
corresponding command line application.

This package has no connection with the UK's Companies House or its affilates.

Built with [servant](http://hackage.haskell.org/package/servant).

## How to install the `co-house` application, with the Haskell Tool Stack

1. If the Haskell Tool Stack tool is not installed, install that tool following
   the tool's
   [installation instructions](https://docs.haskellstack.org/en/stable/README/).

2. Clone (that is, make a local copy of) this repository.

3. Change directory to the root folder of the repository (that is, `co-house`).

4. Use the `stack` command `stack install` to build (compile from the source code) and install the executable,
   which is named `co-house`. (The first time a `stack` command is run, it will download and install what is needed to compile source code.)

5. Use the command `co-house --help` to obtain help on how to use the command.

A Companies House API key is required to use the `co-house` executable, either
provided via the environment variable `CO_HOUSE_API_KEY` or on the command line.

An API key can be obtained by registering a user account with Companies House,
creating an 'application', and creating a new key for that application. See the
Companies House guide to
[getting started](https://developer.company-information.service.gov.uk/get-started).

## Developer tools

The Companies House API makes use of enumerations, some of which (such as `description`) have a large number of members. The flag `tools` (`false` by default) enables the building of an executable, named `tool-description`, that generates the Haskell source for module `Web.CoHouse.Types.Description`.
