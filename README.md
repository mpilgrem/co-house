# co-house

Haskell bindings to the UK Companies House's
[APIs](https://developer.company-information.service.gov.uk/) and a
corresponding command line application.

This package has no connection with the UK's Companies House or its affilates.

Built with [servant](http://hackage.haskell.org/package/servant).

## How to install the application, with the Haskell Tool Stack

1. If the Haskell Tool Stack tool is not installed, install that tool following
   the tool's
   [installation instructions](https://docs.haskellstack.org/en/stable/README/).

2. Clone (that is, make a local copy of) this repository.

3. Change directory to the root folder of the repository (`co-house`).

4. Use the `stack` command `stack install` to build and install the executable,
   which is named `co-house`.

5. Use the command `co-house --help` to obtain help on how to use the command.

A Companies House API key is required to use the `co-house` executable, either
provided via the environment variable CO_HOUSE_API_KEY or on the command line.

An API key can be obtained by registering a user account with Companies House,
creating an 'application', and creating a new key for that application. See the
Companies House guide to
[getting started](https://developer.company-information.service.gov.uk/get-started).
