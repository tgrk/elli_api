# ellija

NOTE: this is work in progress!!

Build Elli based API application with ease.

Motivation for this set of helpers is to allow a bit more comfortable way
of building HTTP API applications (HTTP JSON API or REST API) using Erlang.

# Features

* routing
* middleware
* helpers
  * request
  * response
  * data helpers

# Build

```bash
$ rebar3 compile
```

# Usage

```erlang

```

# TODO

* correct headers:
  * response - Content-Type: application/vnd.api+json
  * fail if client is not compatible?
* wrapper for responses:
  * data - id, type and attributes (that reflect sparse fields)
  * error object (pointers)
  * meta? (eg for total rows in case of pagination)

* handle from url:
  * sparse fields - ?fields[articles]=title,body,author
  * sort - ?sort=-title
  * pagination - ?page[number]=3&page[size]=1
  * filter - ?filter[post]=1

* angular client - https://github.com/jakubrohleder/angular-jsonapi
* elixir client - https://github.com/Decisiv/json_api_client

For usage please see examples.
