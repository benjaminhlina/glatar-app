# Authorization Server

Provides the authorization server for the login modal that displays on
two locked tabs, uploading and user specific data.

Provides the logout modal to ask the user if they would like to logout.

## Usage

``` r
tab_auth_server(
  input,
  output,
  session,
  credentials,
  sidebar_id = "tabs",
  off = FALSE
)

logout_server(id, parent_session)
```

## Arguments

- input:

  a shiny input object.

- output:

  a shiny output object.

- session:

  a shiny session.

- credentials:

  a `data.frame` containing login credentials

- sidebar_id:

  a `vector` containing the sidebar ids to allow the login to display.

- off:

  a `logical` value that turns on and off the authorization server.
  Default is `FALSE`. This switch is for development only.

- id:

  the shiny namespace id name (i.e., `"logout"`).

- parent_session:

  a shiny server session.

## Value

a shiny modal

a modal that asks the user if they really want to logout. This prevents
accidental logouts.
