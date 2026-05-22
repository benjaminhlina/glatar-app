# Make functions

These functions make information that is to be used during the login
processes

## Usage

``` r
make_pg_username(email)

make_temp_password(email)
```

## Arguments

- email:

  a valid email

## Details

`make_pg_username()` returns a username that matches with the database
user name.

`make_temp_password()` returns a temporay password
