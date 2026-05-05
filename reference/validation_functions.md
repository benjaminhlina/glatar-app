# Validator funtions

These functions work within their own enviornment. They take in a
`data.frame` and a set of `rules` either supplied by `rule_*` functions
or within the function itself to check. Once both the `data.frame` and
`rules` are set these functions use `confront()` from `{validate}` to
confront the `data.frame` with rules.

## Usage

``` r
the_golden_lance(df)

validate_tbl_sources(df)

validate_tbl_submission(df)
```

## Arguments

- df:

  a `data.frame` that requires validation.

## Value

returns a `validate` object.

returns a `validate` object.

returns a `validate` object.

## Details

`the_golden_lance()` is named after Galtar's most prized possesion, his
golden lance, which he uses to fight evil. This function validates
incoming data to be uploaded to `tbl_samples`. Let `the_golden_lance()`
have its say.

`validate_tbl_sources()` validates incoming data to be uploaded to
`tbl_sources`.

`validate_tbl_submission()` validates incoming data to be uploaded to
`tbl_submission`.
