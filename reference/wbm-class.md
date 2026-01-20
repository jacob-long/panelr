# Within-Between Model (`wbm`) class

Models fit using
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) return values
of this class, which inherits from
[`merMod-class`](https://rdrr.io/pkg/lme4/man/merMod-class.html).

## Slots

- `call_info`:

  A list of metadata about the arguments used.

- `call`:

  The actual function call.

- `summ`:

  The
  [`jtools::summ()`](https://jtools.jacob-long.com/reference/summ.html)
  object returned from calling it on the `merMod` object.

- `summ_atts`:

  The attributes of the `summ` object.

- `orig_data`:

  The data provided to the `data` argument in the function call.
